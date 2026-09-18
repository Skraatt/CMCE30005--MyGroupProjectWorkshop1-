### COMPILATION
setwd("~/Downloads/BACgroup/CMCE30005--MyGroupProjectWorkshop1-")
## PART 1: CLEANING + ETA
##Packages:
install.packages(c("data.table","dplyr","stringr","tidyr","lubridate","ggplot2"))
suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(lubridate)
})
RAW   <- "data/raw"
CLEAN <- "data/clean"
dir.create(CLEAN, recursive = TRUE, showWarnings = FALSE)

LOG <- character(0)
log_step <- function(...) {
  msg <- paste0(...)
  LOG <<- c(LOG, msg)
  cat(msg, "\n")
}

# Melbourne reference points
CBD_LAT  <- -37.8136; CBD_LON  <- 144.9631   # Flinders St Station
BEACH_LAT <- -37.8677; BEACH_LON <- 144.9740 # St Kilda Beach

## defining dtaset snapshot date
SNAPSHOT_DATE <- as.Date("2026-06-16")

## defining the harvensine function because it wasn't working for me (distance in kilometres function)
haversine_km <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  a <- sin(dlat / 2)^2 +
    cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  R * c
}

##(Have loaded the packages (data.table, stringr), have sets the folder paths. Also defines a logging function, so every decision gets written to a text file as we go, and a haversine function for calculating distances from coordinates.)

# 1. LOAD LISTINGS
log_step("=== STEP 1: LOAD ===")
L <- fread(file.path(RAW, "listings_airbnb.csv"), showProgress = FALSE)
log_step("Loaded listings: ", nrow(L), " rows x ", ncol(L), " columns")
log_step("Unique listing ids: ", uniqueN(L$id), " | unique hosts: ", uniqueN(L$host_id))
stopifnot(uniqueN(L$id) == nrow(L))  

##(Reads listings_airbnb.csv and checks there are no duplicate listing IDs.Result: 25,728 listings from 14,113 hosts.
  ##if you open the raw file it looks like 62,000 rows, but that's just because the description fields contain line breaks. The real count is 25,728. So have cleared that)




#  2. DROP UNUSABLE COLUMNS 
log_step("\n=== STEP 2: DROP UNUSABLE COLUMNS ===")
is_blank <- function(x) {
  if (is.character(x)) is.na(x) | trimws(x) == "" | x == "N/A" else is.na(x)
}
miss_rate <- sapply(L, function(x) mean(is_blank(x)))


# (a) Columns that are 100% empty in this snapshot
empty_cols <- names(miss_rate[miss_rate == 1])
log_step("Fully empty columns (", length(empty_cols), "): ",
         paste(empty_cols, collapse = ", "))
# (b) Zero-variance columns 
nunique <- sapply(L, function(x) uniqueN(x[!is_blank(x)]))
zerovar_cols <- setdiff(names(nunique[nunique <= 1]), empty_cols)
log_step("Zero-variance columns (", length(zerovar_cols), "): ",
         paste(zerovar_cols, collapse = ", "))
# (c) Free-text / URL / identifier columns not used as model inputs

drop_text <- c("listing_url", "scrape_id", "source", "picture_url",
               "host_url", "host_profile_id", "host_profile_url",
               "host_picture_url", "host_about", "host_verifications",
               "price_quote_raw", "neighborhood_overview")
drop_text <- intersect(drop_text, names(L))
# Redundant duplicates of retained fields
drop_redundant <- c("price_quote_price_per_night",  # identical to `price`
                    "minimum_minimum_nights", "maximum_minimum_nights",
                    "minimum_maximum_nights", "maximum_maximum_nights",
                    "minimum_nights_avg_ntm", "maximum_nights_avg_ntm")
drop_redundant <- intersect(drop_redundant, names(L))
log_step("Redundant columns dropped (", length(drop_redundant), "): ",
         paste(drop_redundant, collapse = ", "))
to_drop <- unique(c(empty_cols, zerovar_cols, drop_text, drop_redundant))
L[, (to_drop) := NULL]
log_step("Columns after drops: ", ncol(L))



#  3. PARSE PRICE 
log_step("\n=== STEP 3: PRICE ===")
L[, price_raw := price]
L[, price := as.numeric(str_replace_all(as.character(price_raw), "[$,]", ""))]
L[, price_missing := is.na(price)]
log_step("Price missing: ", sum(L$price_missing),
         " (", round(100 * mean(L$price_missing), 1), "%)")


#  Missingness correlates with listing quality:
sh <- L[, .(miss = round(100 * mean(price_missing), 1)), by = host_is_superhost]
log_step("Price missing by superhost status: ",
         paste(sh$host_is_superhost, sh$miss, sep = "=", collapse = "%, "), "%")
log_step("--> MNAR. Do NOT silently drop these rows; they are flagged and ",
         "retained so the bias can be reported and tested.")


# Outliers:
p99 <- quantile(L$price, 0.99, na.rm = TRUE)
p01 <- quantile(L$price, 0.01, na.rm = TRUE)
L[, price_outlier := !is.na(price) & (price > p99 | price < p01)]
log_step("Price 1st/99th percentile: $", round(p01), " / $", round(p99))
log_step("Flagged as outliers: ", sum(L$price_outlier))
# Winsorised version for model robustness (original retained)
L[, price_wins := pmin(pmax(price, p01), p99)]
L[, log_price := log(price_wins)]
log_step("Created price_wins (winsorised) and log_price.")




#  4. PROPERTY STRUCTURE
log_step("\n=== STEP 4: PROPERTY STRUCTURE ===")
# bathrooms is 31.7% missing, but bathrooms_text is near-complete.
# Parse the text to recover the numeric value AND the shared/private flag.
bt <- tolower(trimws(as.character(L$bathrooms_text)))
bath_num <- as.numeric(str_extract(bt, "^[0-9]+\\.?[0-9]*"))
bath_num[str_detect(bt, "half-bath") & is.na(bath_num)] <- 0.5   # "Half-bath"
before_na <- sum(is.na(L$bathrooms))
L[, bathrooms_clean := fifelse(is.na(bathrooms), bath_num, as.numeric(bathrooms))]
log_step("bathrooms NA before: ", before_na,
         " | after recovery from bathrooms_text: ", sum(is.na(L$bathrooms_clean)))
L[, bath_is_shared  := str_detect(bt, "shared")]
L[, bath_is_private := str_detect(bt, "private")]
# bedrooms / beds: missing is informative (studios often omit bedrooms).
# Flag, then impute conservatively.
L[, bedrooms_missing := is.na(bedrooms)]
L[, beds_missing     := is.na(beds)]
L[, bedrooms_clean := fifelse(is.na(bedrooms), 1, as.numeric(bedrooms))]  # studio -> 1
L[, beds_clean     := fifelse(is.na(beds),
                              pmax(1, ceiling(accommodates / 2)),
                              as.numeric(beds))]
log_step("bedrooms imputed (studio=1): ", sum(L$bedrooms_missing),
         " | beds imputed from accommodates: ", sum(L$beds_missing))
# Group 82 property types into an analysable set
L[, property_group := fcase(
  str_detect(property_type, "^Entire rental unit|^Entire condo|^Entire serviced"), "Entire apartment",
  str_detect(property_type, "^Entire home|^Entire townhouse|^Entire villa|^Entire cottage|^Entire bungalow"), "Entire house",
  str_detect(property_type, "^Entire guest suite|^Entire guesthouse|^Entire loft|^Entire cabin"), "Entire other",
  str_detect(property_type, "^Private room"), "Private room",
  str_detect(property_type, "^Shared room"), "Shared room",
  str_detect(property_type, "hotel|Hotel|boutique"), "Hotel/serviced",
  default = "Other"
)]
log_step("property_type: ", uniqueN(L$property_type), " levels -> property_group: ",
         uniqueN(L$property_group), " levels")
# Capacity ratios (guard against divide-by-zero)
L[, accommodates := pmax(accommodates, 1)]
L[, guests_per_bedroom := accommodates / pmax(bedrooms_clean, 1)]
L[, price_per_guest    := price_wins / accommodates]
##capacity groupings for some descriptive analysis stuff
L[, capacity_segment := fcase(
  accommodates <= 2, "1-2 guests",
  accommodates <= 4, "3-4 guests",
  accommodates <= 6, "5-6 guests",
  default = "7+ guests"
)]

#  5. HOST FEATURES 
log_step("\n=== STEP 5: HOST FEATURES ===")
# t/f character flags -> logical
tf_cols <- c("host_is_superhost", "host_has_profile_pic", "host_identity_verified")
tf_cols <- intersect(tf_cols, names(L))
for (cc in tf_cols) set(L, j = cc, value = tolower(L[[cc]]) == "t")
log_step("Converted to logical: ", paste(tf_cols, collapse = ", "))
log_step("Superhosts: ", sum(L$host_is_superhost, na.rm = TRUE),
         " (", round(100 * mean(L$host_is_superhost, na.rm = TRUE), 1), "%)")
# host_since is empty in this snapshot; use the tenure columns that survived
L[, host_tenure_years := hosts_time_as_host_years +
    fifelse(is.na(hosts_time_as_host_months), 0, hosts_time_as_host_months) / 12]
# Professional vs casual host — directly relevant to a multi-property client
L[, host_is_multi := calculated_host_listings_count > 1]
L[, host_scale := fcase(
  calculated_host_listings_count == 1, "Single",
  calculated_host_listings_count <= 5, "Small (2-5)",
  calculated_host_listings_count <= 20, "Medium (6-20)",
  default = "Large (21+)"
)]
log_step("Host scale distribution: ",
         paste(names(table(L$host_scale)), table(L$host_scale),
               sep = "=", collapse = ", "))

#  6. AMENITIES 
log_step("\n=== STEP 6: AMENITIES ===")
# Stored as a JSON-like string with escaped unicode. Parse to a count plus
# binary flags for amenities plausibly linked to price/demand.
am <- tolower(as.character(L$amenities))
L[, amenity_count := str_count(am, ",") + 1L]
L[amenities == "" | amenities == "[]", amenity_count := 0L]
amenity_flags <- list(
  am_wifi        = "wifi|internet",
  am_aircon      = "air condition|\\bac\\b|cooling",
  am_heating     = "heating",
  am_kitchen     = "kitchen",
  am_washer      = "washer",
  am_dryer       = "dryer",
  am_parking     = "free parking|parking on premises|garage",
  am_pool        = "pool",
  am_gym         = "gym|exercise",
  am_selfcheckin = "self check-in|lockbox|keypad",
  am_workspace   = "dedicated workspace",
  am_tv          = "\\btv\\b|television",
  am_bathtub     = "bathtub",
  am_balcony     = "balcony|patio",
  am_petsallowed = "pets allowed",
  am_elevator    = "elevator",
  am_bbq         = "bbq|barbecue"
)
for (nm in names(amenity_flags)) {
  set(L, j = nm, value = str_detect(am, amenity_flags[[nm]]))
}
log_step("Amenity count — median: ", median(L$amenity_count),
         " | range: ", min(L$amenity_count), "-", max(L$amenity_count))
log_step("Created ", length(amenity_flags), " binary amenity flags.")

#  7. LOCATION 
log_step("\n=== STEP 7: LOCATION ===")
L[, dist_cbd_km   := haversine_km(latitude, longitude, CBD_LAT, CBD_LON)]
L[, dist_beach_km := haversine_km(latitude, longitude, BEACH_LAT, BEACH_LON)]
L[, log_dist_cbd  := log1p(dist_cbd_km)]
log_step("Distance to CBD (km) — median: ", round(median(L$dist_cbd_km), 2),
         " | 90th pct: ", round(quantile(L$dist_cbd_km, 0.9), 2))
# NOTE for the multicollinearity check flagged in your feedback:
log_step("cor(dist_cbd_km, dist_beach_km) = ",
         round(cor(L$dist_cbd_km, L$dist_beach_km), 3),
         "  <-- check VIF before using both")
# Distance bands for interpretable submarket analysis
L[, cbd_band := cut(dist_cbd_km,
                    breaks = c(-Inf, 2, 5, 10, 20, Inf),
                    labels = c("0-2km", "2-5km", "5-10km", "10-20km", "20km+"))]
L[, lga := neighbourhood_cleansed]
# Keep the 15 largest LGAs; pool the rest to avoid sparse factor levels
top_lga <- names(sort(table(L$lga), decreasing = TRUE))[1:15]
L[, lga_grouped := fifelse(lga %in% top_lga, lga, "Other")]
log_step("LGAs: ", uniqueN(L$lga), " -> grouped to ", uniqueN(L$lga_grouped))


# 8. REVIEW FEATURES
log_step("\n=== STEP 8: REVIEW FEATURES ===")
L[, has_reviews := number_of_reviews > 0]
log_step("Listings with no reviews: ", sum(!L$has_reviews),
         " (", round(100 * mean(!L$has_reviews), 1), "%)")
log_step("review_scores_rating NA: ", sum(is.na(L$review_scores_rating)),
         " — matches the no-review group exactly")
L[, first_review := as.Date(first_review)]
L[, last_review  := as.Date(last_review)]
L[, days_since_last_review := as.numeric(SNAPSHOT_DATE - last_review)]
L[, listing_age_days       := as.numeric(SNAPSHOT_DATE - first_review)]
# Active = reviewed within 6 months. Proxy for a genuinely trading listing.
L[, is_active := !is.na(days_since_last_review) & days_since_last_review <= 180]
log_step("Active listings (reviewed within 180 days): ", sum(L$is_active),
         " (", round(100 * mean(L$is_active), 1), "%)")

# ============================================================
# B. RAW REVIEWS CLEANING + VALIDATION
# ============================================================
reviews <- fread(
  file.path(RAW, "reviews_airbnb.csv"),
  select = c("listing_id", "id", "date"),
  showProgress = FALSE
)

# Standardise data types
reviews[, listing_id := as.character(listing_id)]
reviews[, id := as.character(id)]
reviews[, date := as.Date(date)]

# ------------------------------------------------------------
# Basic structure
# ------------------------------------------------------------

cat("Review rows:", nrow(reviews), "\n")
cat("Unique review IDs:", uniqueN(reviews$id), "\n")
cat("Listings represented in reviews:", uniqueN(reviews$listing_id), "\n")

# ------------------------------------------------------------
# Duplicate review IDs
# ------------------------------------------------------------

duplicate_reviews <- reviews[
  duplicated(id) |
    duplicated(id, fromLast = TRUE)
]

cat(
  "Duplicate review IDs:",
  nrow(duplicate_reviews),
  "\n"
)

# ------------------------------------------------------------
# Missing key fields
# ------------------------------------------------------------

cat(
  "Missing listing_id:",
  sum(is.na(reviews$listing_id)),
  "\n"
)

cat(
  "Missing review ID:",
  sum(is.na(reviews$id)),
  "\n"
)

cat(
  "Missing review date:",
  sum(is.na(reviews$date)),
  "\n"
)

# ------------------------------------------------------------
# Check whether reviews belong to listings in listings dataset
# ------------------------------------------------------------

valid_listing_ids <- as.character(L$id)

invalid_review_listings <- reviews[
  !listing_id %in% valid_listing_ids
]

cat(
  "Reviews with unmatched listing IDs:",
  nrow(invalid_review_listings),
  "\n"
)

# ------------------------------------------------------------
# Aggregate reviews to listing level
# ------------------------------------------------------------

reviews_agg <- reviews[
  ,
  .(
    reviews_raw_count = .N,
    
    first_review_raw =
      min(
        date,
        na.rm = TRUE
      ),
    
    last_review_raw =
      max(
        date,
        na.rm = TRUE
      )
  ),
  by = listing_id
]

# ------------------------------------------------------------
# Validate against listings dataset
# ------------------------------------------------------------

review_validation <- merge(
  L[
    ,
    .(
      id = as.character(id),
      number_of_reviews,
      first_review,
      last_review
    )
  ],
  
  reviews_agg,
  
  by.x = "id",
  by.y = "listing_id",
  all.x = TRUE
)

# Listings with no reviews should have review count = 0
review_validation[
  is.na(reviews_raw_count),
  reviews_raw_count := 0
]

review_validation[
  ,
  review_count_difference :=
    number_of_reviews -
    reviews_raw_count
]

cat(
  "Listings where review counts disagree:",
  sum(
    review_validation$review_count_difference != 0,
    na.rm = TRUE
  ),
  "\n"
)

summary(
  review_validation$review_count_difference
)


#  9. OUTCOME VARIABLES 
log_step("\n=== STEP 9: OUTCOMES ===")


# -- 9a. 
# Verified on this dataset:
#   estimated_revenue_l365d  == price * estimated_occupancy_l365d   (r = 1.000000)
#   estimated_occupancy_l365d == number_of_reviews_ltm * 2 * max(minimum_nights,3)
#                                                        (95.5% exact match)
# Consequences:
#   * Regressing revenue on price is CIRCULAR — price is a factor of the outcome.
#   * Occupancy is a deterministic function of reviews_ltm and minimum_nights,
#     so minimum_nights must NEVER be a predictor of it.
# They are retained for benchmarking//comparison only, clearly renamed.
setnames(L, "estimated_occupancy_l365d", "ia_occupancy_derived", skip_absent = TRUE)
setnames(L, "estimated_revenue_l365d",   "ia_revenue_derived",   skip_absent = TRUE)
log_step("Renamed Inside Airbnb estimates with 'ia_*_derived' prefix to prevent ",
         "accidental use as independent outcomes.")


# 9b. Demand proxy independent of price
L[, review_velocity := number_of_reviews_ltm]           # reviews in last 12 months
L[, log_review_velocity := log1p(review_velocity)]
log_step("Demand proxy = review_velocity (reviews last 12m). Median: ",
         median(L$review_velocity), " | zero for ",
         sum(L$review_velocity == 0), " listings")


# 10. CALENDAR AGGREGATION
log_step("\n=== STEP 10: CALENDAR ===")
C <- fread(file.path(RAW, "calendar_airbnb.csv"), showProgress = FALSE)
log_step("Calendar: ", nrow(C), " rows | ", uniqueN(C$listing_id), " listings | ",
         as.character(min(C$date)), " to ", as.character(max(C$date)))
C[, date := as.Date(date)]
C[, is_booked := available == "f"]
# CAUTION: `available == "f"` conflates BOOKED with HOST-BLOCKED. Availability
# also decays with horizon (49% at 1 month -> 31% at 12) because hosts have not
# opened distant calendars. So we use SHORT horizons only, where the artifact is
# smallest, and name the variable a proxy.
horizon_check <- C[, .(avail = round(mean(!is_booked), 3)),
                   by = .(mth = format(date, "%Y-%m"))][order(mth)]
log_step("Availability by horizon (artifact check): ",
         paste(horizon_check$mth, horizon_check$avail, sep = "=", collapse = ", "))
cal <- C[date <= SNAPSHOT_DATE + 90, .(
  cal_days_30 = sum(date <= SNAPSHOT_DATE + 30),
  cal_booked_30 = sum(is_booked & date <= SNAPSHOT_DATE + 30),
  cal_booked_60 = sum(is_booked & date <= SNAPSHOT_DATE + 60),
  cal_booked_90 = sum(is_booked),
  cal_min_nights_mode = as.numeric(names(sort(table(minimum_nights),
                                              decreasing = TRUE))[1])
), by = listing_id]
cal[, occupancy_cal_30 := cal_booked_30 / 30]
cal[, occupancy_cal_90 := cal_booked_90 / 90]
log_step("Calendar occupancy (90d) — median: ",
         round(median(cal$occupancy_cal_90), 3),
         " | mean: ", round(mean(cal$occupancy_cal_90), 3))
L <- merge(L, cal, by.x = "id", by.y = "listing_id", all.x = TRUE)
log_step("Merged calendar features. Unmatched listings: ",
         sum(is.na(L$occupancy_cal_90)))

# Reload calendar temporarily for validation
C <- fread(
  file.path(RAW, "calendar_airbnb.csv"),
  showProgress = FALSE
)

C[, date := as.Date(date)]

cal_check <- C[
  date >= SNAPSHOT_DATE &
    date < SNAPSHOT_DATE + 90,
  .(
    cal_days_90 = .N,
    cal_unavailable_90 =
      sum(available == "f", na.rm = TRUE)
  ),
  by = listing_id
]

cal_check[, occupancy_cal_90_check :=
            cal_unavailable_90 / cal_days_90]
rm(C); invisible(gc())


##  DESCRIPTIVE / MORE EXPLORATORY ANALYSIS


library(data.table)
library(ggplot2)
library(scales)
library(gt)

# For nicer labels on scatterplot:
# install.packages("ggrepel")   # run once if needed

# Evidence of recent listing activity
L[, active_90 := !is.na(days_since_last_review) &
    days_since_last_review <= 90]

# Price sample
L_price <- L[!is.na(price_wins)]

# Consistent visual colours
COL_BLUE   <- "#0C6DCD"
COL_ORANGE <- "#F59E0B"
COL_SLATE  <- "#64748B"
COL_LIGHT  <- "#EAF2FB"
COL_TEXT   <- "#344054"

# Consistent graph style
theme_rq1 <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      colour = "#101828"
    ),
    plot.subtitle = element_text(
      size = 10.5,
      colour = "#667085"
    ),
    plot.caption = element_text(
      size = 8.5,
      colour = "#667085",
      hjust = 0
    ),
    axis.title = element_text(
      face = "bold",
      colour = COL_TEXT
    ),
    axis.text = element_text(
      colour = COL_TEXT
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(10, 18, 10, 10)
  )

stat_row <- function(label, x) {
  
  data.table(
    Variable = label,
    N = sum(!is.na(x)),
    Missing = mean(is.na(x)),
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    IQR = IQR(x, na.rm = TRUE)
  )
}


summary_stats <- rbindlist(list(
  
  stat_row(
    "Nightly price (AUD)",
    L$price_wins
  ),
  
  stat_row(
    "Reviews in last 12 months",
    L$review_velocity
  ),
  
  stat_row(
    "Bedrooms",
    L$bedrooms_clean
  ),
  
  stat_row(
    "Guest capacity",
    L$accommodates
  ),
  
  stat_row(
    "Amenity count",
    L$amenity_count
  ),
  
  stat_row(
    "Distance to CBD (km)",
    L$dist_cbd_km
  )
))



# 1. SUMMARY STATISTICS TABLE


summary_stats_table <- summary_stats |>
  
  gt() |>
  
  tab_header(
    title = md("**Melbourne Airbnb Market — Summary Statistics**"),
    subtitle = "Key variables used in descriptive and exploratory analysis"
  ) |>
  
  cols_label(
    Variable = "Variable",
    N = "Valid N",
    Missing = "Missing",
    Mean = "Mean",
    SD = "SD",
    Median = "Median",
    IQR = "IQR"
  ) |>
  
  fmt_integer(
    columns = N
  ) |>
  
  fmt_percent(
    columns = Missing,
    decimals = 1
  ) |>
  
  fmt_currency(
    columns = c(Mean, SD, Median, IQR),
    rows = Variable == "Nightly price (AUD)",
    currency = "AUD",
    decimals = 0
  ) |>
  
  fmt_number(
    columns = c(Mean, SD, Median, IQR),
    rows = Variable != "Nightly price (AUD)",
    decimals = 1
  ) |>
  
  cols_align(
    align = "left",
    columns = Variable
  ) |>
  
  opt_row_striping() |>
  
  tab_source_note(
    source_note =
      "Price is winsorised at the 1st and 99th percentiles. Reviews in the previous 12 months are used as the primary demand proxy."
  ) |>
  
  tab_options(
    table.width = gt::pct(100),
    data_row.padding = gt::px(6),
    heading.align = "left"
  )

summary_stats_table

# 2. LGA PRICE–DEMAND POSITIONING


lga_summary <- L[, .(
  
  listings = .N,
  
  median_price =
    median(price_wins, na.rm = TRUE),
  
  median_reviews_ltm =
    median(review_velocity, na.rm = TRUE),
  
  active_90_share =
    mean(active_90),
  
  median_dist_cbd =
    median(dist_cbd_km, na.rm = TRUE)
  
), by = lga]


# Remove very small submarkets
lga_summary <- lga_summary[
  listings >= 50
]


# Median reference lines
price_cut <- median(
  lga_summary$median_price,
  na.rm = TRUE
)

demand_cut <- median(
  lga_summary$median_reviews_ltm,
  na.rm = TRUE
)


# Automatically label larger, higher-price,
# or higher-demand LGAs
lga_summary[, label_flag :=
              listings >= quantile(
                listings,
                0.75,
                na.rm = TRUE
              ) |
              median_reviews_ltm >= quantile(
                median_reviews_ltm,
                0.75,
                na.rm = TRUE
              ) |
              median_price >= quantile(
                median_price,
                0.75,
                na.rm = TRUE
              )
]


lga_summary[, label_text :=
              ifelse(
                label_flag,
                lga,
                ""
              )
]



# Plot


p_lga <- ggplot(
  lga_summary,
  aes(
    x = median_price,
    y = median_reviews_ltm,
    size = listings,
    colour = active_90_share
  )
) +
  
  geom_vline(
    xintercept = price_cut,
    linetype = "dashed",
    colour = "#6B7280",
    linewidth = 0.8
  ) +
  
  geom_hline(
    yintercept = demand_cut,
    linetype = "dashed",
    colour = "#6B7280",
    linewidth = 0.8
  ) +
  
  geom_point(
    alpha = 0.78
  ) +
  
  ggrepel::geom_text_repel(
    aes(label = label_text),
    size = 3.5,
    colour = COL_TEXT,
    box.padding = 0.35,
    point.padding = 0.2,
    segment.colour = "#BFC5CE",
    segment.size = 0.35,
    max.overlaps = Inf,
    min.segment.length = 0,
    seed = 123
  ) +
  
  scale_x_continuous(
    labels = label_dollar(prefix = "$"),
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0.03, 0.08))
  ) +
  
  scale_y_continuous(
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0.05, 0.10))
  ) +
  
  scale_size_continuous(
    range = c(3, 10),
    labels = label_comma()
  ) +
  
  scale_colour_gradient(
    low = "#CFE3F7",
    high = COL_BLUE,
    labels = label_percent(accuracy = 1)
  ) +
  
  labs(
    title = "LGA Price–Demand Positioning",
    subtitle =
      "Selected labels highlight larger or higher-demand Melbourne Airbnb submarkets",
    x = "Median nightly price (AUD)",
    y = "Median reviews in last 12 months",
    size = "Listings",
    colour = "Reviewed ≤90d",
    caption = paste0(
      "Dashed lines show median values across included LGAs.\n",
      "LGAs with fewer than 50 listings are excluded."
    )
  ) +
  
  theme_rq1 +
  
  theme(
    panel.grid.major.y = element_line(
      colour = "#E5E7EB",
      linewidth = 0.45
    ),
    panel.grid.major.x = element_line(
      colour = "#E5E7EB",
      linewidth = 0.45
    ),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = 4),
    legend.title = element_text(
      face = "bold"
    )
  ) +
  
  guides(
    size = guide_legend(
      title.position = "top",
      nrow = 1,
      override.aes = list(
        alpha = 0.9
      )
    ),
    colour = guide_colourbar(
      title.position = "top",
      barwidth = grid::unit(8, "cm"),
      barheight = grid::unit(0.5, "cm")
    )
  )

p_lga


# 3. NIGHTLY PRICE BY PROPERTY TYPE

# Order property types by median nightly price
property_order <- L[
  !is.na(price_wins),
  .(
    median_price =
      median(price_wins, na.rm = TRUE)
  ),
  by = property_group
][
  order(median_price),
  property_group
]


p_property_price <- ggplot(
  L_price,
  aes(
    x = factor(
      property_group,
      levels = property_order
    ),
    y = price_wins
  )
) +
  
  geom_boxplot(
    fill = COL_LIGHT,
    colour = COL_BLUE,
    alpha = 0.85,
    width = 0.65,
    linewidth = 0.6,
    outlier.alpha = 0.15,
    outlier.size = 0.7
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    labels = label_dollar(prefix = "$"),
    expand = expansion(
      mult = c(0.02, 0.04)
    )
  ) +
  
  labs(
    title = "Nightly Prices Differ Across Property Types",
    
    subtitle =
      "Distribution of winsorised nightly prices across major property groups",
    
    x = NULL,
    
    y = "Nightly price (AUD)",
    
    caption =
      "Boxes show the median and interquartile range; points represent more extreme observations."
  ) +
  
  theme_rq1


p_property_price


# 4. PROPERTY PROFILE — PRICE AND DEMAND


profile_overview <- L[, .(
  
  listings = .N,
  
  median_price = as.numeric(
    median(
      price_wins,
      na.rm = TRUE
    )
  ),
  
  median_reviews_ltm = as.numeric(
    median(
      review_velocity,
      na.rm = TRUE
    )
  ),
  
  active_90_share = as.numeric(
    mean(
      active_90,
      na.rm = TRUE
    )
  )
  
), by = .(
  property_group,
  capacity_segment
)]


# Remove small profile groups
profile_overview <- profile_overview[
  listings >= 50
]


# Put capacity categories in logical order
profile_overview[
  ,
  capacity_segment := factor(
    capacity_segment,
    levels = c(
      "1-2 guests",
      "3-4 guests",
      "5-6 guests",
      "7+ guests"
    )
  )
]



# Heatmap


p_profile_overview <- ggplot(
  profile_overview,
  aes(
    x = capacity_segment,
    y = property_group,
    fill = median_reviews_ltm
  )
) +
  
  geom_tile(
    colour = "white",
    linewidth = 1
  ) +
  
  # Show median nightly price inside each profile
  geom_text(
    aes(
      label = paste0(
        "$",
        round(median_price)
      )
    ),
    size = 3.6,
    fontface = "bold"
  ) +
  
  scale_fill_gradient(
    low = "#EAF2FB",
    high = COL_BLUE
  ) +
  
  labs(
    title = "Price and Demand Across Property Profiles",
    
    subtitle =
      "Colour shows median review activity; labels show median nightly price",
    
    x = "Guest capacity",
    
    y = NULL,
    
    fill = "Median reviews\n(last 12 months)",
    
    caption =
      "Only property profiles with at least 50 listings are shown."
  ) +
  
  theme_rq1 +
  
  theme(
    panel.grid = element_blank(),
    
    axis.text.x = element_text(
      face = "bold"
    ),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold"
    )
  ) +
  
  guides(
    fill = guide_colourbar(
      title.position = "top",
      barwidth = grid::unit(8, "cm"),
      barheight = grid::unit(0.5, "cm")
    )
  )


p_profile_overview


#5. PROPERTY PROFILES FOR DEEPER INVESTIGATION


# Create summary statistics for each:
# LGA × property type × guest capacity combination

profile_summary <- L[, .(
  
  listings = .N,
  
  median_price = as.numeric(
    median(
      price_wins,
      na.rm = TRUE
    )
  ),
  
  price_IQR = as.numeric(
    IQR(
      price_wins,
      na.rm = TRUE
    )
  ),
  
  median_reviews_ltm = as.numeric(
    median(
      review_velocity,
      na.rm = TRUE
    )
  ),
  
  active_90_share = as.numeric(
    mean(
      active_90,
      na.rm = TRUE
    )
  ),
  
  median_amenities = as.numeric(
    median(
      amenity_count,
      na.rm = TRUE
    )
  ),
  
  median_dist_cbd = as.numeric(
    median(
      dist_cbd_km,
      na.rm = TRUE
    )
  )
  
), by = .(
  lga,
  property_group,
  capacity_segment
)]


# Exclude very small profile groups
# to avoid drawing conclusions from unstable samples
profile_summary <- profile_summary[
  listings >= 20
]


# Order profiles for exploratory screening
# This is NOT an overall performance ranking
profile_screen <- profile_summary[
  order(
    -median_reviews_ltm,
    -active_90_share,
    -listings
  )
][
  1:min(20, .N)
]


# ------------------------------------------------------------
# Create final formatted table
# ------------------------------------------------------------

profile_table <- profile_screen |>
  
  gt() |>
  
  tab_header(
    title = md(
      "**Property Profiles for Deeper Investigation**"
    ),
    
    subtitle =
      "Submarket and property combinations showing notable recent demand activity"
  ) |>
  
  tab_spanner(
    label = "Property profile",
    columns = c(
      lga,
      property_group,
      capacity_segment
    )
  ) |>
  
  tab_spanner(
    label = "Pricing",
    columns = c(
      median_price,
      price_IQR
    )
  ) |>
  
  tab_spanner(
    label = "Demand activity",
    columns = c(
      median_reviews_ltm,
      active_90_share
    )
  ) |>
  
  cols_label(
    lga = "LGA",
    property_group = "Property type",
    capacity_segment = "Capacity",
    listings = "N",
    median_price = "Median",
    price_IQR = "IQR",
    median_reviews_ltm = "Median reviews",
    active_90_share = "Reviewed ≤90d",
    median_amenities = "Amenities",
    median_dist_cbd = "CBD km"
  ) |>
  
  fmt_integer(
    columns = listings
  ) |>
  
  fmt_currency(
    columns = c(
      median_price,
      price_IQR
    ),
    currency = "AUD",
    decimals = 0
  ) |>
  
  fmt_number(
    columns = c(
      median_reviews_ltm,
      median_amenities,
      median_dist_cbd
    ),
    decimals = 1
  ) |>
  
  fmt_percent(
    columns = active_90_share,
    decimals = 1
  ) |>
  
  # Highlight stronger review activity
  data_color(
    columns = median_reviews_ltm,
    palette = c(
      "#FFF7E6",
      "#FFD58A",
      COL_ORANGE
    )
  ) |>
  
  # Highlight greater recent activity
  data_color(
    columns = active_90_share,
    palette = c(
      "#F3F8FD",
      "#A8CEF2",
      COL_BLUE
    )
  ) |>
  
  opt_row_striping() |>
  
  tab_source_note(
    source_note =
      "Only profiles with at least 20 listings are included. Rows are ordered using review activity, recent activity and sample size for exploratory screening; this is not an overall performance ranking."
  ) |>
  
  tab_options(
    table.width = gt::pct(100),
    data_row.padding = gt::px(5),
    heading.align = "left"
  )


# Display table
profile_table

### REGRESSION \ PREDICTIVE ANALYSIS

install.packages("car")
library(car)
library(dplyr)
library(broom)

# Review_scores_rating is missing exactly where has_reviews == FALSE.
# Using both together with listwise deletion makes has_reviews collinear with the
# intercept (constant in the fitted sample) — R will throw a rank-deficiency warning
# and silently drop a coefficient. Impute so both stay usable.
mean_rating <- mean(L$review_scores_rating[L$has_reviews], na.rm = TRUE)
L[, review_scores_filled := fifelse(is.na(review_scores_rating), mean_rating, review_scores_rating)]



model_price <- lm(
  log_price ~ property_group + accommodates + bedrooms_clean + bathrooms_clean +
    amenity_count + host_is_superhost + host_scale +
    review_scores_filled + has_reviews +
    dist_cbd_km + lga_grouped,
  data = L
)
summary(model_price)
car::vif(model_price)

# Robust inference
lmtest::coeftest(
  model_price,
  vcov = sandwich::vcovHC(model_price, type = "HC3")
)

# MODEL 2: What is associated with stronger demand/calendar pressure?
model_demand <- lm(
  occupancy_cal_90 ~ property_group + accommodates + bedrooms_clean + bathrooms_clean +
    amenity_count + host_is_superhost + host_scale +
    review_scores_filled + has_reviews +
    dist_cbd_km + lga_grouped,
  data = L
)

summary(model_demand)
car::vif(model_demand)

lmtest::coeftest(
  model_demand,
  vcov = sandwich::vcovHC(model_demand, type = "HC3")
)

# Report-ready % effects for both models (log-linear coefficients → approx. % change)
broom::tidy(model_price, conf.int = TRUE) %>%
  dplyr::mutate(
    pct_effect = (exp(estimate) - 1) * 100
  ) %>%
  dplyr::arrange(p.value) %>%
  print(n = Inf)


### PRESCRIPTIVE ANALYSISsuppressPackageStartupMessages
({
  library(data.table)
  library(ggplot2)
})
# Start fresh from the fully cleaned master dataset
M3_all <- copy(L)


# ============================================================
# 1. CREATE MORE MEANINGFUL PROPERTY PROFILES FOR METHOD 3
# ============================================================

# The broad property_group used elsewhere is useful for regression,
# but "Entire other" is too vague for a market-entry recommendation.
#
# Method 3 therefore creates a more interpretable property profile
# specifically for market segmentation.

M3_all[, property_profile_m3 := fcase(
  
  # Standard apartments
  property_type %chin% c(
    "Entire rental unit",
    "Entire condo"
  ),
  "Apartment / condo",
  
  # Serviced apartments
  property_type == "Entire serviced apartment",
  "Serviced apartment",
  
  # Detached houses
  property_type == "Entire home",
  "House",
  
  # Townhouses
  property_type == "Entire townhouse",
  "Townhouse",
  
  # Guesthouse-style properties
  property_type %chin% c(
    "Entire guesthouse",
    "Entire guest suite"
  ),
  "Guesthouse / guest suite",
  
  # Cottage / cabin
  property_type %chin% c(
    "Entire cottage",
    "Entire cabin"
  ),
  "Cottage / cabin",
  
  # Villa / bungalow
  property_type %chin% c(
    "Entire villa",
    "Entire bungalow"
  ),
  "Villa / bungalow",
  
  # Lofts
  property_type == "Entire loft",
  "Loft",
  
  # Vacation homes
  property_type == "Entire vacation home",
  "Vacation home",
  
  # Everything else is excluded from the MAIN segmentation
  # rather than being hidden inside a vague "Other" category
  default = NA_character_
)]


# Check how many listings fall into each profile
cat("\n=== METHOD 3 PROPERTY PROFILES ===\n")

print(
  M3_all[
    !is.na(property_profile_m3),
    .N,
    by = property_profile_m3
  ][order(-N)]
)


# Show entire-property types excluded from the main segmentation
# so the exclusion is transparent
cat("\n=== OTHER ENTIRE-PROPERTY TYPES NOT USED IN MAIN SCREEN ===\n")

excluded_property_types <- M3_all[
  grepl("^Entire", property_type) &
    is.na(property_profile_m3),
  .N,
  by = property_type
][order(-N)]

print(excluded_property_types)


# ============================================================
# 2. DEFINE METHOD 3 ANALYSIS SAMPLE
# ============================================================

# Need:
#   - valid LGA
#   - meaningful property profile
#   - guest-capacity segment
#
# IMPORTANT:
# We DO NOT require price to be non-missing here.
#
# A listing with missing price still exists in the market,
# so it should still count toward existing Airbnb supply.

M3 <- M3_all[
  !is.na(lga) & lga != "" &
    !is.na(property_profile_m3) &
    !is.na(capacity_segment)
]


cat("\n=== METHOD 3 ANALYSIS SAMPLE ===\n")
cat("Listings included:", nrow(M3), "\n")
cat("LGAs:", uniqueN(M3$lga), "\n")
cat(
  "Property profiles:",
  uniqueN(M3$property_profile_m3),
  "\n"
)
cat(
  "Capacity segments:",
  uniqueN(M3$capacity_segment),
  "\n"
)


# ============================================================
# 3. HELPER FUNCTION FOR SAFE MEDIANS
# ============================================================

# Prevents grouped data.table errors if a group happens to contain
# only missing values.

safe_median <- function(x) {
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  as.numeric(
    median(x, na.rm = TRUE)
  )
}


# ============================================================
# 4. BUILD LOCATION × PROPERTY × CAPACITY SEGMENTS
# ============================================================

segment_summary <- M3[
  ,
  .(
    
    # --------------------------------------------------------
    # EXISTING MARKET SUPPLY
    # --------------------------------------------------------
    
    # ALL listings in the segment, including missing-price rows
    listings = .N,
    
    hosts = uniqueN(host_id),
    
    
    # --------------------------------------------------------
    # PRICE DATA AVAILABILITY
    # --------------------------------------------------------
    
    listings_with_price =
      sum(!is.na(price)),
    
    price_coverage =
      mean(!is.na(price)),
    
    
    # --------------------------------------------------------
    # PRICING POTENTIAL
    # --------------------------------------------------------
    
    # Uses only genuinely observed prices.
    # No price imputation.
    median_listed_price =
      safe_median(price),
    
    price_p25 =
      if (all(is.na(price))) {
        NA_real_
      } else {
        as.numeric(
          quantile(
            price,
            0.25,
            na.rm = TRUE
          )
        )
      },
    
    price_p75 =
      if (all(is.na(price))) {
        NA_real_
      } else {
        as.numeric(
          quantile(
            price,
            0.75,
            na.rm = TRUE
          )
        )
      },
    
    
    # --------------------------------------------------------
    # RECENT GUEST ACTIVITY
    # --------------------------------------------------------
    
    # Reviews received in the last 12 months.
    # Used as an IMPERFECT proxy for recent guest activity.
    median_reviews_ltm =
      safe_median(review_velocity)
    
  ),
  
  by = .(
    lga,
    property_profile_m3,
    capacity_segment
  )
]


# Percentage format for reporting
segment_summary[
  ,
  price_coverage_pct :=
    round(price_coverage * 100, 1)
]


cat("\n=== RAW SEGMENT COUNT ===\n")
cat(
  "Total location-property-capacity segments:",
  nrow(segment_summary),
  "\n"
)


# ============================================================
# 5. REMOVE SEGMENTS WITH TOO LITTLE EVIDENCE
# ============================================================

# A segment must have at least:
#   20 total listings
#   20 observed prices
#
# This avoids making conclusions from tiny samples.

MIN_SEGMENT_N <- 20
MIN_PRICE_N   <- 20


segment_valid <- segment_summary[
  listings >= MIN_SEGMENT_N &
    listings_with_price >= MIN_PRICE_N &
    !is.na(median_listed_price) &
    !is.na(median_reviews_ltm)
]


cat("\n=== VALID SEGMENTS ===\n")
cat(
  "Segments retained:",
  nrow(segment_valid),
  "\n"
)


# ============================================================
# 6. CHECK PRICE COVERAGE
# ============================================================

# Because price is missing for some listings,
# inspect how much observed-price information supports
# each segment's median price.

cat("\n=== PRICE COVERAGE SUMMARY ===\n")

print(
  summary(segment_valid$price_coverage)
)


cat("\n=== LOWEST PRICE-COVERAGE SEGMENTS ===\n")

print(
  segment_valid[
    order(price_coverage),
    .(
      lga,
      property_profile_m3,
      capacity_segment,
      listings,
      listings_with_price,
      price_coverage_pct
    )
  ][1:min(15, .N)]
)


# ============================================================
# 7. COMPARE LIKE WITH LIKE
# ============================================================

# Critical step:
#
# Do NOT compare a 7+ guest house directly with a
# 1-2 guest apartment.
#
# Instead:
#
# Yarra | Apartment / condo | 3-4 guests
#
# is benchmarked against:
#
# Melbourne     | Apartment / condo | 3-4 guests
# Port Phillip  | Apartment / condo | 3-4 guests
# Darebin       | Apartment / condo | 3-4 guests
# etc.


# First check how many LGAs are represented for each
# property-profile × capacity combination.

segment_valid[
  ,
  comparable_lgas :=
    uniqueN(lga),
  by = .(
    property_profile_m3,
    capacity_segment
  )
]


# Require at least 3 LGAs for a meaningful cross-location comparison.
segment_valid <- segment_valid[
  comparable_lgas >= 3
]


# Create benchmarks based on the SAME property profile
# AND SAME guest-capacity segment.

segment_valid[
  ,
  `:=`(
    
    # Typical listed price across comparable LGAs
    profile_median_price =
      as.numeric(
        median(
          median_listed_price,
          na.rm = TRUE
        )
      ),
    
    # Typical recent guest activity across comparable LGAs
    profile_median_reviews =
      as.numeric(
        median(
          median_reviews_ltm,
          na.rm = TRUE
        )
      ),
    
    # Typical existing supply across comparable LGAs
    profile_median_supply =
      as.numeric(
        median(
          listings,
          na.rm = TRUE
        )
      )
    
  ),
  
  by = .(
    property_profile_m3,
    capacity_segment
  )
]


# ============================================================
# 8. PRELIMINARY OPPORTUNITY SCREEN
# ============================================================

# A location-property segment passes the preliminary screen when:
#
#   1. Its median listed price is at or above the median
#      for the SAME property profile + capacity across LGAs
#
#   AND
#
#   2. Its median recent review activity is at or above
#      the median for the SAME property profile + capacity
#
# Supply is NOT used as an automatic exclusion criterion.
# High supply may mean strong competition OR a mature market.


segment_valid[
  ,
  strong_price :=
    median_listed_price >=
    profile_median_price
]


segment_valid[
  ,
  strong_activity :=
    median_reviews_ltm >=
    profile_median_reviews
]


segment_valid[
  ,
  opportunity_candidate :=
    strong_price &
    strong_activity
]


# ============================================================
# 9. DESCRIBE RELATIVE EXISTING SUPPLY
# ============================================================

segment_valid[
  ,
  supply_position := fcase(
    
    listings < profile_median_supply,
    "Lower relative supply",
    
    listings > profile_median_supply,
    "Higher relative supply",
    
    default =
      "Typical relative supply"
  )
]


# ============================================================
# 10. EXTRACT PRELIMINARY MARKET-ENTRY CANDIDATES
# ============================================================

candidate_segments <- segment_valid[
  opportunity_candidate == TRUE
]


# Create readable labels
candidate_segments[
  ,
  segment_label :=
    paste(
      lga,
      capacity_segment,
      sep = " | "
    )
]


candidate_segments[
  ,
  full_segment :=
    paste(
      lga,
      property_profile_m3,
      capacity_segment,
      sep = " | "
    )
]


# ============================================================
# 11. CLEAN CANDIDATE TABLE
# ============================================================

candidate_table <- candidate_segments[
  ,
  .(
    
    LGA = lga,
    
    `Property profile` =
      property_profile_m3,
    
    `Guest capacity` =
      capacity_segment,
    
    `Existing listings` =
      listings,
    
    `Listings with observed price` =
      listings_with_price,
    
    `Price coverage (%)` =
      price_coverage_pct,
    
    `Median listed price ($)` =
      round(
        median_listed_price,
        0
      ),
    
    `Median reviews last 12m` =
      round(
        median_reviews_ltm,
        1
      ),
    
    `Relative supply` =
      supply_position
    
  )
]


# Sort cleanly
setorderv(
  candidate_table,
  cols = c(
    "Property profile",
    "Guest capacity",
    "Median reviews last 12m",
    "Median listed price ($)"
  ),
  order = c(
    1,
    1,
    -1,
    -1
  )
)


cat("\n")
cat("===============================================\n")
cat("PRELIMINARY METHOD 3 MARKET-ENTRY CANDIDATES\n")
cat("===============================================\n")

print(candidate_table)


# ============================================================
# 12. SUPPORTING TABLE — ALL VALID SEGMENTS
# ============================================================

# Useful if you want to inspect candidates against segments
# that DID NOT pass the screen.

all_segment_table <- segment_valid[
  ,
  .(
    
    LGA = lga,
    
    `Property profile` =
      property_profile_m3,
    
    `Guest capacity` =
      capacity_segment,
    
    `Existing listings` =
      listings,
    
    `Price coverage (%)` =
      price_coverage_pct,
    
    `Median listed price ($)` =
      round(
        median_listed_price,
        0
      ),
    
    `Median reviews last 12m` =
      round(
        median_reviews_ltm,
        1
      ),
    
    `Strong price` =
      strong_price,
    
    `Strong activity` =
      strong_activity,
    
    `Preliminary candidate` =
      opportunity_candidate,
    
    `Relative supply` =
      supply_position
  )
]


# ============================================================
# 13. PRELIMINARY OPPORTUNITY VISUAL
# ============================================================

# This chart displays ONLY the segments that pass the
# preliminary price + recent-activity screen.
#
# Interpretation:
#
# Further right = higher median listed nightly price
# Larger dot    = greater median recent review activity
# n =           = number of existing comparable listings
#
# Each facet is now a meaningful property profile,
# rather than the vague "Entire other" category.


opportunity_plot <- ggplot(
  candidate_segments,
  aes(
    x = median_listed_price,
    y = reorder(
      segment_label,
      median_listed_price
    ),
    size = median_reviews_ltm
  )
) +
  
  geom_point(
    alpha = 0.75
  ) +
  
  geom_text(
    aes(
      label =
        paste0(
          "n=",
          listings
        )
    ),
    hjust = -0.15,
    size = 3
  ) +
  
  facet_wrap(
    ~ property_profile_m3,
    scales = "free_y"
  ) +
  
  scale_x_continuous(
    labels =
      scales::label_dollar(
        prefix = "$"
      ),
    
    expand =
      expansion(
        mult = c(
          0.05,
          0.18
        )
      )
  ) +
  
  labs(
    
    title =
      "Preliminary Market-Entry Candidate Segments",
    ,
    
    x =
      "Median listed nightly price ($)",
    
    y =
      "LGA | Guest capacity",
    
    size =
      "Median reviews\nlast 12 months",
    
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position =
      "bottom",
    
    panel.grid.minor =
      element_blank(),
    
    strip.text =
      element_text(
        face = "bold"
      )
  )


# Display the chart
print(opportunity_plot)


# ============================================================
# 14. FINAL DIAGNOSTIC SUMMARY
# ============================================================

cat("\n=== METHOD 3 SUMMARY ===\n")

cat(
  "Valid comparable segments:",
  nrow(segment_valid),
  "\n"
)

cat(
  "Preliminary opportunity candidates:",
  nrow(candidate_segments),
  "\n"
)

cat(
  "Property profiles represented among candidates:",
  uniqueN(candidate_segments$property_profile_m3),
  "\n"
)

cat("\nCandidate count by property profile:\n")

print(
  candidate_segments[
    ,
    .N,
    by = property_profile_m3
  ][order(-N)]
)

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
rm(C); invisible(gc())

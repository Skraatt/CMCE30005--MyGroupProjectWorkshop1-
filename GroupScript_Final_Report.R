setwd("~/Downloads/BACgroup/CMCE30005--MyGroupProjectWorkshop1-")


# 0. SETUP


current_repos <- getOption("repos")
if (
  is.null(current_repos) ||
    is.na(current_repos["CRAN"]) ||
    current_repos["CRAN"] == "@CRAN@"
) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

required_packages <- c(
  "data.table", "stringr", "lubridate", "ggplot2", "ggrepel",
  "scales", "sandwich", "lmtest", "car", "ranger", "gt",
  "webshot2"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(ggrepel)
  library(scales)
  library(sandwich)
  library(lmtest)
  library(car)
  library(ranger)
  library(gt)
})

set.seed(30005)

RAW_DIR <- file.path("data", "raw")
CLEAN_DIR <- file.path("data", "clean")
OUTPUT_DIR <- file.path("outputs", "final_report")
FIGURE_DIR <- file.path(OUTPUT_DIR, "figures")
TABLE_DIR <- file.path(OUTPUT_DIR, "tables")
RESULT_DIR <- file.path(OUTPUT_DIR, "model_results")

dir.create(CLEAN_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RESULT_DIR, recursive = TRUE, showWarnings = FALSE)

SNAPSHOT_DATE <- as.Date("2026-06-16")
CBD_LAT <- -37.8136
CBD_LON <- 144.9631
BEACH_LAT <- -37.8677
BEACH_LON <- 144.9740

COL_MAIN <- "#E0484E"
COL_LIGHT <- "#F6A9AC"
COL_DARK <- "#484848"
COL_ACCENT <- "#00A699"
COL_GREY <- "#BFC4CC"
ZONE_A <- "#FCECEC"
ZONE_B <- "#FFF8F7"
NOTE_FILL <- "#F7F7F7"

LOG <- character()

log_step <- function(...) {
  message_text <- paste0(...)
  LOG <<- c(LOG, message_text)
  cat(message_text, "\n")
}

find_input <- function(candidates) {
  possible <- file.path(RAW_DIR, candidates)
  found <- possible[file.exists(possible)]
  if (length(found) == 0) {
    stop(
      "Could not find any of these files in data/raw: ",
      paste(candidates, collapse = ", ")
    )
  }
  found[1]
}

haversine_km <- function(lat1, lon1, lat2, lon2) {
  earth_radius <- 6371
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  angle <- 2 * atan2(sqrt(a), sqrt(1 - a))
  earth_radius * angle
}

safe_median <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  as.numeric(median(x, na.rm = TRUE))
}

safe_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  as.numeric(mean(x, na.rm = TRUE))
}

safe_min_date <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(as.Date(NA))
  min(x, na.rm = TRUE)
}

safe_max_date <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(as.Date(NA))
  max(x, na.rm = TRUE)
}

percent_effect <- function(beta) {
  100 * expm1(beta)
}

format_p <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p)))
}

format_effect <- function(x) {
  ifelse(
    is.na(x),
    "n/a",
    ifelse(abs(x) < 0.1, sprintf("%+.2f%%", x), sprintf("%+.1f%%", x))
  )
}

format_dollar <- function(x) {
  ifelse(is.na(x), "n/a", paste0("$", formatC(round(x), format = "d", big.mark = ",")))
}

check_expected <- function(actual, expected, label) {
  if (!identical(as.integer(actual), as.integer(expected))) {
    warning(label, " is ", actual, "; the report used ", expected, ".")
  }
}

save_plot <- function(plot, filename, width, height) {
  ggsave(
    filename = file.path(FIGURE_DIR, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

theme_report <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 17, colour = "#222222"),
    plot.subtitle = element_text(size = 12, colour = "#667085"),
    plot.caption = element_text(size = 9.5, colour = "#667085", hjust = 0),
    axis.title = element_text(face = "bold", colour = "#222222"),
    axis.text = element_text(size = 11, colour = "#333333"),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )

style_gt_table <- function(table_object, font_size = 14) {
  table_object |>
    tab_style(
      style = list(
        cell_fill(color = COL_MAIN),
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_column_labels(everything())
    ) |>
    opt_row_striping() |>
    tab_options(
      table.font.names = "Arial",
      table.font.size = px(font_size),
      table.background.color = "white",
      heading.align = "left",
      heading.title.font.size = px(font_size + 5),
      heading.title.font.weight = "bold",
      heading.subtitle.font.size = px(font_size - 1),
      column_labels.font.weight = "bold",
      data_row.padding = px(7),
      row.striping.background_color = "#F8EEEE",
      table.border.top.color = "#777777",
      table.border.bottom.color = "#BBBBBB"
    )
}

save_gt_png <- function(table_object, filename, width = 1400, zoom = 2) {
  gt::gtsave(
    data = table_object,
    filename = file.path(TABLE_DIR, filename),
    vwidth = width,
    zoom = zoom
  )
}


# 1. DATA PREPARATION


listings_file <- find_input(c("listings_airbnb.csv", "listings.csv"))
reviews_file <- find_input(c("reviews_airbnb.csv", "reviews.csv"))
calendar_file <- find_input(c("calendar_airbnb.csv", "calendar.csv"))

log_step("Loading listings")
L <- fread(listings_file, showProgress = FALSE)
raw_listing_rows <- nrow(L)
raw_listing_columns <- ncol(L)
raw_hosts <- uniqueN(L$host_id)

stopifnot(uniqueN(L$id) == nrow(L))

log_step(
  "Listings: ", raw_listing_rows, " rows, ", raw_listing_columns,
  " columns, ", raw_hosts, " hosts"
)

# 1.1 Remove unusable fields
is_blank <- function(x) {
  if (is.character(x)) {
    is.na(x) | trimws(x) == "" | x == "N/A"
  } else {
    is.na(x)
  }
}

missing_rate <- vapply(L, function(x) mean(is_blank(x)), numeric(1))
empty_columns <- names(missing_rate[missing_rate == 1])

unique_values <- vapply(
  L,
  function(x) uniqueN(x[!is_blank(x)]),
  numeric(1)
)

zero_variance_columns <- setdiff(
  names(unique_values[unique_values <= 1]),
  empty_columns
)

text_columns <- c(
  "listing_url", "scrape_id", "source", "picture_url", "host_url",
  "host_profile_id", "host_profile_url", "host_picture_url", "host_about",
  "host_verifications", "price_quote_raw", "neighborhood_overview"
)

redundant_columns <- c(
  "price_quote_price_per_night", "minimum_minimum_nights",
  "maximum_minimum_nights", "minimum_maximum_nights",
  "maximum_maximum_nights", "minimum_nights_avg_ntm",
  "maximum_nights_avg_ntm"
)

columns_to_drop <- unique(c(
  empty_columns,
  zero_variance_columns,
  intersect(text_columns, names(L)),
  intersect(redundant_columns, names(L))
))

if (length(columns_to_drop) > 0) {
  L[, (columns_to_drop) := NULL]
}

variables_after_initial_drops <- ncol(L)
L[, listing_id := as.character(id)]

log_step(
  "Removed ", length(empty_columns), " empty fields and ",
  length(zero_variance_columns), " zero-variance fields"
)

# 1.2 Price
L[, price_raw := price]
L[, price := as.numeric(str_replace_all(as.character(price_raw), "[$,]", ""))]
L[, price_missing := is.na(price)]

price_p01 <- as.numeric(quantile(L$price, 0.01, na.rm = TRUE))
price_p99 <- as.numeric(quantile(L$price, 0.99, na.rm = TRUE))

L[, price_outlier := !is.na(price) & (price < price_p01 | price > price_p99)]
L[, price_wins := pmin(pmax(price, price_p01), price_p99)]
L[, log_price := log(price_wins)]

price_missing_by_superhost <- L[
  ,
  .(
    listings = .N,
    missing_prices = sum(price_missing),
    missing_price_share = mean(price_missing)
  ),
  by = host_is_superhost
]

fwrite(
  price_missing_by_superhost,
  file.path(RESULT_DIR, "price_missingness_by_superhost.csv")
)

log_step(
  "Missing prices: ", sum(L$price_missing),
  "; price limits: $", round(price_p01), " and $", round(price_p99)
)

# 1.3 Property fields
bathroom_text <- tolower(trimws(as.character(L$bathrooms_text)))
bathroom_from_text <- as.numeric(str_extract(bathroom_text, "^[0-9]+\\.?[0-9]*"))
bathroom_from_text[
  str_detect(bathroom_text, "half-bath") & is.na(bathroom_from_text)
] <- 0.5

L[, bathrooms_clean := fifelse(
  is.na(bathrooms),
  bathroom_from_text,
  as.numeric(bathrooms)
)]

L[, bath_is_shared := str_detect(bathroom_text, "shared")]
L[, bath_is_private := str_detect(bathroom_text, "private")]
L[, bedrooms_missing := is.na(bedrooms)]
L[, beds_missing := is.na(beds)]
L[, bedrooms_clean := fifelse(is.na(bedrooms), 1, as.numeric(bedrooms))]
L[, beds_clean := fifelse(
  is.na(beds),
  pmax(1, ceiling(accommodates / 2)),
  as.numeric(beds)
)]

L[, property_group := fcase(
  str_detect(property_type, "^Entire rental unit|^Entire condo|^Entire serviced"),
  "Entire apartment",
  str_detect(property_type, "^Entire home|^Entire townhouse|^Entire villa|^Entire cottage|^Entire bungalow"),
  "Entire house",
  str_detect(property_type, "^Entire guest suite|^Entire guesthouse|^Entire loft|^Entire cabin"),
  "Entire other",
  str_detect(property_type, "^Private room"),
  "Private room",
  str_detect(property_type, "^Shared room"),
  "Shared room",
  str_detect(property_type, "hotel|Hotel|boutique"),
  "Hotel/serviced",
  default = "Other"
)]

L[, accommodates := pmax(as.numeric(accommodates), 1)]
L[, guests_per_bedroom := accommodates / pmax(bedrooms_clean, 1)]
L[, price_per_guest := price_wins / accommodates]
L[, capacity_segment := fcase(
  accommodates <= 2, "1-2 guests",
  accommodates <= 4, "3-4 guests",
  accommodates <= 6, "5-6 guests",
  default = "7+ guests"
)]

capacity_levels <- c("1-2 guests", "3-4 guests", "5-6 guests", "7+ guests")

# 1.4 Host fields
logical_host_fields <- intersect(
  c("host_is_superhost", "host_has_profile_pic", "host_identity_verified"),
  names(L)
)

for (field_name in logical_host_fields) {
  set(L, j = field_name, value = tolower(as.character(L[[field_name]])) == "t")
}

L[, host_is_multi := calculated_host_listings_count > 1]
L[, host_scale := fcase(
  is.na(calculated_host_listings_count), NA_character_,
  calculated_host_listings_count == 1, "Single",
  calculated_host_listings_count <= 5, "Small (2-5)",
  calculated_host_listings_count <= 20, "Medium (6-20)",
  default = "Large (21+)"
)]

# 1.5 Amenities
amenities_text <- tolower(as.character(L$amenities))
L[, amenity_count := str_count(amenities_text, ",") + 1L]
L[is.na(amenities) | amenities == "" | amenities == "[]", amenity_count := 0L]

amenity_patterns <- list(
  am_wifi = "wifi|internet",
  am_aircon = "air condition|\\bac\\b|cooling",
  am_heating = "heating",
  am_kitchen = "kitchen",
  am_washer = "washer",
  am_dryer = "dryer",
  am_parking = "free parking|parking on premises|garage",
  am_pool = "pool",
  am_gym = "gym|exercise",
  am_selfcheckin = "self check-in|lockbox|keypad",
  am_workspace = "dedicated workspace",
  am_tv = "\\btv\\b|television",
  am_bathtub = "bathtub",
  am_balcony = "balcony|patio",
  am_petsallowed = "pets allowed",
  am_elevator = "elevator",
  am_bbq = "bbq|barbecue"
)

for (field_name in names(amenity_patterns)) {
  set(
    L,
    j = field_name,
    value = str_detect(amenities_text, amenity_patterns[[field_name]])
  )
}

# 1.6 Location
L[, dist_cbd_km := haversine_km(latitude, longitude, CBD_LAT, CBD_LON)]
L[, dist_beach_km := haversine_km(latitude, longitude, BEACH_LAT, BEACH_LON)]
L[, log_dist_cbd := log1p(dist_cbd_km)]
L[, lga := neighbourhood_cleansed]

top_lgas <- names(sort(table(L$lga), decreasing = TRUE))[1:15]
L[, lga_grouped := fifelse(lga %in% top_lgas, lga, "Other")]

distance_correlation <- cor(
  L$dist_cbd_km,
  L$dist_beach_km,
  use = "complete.obs"
)

log_step("CBD/coastal distance correlation: ", round(distance_correlation, 3))

# 1.7 Review fields and outcomes
L[, has_reviews := number_of_reviews > 0]
L[, first_review := as.Date(first_review)]
L[, last_review := as.Date(last_review)]
L[, days_since_last_review := as.numeric(SNAPSHOT_DATE - last_review)]
L[, listing_age_days := as.numeric(SNAPSHOT_DATE - first_review)]
L[, active_90 := !is.na(days_since_last_review) & days_since_last_review <= 90]
L[, active_180 := !is.na(days_since_last_review) & days_since_last_review <= 180]
L[, review_velocity := as.numeric(number_of_reviews_ltm)]
L[, log_review_velocity := log1p(review_velocity)]
L[, log_minimum_nights := log1p(minimum_nights)]

mean_review_score <- mean(
  L$review_scores_rating[L$has_reviews],
  na.rm = TRUE
)

L[, review_scores_filled := fifelse(
  is.na(review_scores_rating),
  mean_review_score,
  as.numeric(review_scores_rating)
)]

if ("estimated_occupancy_l365d" %in% names(L)) {
  setnames(L, "estimated_occupancy_l365d", "ia_occupancy_derived")
}

if ("estimated_revenue_l365d" %in% names(L)) {
  setnames(L, "estimated_revenue_l365d", "ia_revenue_derived")
}

# 1.8 Review-file validation
log_step("Loading reviews")

reviews <- fread(
  reviews_file,
  select = c("listing_id", "id", "date"),
  showProgress = FALSE
)

reviews[, listing_id := as.character(listing_id)]
reviews[, id := as.character(id)]
reviews[, date := as.Date(date)]

reviews_agg <- reviews[
  ,
  .(
    reviews_raw_count = .N,
    first_review_raw = safe_min_date(date),
    last_review_raw = safe_max_date(date)
  ),
  by = listing_id
]

review_validation <- merge(
  L[
    ,
    .(
      listing_id,
      number_of_reviews,
      first_review,
      last_review
    )
  ],
  reviews_agg,
  by = "listing_id",
  all.x = TRUE
)

review_validation[is.na(reviews_raw_count), reviews_raw_count := 0L]
review_validation[, review_count_difference := number_of_reviews - reviews_raw_count]

fwrite(
  review_validation,
  file.path(RESULT_DIR, "review_file_validation.csv")
)

log_step(
  "Reviews: ", nrow(reviews),
  " rows; count differences for ",
  sum(review_validation$review_count_difference != 0, na.rm = TRUE),
  " listings"
)

rm(reviews, reviews_agg)
invisible(gc())

# 1.9 Calendar validation
log_step("Loading calendar")

calendar <- fread(
  calendar_file,
  select = c("listing_id", "date", "available", "minimum_nights"),
  showProgress = FALSE
)

calendar[, listing_id := as.character(listing_id)]
calendar[, date := as.Date(date)]

calendar_monthly <- calendar[
  ,
  .(
    dates = .N,
    open_share = mean(available == "t", na.rm = TRUE)
  ),
  by = .(month = format(date, "%Y-%m"))
][order(month)]

calendar_90 <- calendar[
  date >= SNAPSHOT_DATE & date < SNAPSHOT_DATE + 90,
  .(
    calendar_days = .N,
    unavailable_days = sum(available == "f", na.rm = TRUE),
    calendar_minimum_nights = as.numeric(
      names(sort(table(minimum_nights), decreasing = TRUE))[1]
    )
  ),
  by = listing_id
]

calendar_90[, unavailable_share_90 := unavailable_days / calendar_days]

calendar_validation <- merge(
  L[
    ,
    .(
      listing_id = as.character(id),
      host_scale,
      review_velocity
    )
  ],
  calendar_90,
  by = "listing_id",
  all.x = TRUE
)

calendar_host_check <- calendar_validation[
  !is.na(host_scale),
  .(
    listings = .N,
    mean_unavailable_share_90 = mean(unavailable_share_90, na.rm = TRUE),
    mean_reviews_ltm = mean(review_velocity, na.rm = TRUE)
  ),
  by = host_scale
]

fwrite(
  calendar_monthly,
  file.path(RESULT_DIR, "calendar_availability_by_month.csv")
)

fwrite(
  calendar_host_check,
  file.path(RESULT_DIR, "calendar_proxy_validation_by_host_scale.csv")
)

L <- merge(
  L,
  calendar_90[, .(listing_id, unavailable_share_90)],
  by = "listing_id",
  all.x = TRUE
)

log_step(
  "Calendar loaded for validation only. Unavailable dates are not used as the demand outcome."
)

rm(calendar, calendar_90, calendar_validation)
invisible(gc())

# 1.10 Save cleaned data and audit totals
cleaning_summary <- data.table(
  measure = c(
    "Raw listings",
    "Raw hosts",
    "Raw variables",
    "Variables after initial drops",
    "Missing prices",
    "Price outliers capped",
    "Missing bathrooms after recovery",
    "Listings with recent-review outcome"
  ),
  value = c(
    raw_listing_rows,
    raw_hosts,
    raw_listing_columns,
    variables_after_initial_drops,
    sum(L$price_missing),
    sum(L$price_outlier),
    sum(is.na(L$bathrooms_clean)),
    sum(!is.na(L$review_velocity))
  )
)

fwrite(cleaning_summary, file.path(RESULT_DIR, "cleaning_summary.csv"))
fwrite(L, file.path(CLEAN_DIR, "listings_model_ready.csv"))
writeLines(LOG, file.path(RESULT_DIR, "cleaning_log.txt"))

check_expected(raw_listing_rows, 25728, "Raw listings")
check_expected(raw_hosts, 14113, "Raw hosts")
check_expected(sum(L$price_missing), 6553, "Missing prices")
check_expected(sum(L$price_outlier), 384, "Capped price outliers")
check_expected(sum(is.na(L$bathrooms_clean)), 23, "Missing bathrooms")


# 2. METHOD 1: DESCRIPTIVE ANALYSIS

# 2.1 Table 5.1
summary_row <- function(label, x) {
  data.table(
    characteristic = label,
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    iqr = IQR(x, na.rm = TRUE)
  )
}

summary_statistics <- rbindlist(list(
  summary_row("Listed nightly price (AUD)", L$price_wins),
  summary_row("Reviews in the last 12 months", L$review_velocity),
  summary_row("Bedrooms", L$bedrooms_clean),
  summary_row("Bathrooms", L$bathrooms_clean),
  summary_row("Guest capacity", L$accommodates),
  summary_row("Amenity count", L$amenity_count),
  summary_row("Minimum stay (nights)", L$minimum_nights),
  summary_row("Distance to CBD (km)", L$dist_cbd_km)
))

fwrite(
  summary_statistics,
  file.path(TABLE_DIR, "Table_5_1_summary_statistics.csv")
)

table_5_1 <- gt(summary_statistics) |>
  tab_header(
    title = md("**Table 5.1: Summary Statistics for Key Listing Characteristics**"),
    subtitle = "Greater Melbourne Airbnb listings, 16 June 2026; statistics use available observations for each variable"
  ) |>
  cols_label(
    characteristic = "Characteristic",
    n = "N",
    mean = "Mean",
    sd = "SD",
    median = "Median",
    iqr = "IQR"
  ) |>
  fmt_integer(columns = n) |>
  fmt_currency(
    columns = c(mean, sd, median, iqr),
    rows = characteristic == "Listed nightly price (AUD)",
    currency = "AUD",
    decimals = 0
  ) |>
  fmt_number(
    columns = c(mean, sd, median, iqr),
    rows = characteristic != "Listed nightly price (AUD)",
    decimals = 1
  ) |>
  cols_align(align = "left", columns = characteristic) |>
  tab_source_note(
    source_note = paste0(
      "Price statistics use listings with an observed price. This is a descriptive ",
      "table, so N varies by variable and does not represent either regression sample."
    )
  ) |>
  style_gt_table(font_size = 14)

save_gt_png(
  table_5_1,
  "Table_5_1_summary_statistics.png",
  width = 1500,
  zoom = 2
)

# 2.2 Figure 5.1
lga_summary <- L[
  !is.na(lga) & lga != "",
  .(
    listings = .N,
    median_price = safe_median(price_wins),
    median_reviews_ltm = safe_median(review_velocity)
  ),
  by = lga
]

lga_summary <- lga_summary[
  listings >= 50 & !is.na(median_price) & !is.na(median_reviews_ltm)
]

lga_price_cut <- median(lga_summary$median_price)
lga_review_cut <- median(lga_summary$median_reviews_ltm)

lga_summary[, market_position := fcase(
  median_price >= lga_price_cut & median_reviews_ltm >= lga_review_cut,
  "Higher price and activity",
  median_price >= lga_price_cut & median_reviews_ltm < lga_review_cut,
  "Higher price, lower activity",
  median_price < lga_price_cut & median_reviews_ltm >= lga_review_cut,
  "Lower price, higher activity",
  default = "Below both medians"
)]

lga_summary[, market_position := factor(
  market_position,
  levels = c(
    "Higher price and activity",
    "Higher price, lower activity",
    "Lower price, higher activity",
    "Below both medians"
  )
)]

lga_labels <- c(
  "Yarra Ranges", "Melbourne", "Yarra", "Bayside",
  "Maribyrnong", "Moreland"
)

lga_summary[, label := fifelse(lga %in% lga_labels, lga, "")]

figure_5_1 <- ggplot(
  lga_summary,
  aes(
    x = median_price,
    y = median_reviews_ltm,
    size = listings,
    fill = market_position
  )
) +
  geom_vline(
    xintercept = lga_price_cut,
    linetype = "dashed",
    colour = "#667085",
    linewidth = 0.8
  ) +
  geom_hline(
    yintercept = lga_review_cut,
    linetype = "dashed",
    colour = "#667085",
    linewidth = 0.8
  ) +
  geom_point(shape = 21, colour = "white", stroke = 0.6, alpha = 0.9) +
  geom_text_repel(
    aes(label = label),
    colour = "#222222",
    fontface = "bold",
    size = 4,
    box.padding = 0.35,
    point.padding = 0.2,
    min.segment.length = 0,
    max.overlaps = Inf,
    seed = 30005,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "Higher price and activity" = "#EF5A67",
      "Higher price, lower activity" = "#C33E50",
      "Lower price, higher activity" = "#F49AA0",
      "Below both medians" = "#BFC4CC"
    ),
    name = "Market positioning"
  ) +
  scale_size_area(
    max_size = 15,
    breaks = c(2000, 4000, 6000, 8000),
    labels = comma,
    name = "Listings"
  ) +
  scale_x_continuous(
    labels = label_dollar(prefix = "$", accuracy = 1),
    breaks = seq(100, 400, 100),
    expand = expansion(mult = c(0.04, 0.08))
  ) +
  scale_y_continuous(
    breaks = seq(0, 10, 2),
    expand = expansion(mult = c(0.04, 0.12))
  ) +
  labs(
    title = "Where Price and Guest Activity Stand Out",
    subtitle = "LGA medians; dashed lines show the median across eligible LGAs",
    x = "Median listed nightly price",
    y = "Median reviews in the last 12 months",
    caption = "LGAs with fewer than 50 listings are excluded. Recent reviews proxy realised guest activity, not occupancy."
  ) +
  theme_report +
  theme(
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(colour = "#E6E7E9")
  ) +
  guides(
    fill = guide_legend(order = 1, nrow = 1),
    size = guide_legend(order = 2, nrow = 1)
  )

save_plot(
  figure_5_1,
  "Figure_5_1_lga_price_activity.png",
  width = 12,
  height = 7.5
)

fwrite(lga_summary, file.path(RESULT_DIR, "Figure_5_1_plot_data.csv"))

# 2.3 Figure 5.2
property_profile_summary <- L[
  ,
  .(
    listings = .N,
    listings_with_price = sum(!is.na(price_wins)),
    median_price = safe_median(price_wins),
    median_reviews_ltm = safe_median(review_velocity)
  ),
  by = .(property_group, capacity_segment)
]

property_profile_summary <- property_profile_summary[
  listings >= 20 & listings_with_price >= 20
]

property_levels <- c(
  "Entire apartment", "Entire house", "Entire other", "Private room",
  "Shared room", "Hotel/serviced", "Other"
)

property_profile_summary[, property_group := factor(
  property_group,
  levels = rev(property_levels)
)]

property_profile_summary[, capacity_segment := factor(
  capacity_segment,
  levels = capacity_levels
)]

property_profile_summary[, price_label := paste0("$", round(median_price))]
property_profile_summary[, text_colour := fifelse(
  median_reviews_ltm >= 7,
  "white",
  "#222222"
)]

figure_5_2 <- ggplot(
  property_profile_summary,
  aes(
    x = capacity_segment,
    y = property_group,
    fill = median_reviews_ltm
  )
) +
  geom_tile(colour = "white", linewidth = 1.2) +
  geom_text(
    aes(label = price_label, colour = text_colour),
    size = 4.2,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  scale_fill_gradient(
    low = "#FFF8F8",
    high = "#EA4556",
    limits = c(0, 12),
    breaks = c(0, 3, 6, 9, 12),
    oob = squish,
    name = "Median reviews\n(last 12 months)"
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title = "Property Profiles Reveal the Price-Activity Trade-Off",
    subtitle = "Tile colour shows median recent reviews; labels show median listed nightly price",
    x = "Guest capacity",
    y = NULL,
    caption = "A property profile combines property type and guest capacity. Profiles require at least 20 listings and 20 observed prices."
  ) +
  theme_report +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

save_plot(
  figure_5_2,
  "Figure_5_2_property_profiles.png",
  width = 10,
  height = 6.4
)

fwrite(
  property_profile_summary,
  file.path(RESULT_DIR, "Figure_5_2_plot_data.csv")
)


# 3. METHOD 2: CROSS-SECTIONAL REGRESSION


D <- copy(L)

D[, property_group := factor(
  property_group,
  levels = c(
    "Entire apartment", "Entire house", "Entire other", "Hotel/serviced",
    "Other", "Private room", "Shared room"
  )
)]

D[, host_scale := factor(
  host_scale,
  levels = c("Large (21+)", "Medium (6-20)", "Single", "Small (2-5)")
)]

lga_model_levels <- c(
  "Bayside",
  sort(setdiff(unique(D$lga_grouped), "Bayside"))
)

D[, lga_grouped := factor(lga_grouped, levels = lga_model_levels)]

price_formula <- log_price ~
  property_group + accommodates + bedrooms_clean + bathrooms_clean +
  amenity_count + host_is_superhost + host_scale + review_scores_filled +
  has_reviews + dist_cbd_km + lga_grouped

demand_formula <- log_review_velocity ~
  property_group + accommodates + bedrooms_clean + bathrooms_clean +
  amenity_count + host_scale + log_minimum_nights + dist_cbd_km +
  lga_grouped

model_price <- lm(price_formula, data = D)
model_demand <- lm(demand_formula, data = D)

check_expected(nobs(model_price), 19156, "Price-model sample")
check_expected(nobs(model_demand), 25705, "Demand-model sample")

robust_results <- function(model, model_name) {
  robust_covariance <- vcovHC(model, type = "HC3")
  coefficient_test <- coeftest(model, vcov. = robust_covariance)
  critical_value <- qt(0.975, df.residual(model))

  output <- data.table(
    term = rownames(coefficient_test),
    estimate = coefficient_test[, 1],
    robust_se = coefficient_test[, 2],
    statistic = coefficient_test[, 3],
    p_value = coefficient_test[, 4]
  )

  output[, ci_low := estimate - critical_value * robust_se]
  output[, ci_high := estimate + critical_value * robust_se]
  output[, effect_pct := percent_effect(estimate)]
  output[, effect_ci_low := percent_effect(ci_low)]
  output[, effect_ci_high := percent_effect(ci_high)]
  output[, model := model_name]
  output
}

price_robust <- robust_results(model_price, "Nightly price")
demand_robust <- robust_results(model_demand, "Recent reviews")
all_robust <- rbindlist(list(price_robust, demand_robust), use.names = TRUE)

fwrite(
  price_robust,
  file.path(RESULT_DIR, "price_regression_full_HC3.csv")
)

fwrite(
  demand_robust,
  file.path(RESULT_DIR, "demand_regression_full_HC3.csv")
)

model_summary <- data.table(
  model = c("Nightly price", "Recent reviews"),
  observations = c(nobs(model_price), nobs(model_demand)),
  r_squared = c(
    summary(model_price)$r.squared,
    summary(model_demand)$r.squared
  ),
  adjusted_r_squared = c(
    summary(model_price)$adj.r.squared,
    summary(model_demand)$adj.r.squared
  ),
  residual_standard_error = c(
    summary(model_price)$sigma,
    summary(model_demand)$sigma
  )
)

fwrite(model_summary, file.path(RESULT_DIR, "regression_model_summary.csv"))

# Diagnostics used to justify HC3 standard errors and check collinearity
vif_to_table <- function(model, model_name) {
  vif_result <- car::vif(model)

  if (is.matrix(vif_result)) {
    output <- data.table(
      variable = rownames(vif_result),
      gvif = vif_result[, 1],
      degrees_freedom = vif_result[, 2],
      adjusted_gvif = vif_result[, 3]
    )
  } else {
    output <- data.table(
      variable = names(vif_result),
      gvif = as.numeric(vif_result),
      degrees_freedom = 1,
      adjusted_gvif = sqrt(as.numeric(vif_result))
    )
  }

  output[, model := model_name]
  output
}

vif_results <- rbindlist(list(
  vif_to_table(model_price, "Nightly price"),
  vif_to_table(model_demand, "Recent reviews")
), fill = TRUE)

bp_price <- bptest(model_price)
bp_demand <- bptest(model_demand)

diagnostic_tests <- data.table(
  model = c("Nightly price", "Recent reviews"),
  test = "Breusch-Pagan",
  statistic = c(unname(bp_price$statistic), unname(bp_demand$statistic)),
  degrees_freedom = c(unname(bp_price$parameter), unname(bp_demand$parameter)),
  p_value = c(bp_price$p.value, bp_demand$p.value)
)

fwrite(vif_results, file.path(RESULT_DIR, "regression_vif.csv"))
fwrite(diagnostic_tests, file.path(RESULT_DIR, "regression_diagnostics.csv"))

# 3.1 Figure 5.3
price_plot_terms <- c(
  "property_groupEntire house",
  "bedrooms_clean",
  "accommodates",
  "bathrooms_clean",
  "amenity_count",
  "dist_cbd_km",
  "host_is_superhostTRUE"
)

price_plot_labels <- c(
  "Entire house",
  "Bedrooms",
  "Guest capacity",
  "Bathrooms",
  "Amenities",
  "CBD distance",
  "Superhost"
)

price_plot_data <- price_robust[match(price_plot_terms, term)]

if (any(is.na(price_plot_data$term))) {
  stop("One or more Figure 5.3 coefficients were not found in model_price.")
}

price_plot_data[, label := factor(price_plot_labels, levels = rev(price_plot_labels))]
price_plot_data[, significance := fifelse(
  p_value < 0.05,
  "Statistically significant (p < 0.05)",
  "Not statistically significant"
)]
price_plot_data[, value_label := fifelse(
  abs(effect_pct) < 0.1,
  sprintf("%+.2f%%", effect_pct),
  sprintf("%+.1f%%", effect_pct)
)]
price_plot_data[, text_position := fifelse(
  effect_pct >= 0,
  effect_ci_high + 0.9,
  effect_ci_low - 0.9
)]

figure_5_3 <- ggplot(
  price_plot_data,
  aes(x = label, y = effect_pct)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey40",
    linewidth = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = effect_ci_low,
      ymax = effect_ci_high,
      colour = significance
    ),
    width = 0.15,
    linewidth = 0.85
  ) +
  geom_point(aes(colour = significance), size = 3.3) +
  geom_text(
    aes(y = text_position, label = value_label),
    colour = "#222222",
    fontface = "bold",
    size = 4
  ) +
  coord_flip(clip = "off") +
  scale_colour_manual(
    values = c(
      "Statistically significant (p < 0.05)" = "#FF5A5F",
      "Not statistically significant" = "#9CA3AF"
    ),
    name = NULL
  ) +
  scale_y_continuous(
    breaks = seq(-10, 20, 5),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.08, 0.12))
  ) +
  labs(
    title = "What Is Associated with Higher Airbnb Nightly Prices?",
    subtitle = "Adjusted percentage differences with HC3 robust 95% confidence intervals",
    x = NULL,
    y = "Estimated difference in nightly price (%)",
    caption = "Dots = regression estimates; horizontal lines = 95% confidence intervals.\nDashed line = no estimated difference (0%)."
  ) +
  theme_report +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(15, 45, 15, 15)
  )

save_plot(
  figure_5_3,
  "Figure_5_3_price_regression.png",
  width = 11,
  height = 6.5
)

fwrite(price_plot_data, file.path(RESULT_DIR, "Figure_5_3_plot_data.csv"))

# 3.2 Figure 5.4
demand_plot_terms <- c(
  "property_groupEntire house",
  "bedrooms_clean",
  "bathrooms_clean",
  "amenity_count",
  "accommodates"
)

demand_plot_labels <- c(
  "Entire house (vs. apartment)",
  "Additional bedroom",
  "Additional bathroom",
  "Additional amenity",
  "Additional guest capacity"
)

demand_plot_data <- demand_robust[match(demand_plot_terms, term)]

if (any(is.na(demand_plot_data$term))) {
  stop("One or more Figure 5.4 coefficients were not found in model_demand.")
}

demand_plot_data[, label := factor(
  demand_plot_labels,
  levels = rev(demand_plot_labels)
)]

demand_plot_data[, significance := fifelse(
  p_value < 0.05,
  "Statistically significant (p < 0.05)",
  "Not statistically significant"
)]

demand_plot_data[, value_label := sprintf("%+.1f%%", effect_pct)]
demand_plot_data[, text_position := fifelse(
  effect_pct >= 0,
  effect_ci_high + 0.8,
  effect_ci_low - 0.8
)]
demand_plot_data[, text_alignment := fifelse(effect_pct >= 0, 0, 1)]

figure_5_4 <- ggplot(
  demand_plot_data,
  aes(x = label, y = effect_pct)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey45",
    linewidth = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = effect_ci_low,
      ymax = effect_ci_high,
      colour = significance
    ),
    width = 0.15,
    linewidth = 0.8
  ) +
  geom_point(aes(colour = significance), size = 3.3) +
  geom_text(
    aes(
      y = text_position,
      label = value_label,
      hjust = text_alignment
    ),
    colour = "#222222",
    fontface = "bold",
    size = 4,
    show.legend = FALSE
  ) +
  coord_flip(clip = "off") +
  scale_colour_manual(
    values = c(
      "Statistically significant (p < 0.05)" = "#FF5A5F",
      "Not statistically significant" = "#9CA3AF"
    ),
    name = NULL
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.14, 0.14))
  ) +
  labs(
    title = "What Is Associated with More Airbnb Guest Reviews?",
    subtitle = "Adjusted associations with recent review activity and HC3 robust 95% confidence intervals",
    x = NULL,
    y = "Estimated difference in (1 + annual reviews) (%)",
    caption = "Dots = regression estimates; horizontal lines = 95% confidence intervals.\nDashed line = no estimated difference (0%)."
  ) +
  theme_report +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(15, 45, 15, 15)
  )

save_plot(
  figure_5_4,
  "Figure_5_4_demand_regression.png",
  width = 11,
  height = 6.5
)

fwrite(demand_plot_data, file.path(RESULT_DIR, "Figure_5_4_plot_data.csv"))

# 3.3 Appendix A tables
clean_term_labels <- function(terms) {
  output <- terms
  output <- sub("^property_group", "Property: ", output)
  output <- sub("^host_scale", "Host scale: ", output)
  output <- sub("^lga_grouped", "LGA: ", output)

  replacements <- c(
    "(Intercept)" = "(Intercept)",
    "accommodates" = "Guest capacity",
    "bedrooms_clean" = "Bedrooms",
    "bathrooms_clean" = "Bathrooms",
    "amenity_count" = "Amenity count",
    "host_is_superhostTRUE" = "Host is superhost",
    "review_scores_filled" = "Review score",
    "has_reviewsTRUE" = "Has reviews",
    "log_minimum_nights" = "Log minimum nights",
    "dist_cbd_km" = "Distance to CBD (km)"
  )

  matched <- match(output, names(replacements))
  replace_rows <- !is.na(matched)
  output[replace_rows] <- replacements[matched[replace_rows]]
  output
}

format_regression_table <- function(results) {
  data.frame(
    variable = clean_term_labels(results$term),
    estimate = sprintf("%.4f", results$estimate),
    robust_se = sprintf("%.4f", results$robust_se),
    statistic = sprintf("%.2f", results$statistic),
    p_value = format_p(results$p_value),
    confidence_interval = sprintf(
      "[%.4f, %.4f]",
      results$ci_low,
      results$ci_high
    ),
    check.names = FALSE
  )
}

export_regression_tables <- function(
  results,
  title,
  filename_prefix,
  rows_per_part = 18
) {
  formatted <- format_regression_table(results)
  groups <- split(
    seq_len(nrow(formatted)),
    ceiling(seq_len(nrow(formatted)) / rows_per_part)
  )

  for (part_number in seq_along(groups)) {
    part <- formatted[groups[[part_number]], , drop = FALSE]

    table_object <- gt(part) |>
      tab_header(
        title = md(paste0("**", title, "**")),
        subtitle = paste0(
          "Complete OLS coefficients with HC3 robust standard errors - Part ",
          part_number,
          " of ",
          length(groups)
        )
      ) |>
      cols_label(
        variable = "Variable",
        estimate = "Estimate",
        robust_se = "Robust SE",
        statistic = "t",
        p_value = "p",
        confidence_interval = "95% CI"
      ) |>
      cols_align(align = "left", columns = variable) |>
      cols_align(
        align = "right",
        columns = c(
          estimate,
          robust_se,
          statistic,
          p_value,
          confidence_interval
        )
      ) |>
      style_gt_table(font_size = 13)

    save_gt_png(
      table_object,
      paste0(filename_prefix, "_part", part_number, ".png"),
      width = 1500,
      zoom = 2
    )
  }
}

export_regression_tables(
  price_robust,
  "Nightly Price Regression",
  "Table_A1_price_regression"
)

export_regression_tables(
  demand_robust,
  "Guest Activity Regression",
  "Table_A2_demand_regression"
)


# 4. METHOD 3: PREDICTIVE MODELLING AND MARKET SEGMENTATION


price_entry_predictors <- c(
  "property_group",
  "accommodates",
  "bedrooms_clean",
  "bathrooms_clean",
  "amenity_count",
  "dist_cbd_km",
  "lga_grouped"
)

demand_entry_predictors <- c(
  "property_group",
  "accommodates",
  "bedrooms_clean",
  "bathrooms_clean",
  "amenity_count",
  "log_minimum_nights",
  "dist_cbd_km",
  "lga_grouped"
)

prepare_predictive_data <- function(outcome, predictors) {
  required_fields <- c("id", "host_id", outcome, predictors)

  output <- copy(
    L[
      complete.cases(L[, ..required_fields]),
      ..required_fields
    ]
  )

  output[, host_id := as.character(host_id)]
  output[, property_group := factor(
    property_group,
    levels = c(
      "Entire apartment", "Entire house", "Entire other", "Hotel/serviced",
      "Other", "Private room", "Shared room"
    )
  )]
  output[, lga_grouped := factor(lga_grouped, levels = lga_model_levels)]
  output
}

price_prediction_data <- prepare_predictive_data(
  "log_price",
  price_entry_predictors
)

demand_prediction_data <- prepare_predictive_data(
  "log_review_velocity",
  demand_entry_predictors
)

host_level_split <- function(data, seed = 30005, training_share = 0.80) {
  hosts <- sort(unique(data$host_id))
  set.seed(seed)
  training_host_count <- floor(training_share * length(hosts))
  training_hosts <- sample(hosts, training_host_count, replace = FALSE)

  list(
    train = data[host_id %in% training_hosts],
    test = data[!host_id %in% training_hosts],
    training_hosts = training_hosts,
    testing_hosts = setdiff(hosts, training_hosts)
  )
}

prediction_metrics <- function(actual, predicted) {
  data.table(
    rmse = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
    mae = mean(abs(actual - predicted), na.rm = TRUE),
    r_squared = 1 - (
      sum((actual - predicted)^2, na.rm = TRUE) /
        sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
    )
  )
}

run_predictive_benchmark <- function(
  data,
  outcome,
  predictors,
  split_seed = 30005
) {
  split <- host_level_split(data, seed = split_seed)
  model_formula <- as.formula(
    paste(outcome, "~", paste(predictors, collapse = " + "))
  )

  train_frame <- as.data.frame(split$train)
  test_frame <- as.data.frame(split$test)

  ols_model <- lm(model_formula, data = train_frame)
  ols_prediction <- predict(ols_model, newdata = test_frame)

  rf_model <- ranger(
    formula = model_formula,
    data = train_frame,
    num.trees = 500,
    mtry = max(1, floor(sqrt(length(predictors)))),
    min.node.size = 5,
    importance = "permutation",
    respect.unordered.factors = "order",
    seed = 30005,
    num.threads = 1
  )

  rf_prediction <- predict(rf_model, data = test_frame)$predictions
  actual <- test_frame[[outcome]]

  comparison <- rbindlist(list(
    cbind(
      data.table(model = "Entry-stage OLS benchmark"),
      prediction_metrics(actual, ols_prediction)
    ),
    cbind(
      data.table(model = "Random Forest"),
      prediction_metrics(actual, rf_prediction)
    )
  ))

  selected_model <- comparison[which.min(rmse), model]

  list(
    formula = model_formula,
    split = split,
    ols_model = ols_model,
    rf_model = rf_model,
    comparison = comparison,
    selected_model = selected_model,
    actual = actual,
    ols_prediction = ols_prediction,
    rf_prediction = rf_prediction
  )
}

price_benchmark <- run_predictive_benchmark(
  price_prediction_data,
  "log_price",
  price_entry_predictors
)

demand_benchmark <- run_predictive_benchmark(
  demand_prediction_data,
  "log_review_velocity",
  demand_entry_predictors
)

price_model_comparison <- copy(price_benchmark$comparison)
demand_model_comparison <- copy(demand_benchmark$comparison)

fwrite(
  price_model_comparison,
  file.path(RESULT_DIR, "price_predictive_model_comparison.csv")
)

fwrite(
  demand_model_comparison,
  file.path(RESULT_DIR, "demand_predictive_model_comparison.csv")
)

fwrite(
  data.table(host_id = price_benchmark$split$training_hosts),
  file.path(RESULT_DIR, "price_training_hosts.csv")
)

fwrite(
  data.table(host_id = price_benchmark$split$testing_hosts),
  file.path(RESULT_DIR, "price_testing_hosts.csv")
)

fwrite(
  data.table(host_id = demand_benchmark$split$training_hosts),
  file.path(RESULT_DIR, "demand_training_hosts.csv")
)

fwrite(
  data.table(host_id = demand_benchmark$split$testing_hosts),
  file.path(RESULT_DIR, "demand_testing_hosts.csv")
)

cat("\nPredictive model selection\n")
cat("Price:", price_benchmark$selected_model, "\n")
cat("Guest activity:", demand_benchmark$selected_model, "\n")

if (price_benchmark$selected_model != "Random Forest") {
  warning("The report used Random Forest for price, but OLS has lower test RMSE in this run.")
}

if (demand_benchmark$selected_model != "Random Forest") {
  warning("The report used Random Forest for guest activity, but OLS has lower test RMSE in this run.")
}

demand_importance <- data.table(
  variable = names(demand_benchmark$rf_model$variable.importance),
  importance = as.numeric(demand_benchmark$rf_model$variable.importance)
)[order(-importance)]

fwrite(
  demand_importance,
  file.path(RESULT_DIR, "demand_random_forest_importance.csv")
)

# 4.1 Refit the report models using all eligible observations
price_full_frame <- as.data.frame(price_prediction_data)
demand_full_frame <- as.data.frame(demand_prediction_data)

price_full_formula <- price_benchmark$formula
demand_full_formula <- demand_benchmark$formula

price_full_model_type <- price_benchmark$selected_model
demand_full_model_type <- demand_benchmark$selected_model

if (price_full_model_type == "Random Forest") {
  price_full_model <- ranger(
    formula = price_full_formula,
    data = price_full_frame,
    num.trees = 500,
    mtry = max(1, floor(sqrt(length(price_entry_predictors)))),
    min.node.size = 5,
    importance = "permutation",
    respect.unordered.factors = "order",
    seed = 30005,
    num.threads = 1
  )
} else {
  price_full_model <- lm(price_full_formula, data = price_full_frame)
}

if (demand_full_model_type == "Random Forest") {
  demand_full_model <- ranger(
    formula = demand_full_formula,
    data = demand_full_frame,
    num.trees = 500,
    mtry = max(1, floor(sqrt(length(demand_entry_predictors)))),
    min.node.size = 5,
    importance = "permutation",
    respect.unordered.factors = "order",
    seed = 30005,
    num.threads = 1
  )
} else {
  demand_full_model <- lm(demand_full_formula, data = demand_full_frame)
}

scoring_fields <- unique(c(
  price_entry_predictors,
  demand_entry_predictors,
  "capacity_segment"
))

score_data <- copy(
  L[
    complete.cases(L[, ..scoring_fields])
  ]
)

score_data[, property_group := factor(
  property_group,
  levels = levels(price_prediction_data$property_group)
)]
score_data[, lga_grouped := factor(
  lga_grouped,
  levels = levels(price_prediction_data$lga_grouped)
)]

score_frame <- as.data.frame(score_data)

predict_fitted_model <- function(model, model_type, new_data) {
  if (model_type == "Random Forest") {
    predict(model, data = new_data)$predictions
  } else {
    as.numeric(predict(model, newdata = new_data))
  }
}

score_data[, predicted_log_price := predict_fitted_model(
  price_full_model,
  price_full_model_type,
  score_frame
)]

score_data[, predicted_log_reviews := predict_fitted_model(
  demand_full_model,
  demand_full_model_type,
  score_frame
)]

score_data[, predicted_price := exp(predicted_log_price)]
score_data[, predicted_reviews_ltm := pmax(expm1(predicted_log_reviews), 0)]

model_opportunity <- score_data[
  ,
  .(
    listings = .N,
    listings_with_price = sum(!is.na(price)),
    median_predicted_price = median(predicted_price, na.rm = TRUE),
    median_predicted_log_reviews = median(predicted_log_reviews, na.rm = TRUE),
    median_predicted_reviews = median(predicted_reviews_ltm, na.rm = TRUE)
  ),
  by = .(lga_grouped, property_group, capacity_segment)
]

model_opportunity <- model_opportunity[
  listings >= 20 &
    listings_with_price >= 20 &
    as.character(lga_grouped) != "Other"
]

model_opportunity[
  ,
  comparable_lgas := uniqueN(lga_grouped),
  by = .(property_group, capacity_segment)
]

model_opportunity <- model_opportunity[comparable_lgas >= 3]

model_opportunity[
  ,
  c("profile_price_benchmark", "profile_activity_benchmark") := list(
    median(median_predicted_price, na.rm = TRUE),
    median(median_predicted_log_reviews, na.rm = TRUE)
  ),
  by = .(property_group, capacity_segment)
]

model_opportunity[
  ,
  price_percentile := frank(
    median_predicted_price,
    ties.method = "average"
  ) / .N,
  by = .(property_group, capacity_segment)
]

model_opportunity[
  ,
  activity_percentile := frank(
    median_predicted_log_reviews,
    ties.method = "average"
  ) / .N,
  by = .(property_group, capacity_segment)
]

model_opportunity[, opportunity_score := 100 * (
  price_percentile + activity_percentile
) / 2]

model_opportunity[, strong_predicted_price :=
  median_predicted_price >= profile_price_benchmark]

model_opportunity[, strong_predicted_activity :=
  median_predicted_log_reviews >= profile_activity_benchmark]

model_opportunity[, entry_candidate :=
  strong_predicted_price & strong_predicted_activity]

entry_candidates <- model_opportunity[
  entry_candidate == TRUE
][order(
  -opportunity_score,
  -median_predicted_price,
  -median_predicted_log_reviews
)]

entry_candidate_table <- entry_candidates[
  ,
  .(
    lga = as.character(lga_grouped),
    property_type = as.character(property_group),
    guest_capacity = capacity_segment,
    comparable_listings = listings,
    predicted_nightly_price = round(median_predicted_price, 0),
    predicted_reviews_ltm = round(median_predicted_reviews, 1),
    price_percentile = round(100 * price_percentile, 0),
    activity_percentile = round(100 * activity_percentile, 0),
    opportunity_score = round(opportunity_score, 1)
  )
]

top_10_candidates <- head(entry_candidate_table, 10)

fwrite(
  model_opportunity,
  file.path(RESULT_DIR, "all_predictive_market_segments.csv")
)

fwrite(
  entry_candidate_table,
  file.path(RESULT_DIR, "market_entry_candidates.csv")
)

fwrite(
  top_10_candidates,
  file.path(RESULT_DIR, "top_10_market_entry_candidates.csv")
)

# 4.2 Figure 5.5
display_property_groups <- c(
  "Entire apartment",
  "Entire house",
  "Entire other",
  "Private room"
)

model_plot_data <- model_opportunity[
  as.character(property_group) %in% display_property_groups
]

model_plot_data[, property_group := factor(
  as.character(property_group),
  levels = display_property_groups
)]

model_label_data <- model_plot_data[
  entry_candidate == TRUE
][order(property_group, -opportunity_score)]

model_label_data <- model_label_data[
  ,
  head(.SD, 1),
  by = property_group
]

model_label_data[, label := paste0(
  as.character(lga_grouped),
  "\n",
  capacity_segment
)]

figure_5_5 <- ggplot(
  model_plot_data,
  aes(
    x = median_predicted_reviews,
    y = median_predicted_price,
    size = listings,
    shape = entry_candidate,
    fill = entry_candidate,
    colour = entry_candidate
  )
) +
  geom_point(alpha = 0.9, stroke = 0.8) +
  geom_text_repel(
    data = model_label_data,
    aes(
      x = median_predicted_reviews,
      y = median_predicted_price,
      label = label
    ),
    inherit.aes = FALSE,
    size = 3,
    fontface = "bold",
    colour = "#222222",
    box.padding = 0.5,
    point.padding = 0.4,
    min.segment.length = 0,
    segment.colour = "#BDBDBD",
    max.overlaps = Inf,
    seed = 30005
  ) +
  facet_wrap(~ property_group, scales = "free") +
  scale_shape_manual(
    values = c("FALSE" = 21, "TRUE" = 24),
    labels = c("FALSE" = "No", "TRUE" = "Yes")
  ) +
  scale_fill_manual(
    values = c("FALSE" = "white", "TRUE" = "#FF5A5F"),
    labels = c("FALSE" = "No", "TRUE" = "Yes")
  ) +
  scale_colour_manual(
    values = c("FALSE" = "#777777", "TRUE" = "#FF5A5F"),
    labels = c("FALSE" = "No", "TRUE" = "Yes")
  ) +
  scale_size_continuous(
    range = c(2.5, 11),
    labels = comma,
    name = "Comparable listings"
  ) +
  labs(
    title = "Where Are the Strongest Airbnb Entry Opportunities?",
    subtitle = "Profiles benchmarked against the same property type and guest capacity across LGAs",
    x = "Predicted typical guest activity (reviews, last 12 months)",
    y = "Predicted typical nightly price (AUD)",
    shape = "Meets opportunity criteria",
    fill = "Meets opportunity criteria",
    colour = "Meets opportunity criteria"
  ) +
  theme_report +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(colour = "#E9E9E9", linewidth = 0.6),
    strip.text = element_text(face = "bold", colour = "#222222")
  ) +
  guides(
    fill = "none",
    colour = "none",
    shape = guide_legend(order = 2),
    size = guide_legend(order = 1)
  )

save_plot(
  figure_5_5,
  "Figure_5_5_predictive_opportunity.png",
  width = 11,
  height = 6.2
)

fwrite(model_plot_data, file.path(RESULT_DIR, "Figure_5_5_plot_data.csv"))

# 4.3 Appendix B tables
top_10_display <- copy(top_10_candidates)
top_10_display[, market_segment := paste(
  lga,
  property_type,
  guest_capacity,
  sep = " | "
)]

table_b1_part1_data <- top_10_display[
  ,
  .(
    market_segment,
    listings = format(comparable_listings, big.mark = ",", trim = TRUE),
    nightly_price = paste0("$", predicted_nightly_price),
    reviews_ltm = sprintf("%.1f", predicted_reviews_ltm),
    opportunity_score = sprintf("%.1f", opportunity_score)
  )
]

table_b1_part1 <- gt(table_b1_part1_data) |>
  tab_header(
    title = md("**Market-Entry Candidate Ranking**"),
    subtitle = "Predicted price and recent guest activity - Part 1 of 2"
  ) |>
  cols_label(
    market_segment = "Market segment",
    listings = "Listings",
    nightly_price = "Nightly price",
    reviews_ltm = "Reviews LTM",
    opportunity_score = "Opportunity score"
  ) |>
  cols_align(align = "left", columns = market_segment) |>
  cols_align(
    align = "right",
    columns = c(listings, nightly_price, reviews_ltm, opportunity_score)
  ) |>
  style_gt_table(font_size = 13)

save_gt_png(
  table_b1_part1,
  "Table_B1_market_entry_candidates_part1.png",
  width = 1500,
  zoom = 2
)

table_b1_part2_data <- top_10_display[
  ,
  .(
    market_segment,
    price_percentile = sprintf("%.0f", price_percentile),
    activity_percentile = sprintf("%.0f", activity_percentile),
    opportunity_score = sprintf("%.1f", opportunity_score)
  )
]

table_b1_part2 <- gt(table_b1_part2_data) |>
  tab_header(
    title = md("**Market-Entry Candidate Ranking**"),
    subtitle = "Like-for-like percentile rankings - Part 2 of 2"
  ) |>
  cols_label(
    market_segment = "Market segment",
    price_percentile = "Price percentile",
    activity_percentile = "Activity percentile",
    opportunity_score = "Opportunity score"
  ) |>
  cols_align(align = "left", columns = market_segment) |>
  cols_align(
    align = "right",
    columns = c(price_percentile, activity_percentile, opportunity_score)
  ) |>
  style_gt_table(font_size = 13)

save_gt_png(
  table_b1_part2,
  "Table_B1_market_entry_candidates_part2.png",
  width = 1500,
  zoom = 2
)

table_b2_data <- demand_model_comparison[
  ,
  .(
    model,
    rmse = sprintf("%.4f", rmse),
    mae = sprintf("%.4f", mae),
    test_r_squared = sprintf("%.4f", r_squared)
  )
]

table_b2 <- gt(table_b2_data) |>
  tab_header(
    title = md("**Guest Activity - Model Comparison**"),
    subtitle = "Out-of-sample predictive performance | Host-level 80/20 train-test split"
  ) |>
  cols_label(
    model = "Model",
    rmse = "RMSE",
    mae = "MAE",
    test_r_squared = "Test R2"
  ) |>
  cols_align(align = "left", columns = model) |>
  cols_align(
    align = "right",
    columns = c(rmse, mae, test_r_squared)
  ) |>
  style_gt_table(font_size = 13)

save_gt_png(
  table_b2,
  "Table_B2_demand_model_comparison.png",
  width = 1200,
  zoom = 2
)

# 4.4 Figure 5.6: observed segment screen
segment_source <- copy(L)

segment_source[, property_profile_m3 := fcase(
  property_type %in% c("Entire rental unit", "Entire condo"),
  "Apartment / condo",
  property_type == "Entire serviced apartment",
  "Serviced apartment",
  property_type == "Entire home",
  "House",
  property_type == "Entire townhouse",
  "Townhouse",
  property_type %in% c("Entire guesthouse", "Entire guest suite"),
  "Guesthouse / guest suite",
  property_type %in% c("Entire cottage", "Entire cabin"),
  "Cottage / cabin",
  property_type %in% c("Entire villa", "Entire bungalow"),
  "Villa / bungalow",
  property_type == "Entire loft",
  "Loft",
  property_type == "Entire vacation home",
  "Vacation home",
  default = NA_character_
)]

segment_source <- segment_source[
  !is.na(lga) & lga != "" &
    !is.na(property_profile_m3) &
    !is.na(capacity_segment)
]

segment_summary <- segment_source[
  ,
  .(
    listings = .N,
    listings_with_price = sum(!is.na(price)),
    median_price = safe_median(price),
    median_reviews = safe_median(review_velocity)
  ),
  by = .(lga, property_profile_m3, capacity_segment)
]

segment_summary <- segment_summary[
  listings >= 20 &
    listings_with_price >= 20 &
    !is.na(median_price) &
    !is.na(median_reviews)
]

segment_summary[
  ,
  comparable_lgas := uniqueN(lga),
  by = .(property_profile_m3, capacity_segment)
]

segment_summary <- segment_summary[comparable_lgas >= 3]

segment_summary[
  ,
  c("benchmark_price", "benchmark_reviews") := list(
    median(median_price, na.rm = TRUE),
    median(median_reviews, na.rm = TRUE)
  ),
  by = .(property_profile_m3, capacity_segment)
]

segment_summary[, candidate :=
  median_price >= benchmark_price & median_reviews >= benchmark_reviews]

segment_summary[, price_gap_pct :=
  100 * (median_price / benchmark_price - 1)]

segment_summary[, review_gap_pct := fifelse(
  benchmark_reviews > 0,
  100 * (median_reviews / benchmark_reviews - 1),
  NA_real_
)]

# Join the observed screen to the predictive top 10.
# Only the apartment definitions match exactly across both methods.
predictive_apartment_top10 <- top_10_candidates[
  property_type == "Entire apartment",
  .(
    lga,
    property_profile_m3 = "Apartment / condo",
    capacity_segment = guest_capacity,
    predictive_score = opportunity_score
  )
]

agreed_segments <- merge(
  segment_summary[candidate == TRUE],
  predictive_apartment_top10,
  by = c("lga", "property_profile_m3", "capacity_segment")
)

if (nrow(agreed_segments) != 5) {
  warning(
    "The final report used five segments supported by both methods; this run found ",
    nrow(agreed_segments), "."
  )
}

short_property_names <- c(
  "Apartment / condo" = "Apt",
  "House" = "House",
  "Townhouse" = "Townhouse",
  "Guesthouse / guest suite" = "Guest suite",
  "Serviced apartment" = "Serviced apt",
  "Cottage / cabin" = "Cottage",
  "Villa / bungalow" = "Villa",
  "Loft" = "Loft",
  "Vacation home" = "Holiday home"
)

segment_plot_data <- copy(segment_summary)
segment_plot_data[, price_index := median_price / benchmark_price]
segment_plot_data[, review_index :=
  pmax(median_reviews, 0.25) / pmax(benchmark_reviews, 0.5)]

segment_plot_data[, segment_label := paste0(
  lga,
  ", ",
  short_property_names[property_profile_m3],
  ", ",
  sub(" guests", "", capacity_segment)
)]

segment_plot_data[, outlier := !candidate & (
  price_index >= 1.3 |
    price_index <= 0.8 |
    review_index >= 2.5 |
    review_index <= 0.2
)]

segment_plot_data[, point_type := factor(
  fcase(
    candidate, "Candidate (above both benchmarks)",
    outlier, "Outlier (labelled)",
    default = "Other segment"
  ),
  levels = c(
    "Candidate (above both benchmarks)",
    "Outlier (labelled)",
    "Other segment"
  )
)]

agreement_keys <- paste(
  agreed_segments$lga,
  agreed_segments$property_profile_m3,
  agreed_segments$capacity_segment
)

segment_plot_data[, agreed := paste(
  lga,
  property_profile_m3,
  capacity_segment
) %in% agreement_keys]

quadrant_labels <- data.table(
  x = c(2.05, 2.05, 0.70, 0.70),
  y = c(20, 0.10, 20, 0.10),
  horizontal_justification = c(1, 1, 0, 0),
  label = c(
    "Higher price,\nmore reviews",
    "Higher price,\nfewer reviews",
    "Lower price,\nmore reviews",
    "Lower price,\nfewer reviews"
  )
)

figure_5_6 <- ggplot(
  segment_plot_data,
  aes(x = price_index, y = review_index)
) +
  annotate(
    "rect",
    xmin = 1,
    xmax = Inf,
    ymin = 1,
    ymax = Inf,
    fill = ZONE_A
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey55") +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey55") +
  geom_text(
    data = quadrant_labels,
    aes(
      x = x,
      y = y,
      label = label,
      hjust = horizontal_justification
    ),
    inherit.aes = FALSE,
    size = 3.6,
    fontface = "italic",
    colour = "grey45",
    lineheight = 0.9
  ) +
  geom_point(
    aes(size = listings, colour = point_type),
    alpha = 0.8
  ) +
  geom_point(
    data = segment_plot_data[agreed == TRUE],
    aes(x = price_index, y = review_index),
    inherit.aes = FALSE,
    shape = 21,
    size = 7,
    stroke = 1.4,
    colour = COL_DARK,
    fill = NA
  ) +
  geom_text_repel(
    data = segment_plot_data[point_type != "Other segment"],
    aes(
      label = segment_label,
      colour = point_type,
      fontface = fifelse(agreed, "bold", "plain")
    ),
    size = 3,
    show.legend = FALSE,
    min.segment.length = 0,
    segment.size = 0.25,
    segment.colour = "grey60",
    box.padding = 0.3,
    point.padding = 0.15,
    max.overlaps = Inf,
    force = 2,
    seed = 42
  ) +
  scale_colour_manual(
    values = c(
      "Candidate (above both benchmarks)" = COL_MAIN,
      "Outlier (labelled)" = COL_ACCENT,
      "Other segment" = "grey70"
    ),
    name = NULL
  ) +
  scale_size_area(
    max_size = 12,
    name = "Existing listings",
    labels = comma
  ) +
  scale_x_continuous(
    labels = label_percent(),
    breaks = seq(0.8, 2.0, 0.2),
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_y_log10(
    labels = label_percent(accuracy = 1),
    breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16)
  ) +
  guides(
    colour = guide_legend(nrow = 1, override.aes = list(size = 4)),
    size = guide_legend(nrow = 1)
  ) +
  labs(
    title = "Segment opportunity map",
    subtitle = "Each segment (LGA, property type, guest capacity) relative to the same type and capacity in other LGAs; shaded = above both",
    x = "Median price vs comparable segments (100% = typical)",
    y = "Recent reviews vs comparable segments (log scale)",
    caption = paste0(
      nrow(segment_summary),
      " segments with at least 20 listings and 20 observed prices. All ",
      sum(segment_summary$candidate),
      " candidates are labelled in coral; outliers are labelled in teal.\n",
      "Dark rings mark the ",
      nrow(agreed_segments),
      " segments also ranked in the predictive top 10."
    )
  ) +
  theme_report +
  theme(legend.box = "vertical")

save_plot(
  figure_5_6,
  "Figure_5_6_observed_segment_screen.png",
  width = 13,
  height = 11
)

fwrite(
  segment_summary,
  file.path(RESULT_DIR, "Figure_5_6_all_segments.csv")
)

# Appendix C1
table_c1 <- segment_summary[
  candidate == TRUE
][order(property_profile_m3, capacity_segment, -median_reviews)]

table_c1[, supported_by_both := paste(
  lga,
  property_profile_m3,
  capacity_segment
) %in% agreement_keys]

table_c1_display <- table_c1[
  ,
  .(
    lga,
    type = short_property_names[property_profile_m3],
    guests = sub(" guests", "", capacity_segment),
    listings,
    price = format_dollar(median_price),
    price_benchmark = format_dollar(benchmark_price),
    price_gap = format_effect(price_gap_pct),
    reviews = sprintf("%.1f", median_reviews),
    review_benchmark = sprintf("%.1f", benchmark_reviews),
    review_gap = format_effect(review_gap_pct),
    both_methods = fifelse(supported_by_both, "Yes", "")
  )
]

fwrite(table_c1, file.path(TABLE_DIR, "Table_C1_observed_candidates.csv"))

table_c1_gt <- gt(table_c1_display) |>
  tab_header(
    title = md("**Observed Segment Candidates**"),
    subtitle = paste0(
      sum(segment_summary$candidate),
      " candidates among ",
      nrow(segment_summary),
      " screened segments"
    )
  ) |>
  cols_label(
    lga = "LGA",
    type = "Type",
    guests = "Guests",
    listings = "Listings",
    price = "Price",
    price_benchmark = "Price benchmark",
    price_gap = "Price gap",
    reviews = "Reviews",
    review_benchmark = "Review benchmark",
    review_gap = "Review gap",
    both_methods = "Both methods"
  ) |>
  style_gt_table(font_size = 10)

save_gt_png(
  table_c1_gt,
  "Table_C1_observed_candidates.png",
  width = 2000,
  zoom = 2
)


# 5. KEY-FINDING FIGURES AND SUPPORTING TABLES

# 5.1 Figure 6.1
price_per_guest_summary <- L[
  !is.na(price_wins) &
    property_group %in% c("Entire apartment", "Entire house"),
  .(
    median_price_per_guest = median(price_per_guest),
    listings = .N
  ),
  by = .(property_group, capacity_segment)
]

price_per_guest_summary[, capacity_segment := factor(
  capacity_segment,
  levels = capacity_levels
)]

figure_6_1 <- ggplot(
  price_per_guest_summary,
  aes(
    x = capacity_segment,
    y = median_price_per_guest,
    fill = property_group
  )
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(
    aes(label = dollar(round(median_price_per_guest))),
    position = position_dodge(width = 0.75),
    vjust = -0.4,
    size = 4.2
  ) +
  scale_fill_manual(
    values = c(
      "Entire apartment" = COL_LIGHT,
      "Entire house" = COL_MAIN
    ),
    name = NULL
  ) +
  scale_y_continuous(
    labels = label_dollar(prefix = "$"),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Median nightly price per guest falls as capacity rises",
    subtitle = "Larger properties earn more per night but less per guest accommodated",
    x = "Guest capacity",
    y = "Median price per guest (AUD)",
    caption = "Price per guest = winsorised nightly price / guest capacity. Entire apartments and houses with an observed price."
  ) +
  theme_report +
  theme(panel.grid.major.x = element_blank())

save_plot(
  figure_6_1,
  "Figure_6_1_price_per_guest.png",
  width = 8.5,
  height = 5.2
)

fwrite(
  price_per_guest_summary,
  file.path(RESULT_DIR, "Figure_6_1_plot_data.csv")
)

table_c2 <- dcast(
  price_per_guest_summary,
  capacity_segment ~ property_group,
  value.var = c("median_price_per_guest", "listings")
)

setnames(
  table_c2,
  old = c(
    "median_price_per_guest_Entire apartment",
    "median_price_per_guest_Entire house",
    "listings_Entire apartment",
    "listings_Entire house"
  ),
  new = c(
    "apartment_price_per_guest",
    "house_price_per_guest",
    "apartment_listings",
    "house_listings"
  ),
  skip_absent = TRUE
)

table_c2_display <- table_c2[
  ,
  .(
    guest_capacity = as.character(capacity_segment),
    apartment_price_per_guest = format_dollar(
      apartment_price_per_guest
    ),
    apartment_listings = format(
      apartment_listings,
      big.mark = ",",
      trim = TRUE
    ),
    house_price_per_guest = format_dollar(
      house_price_per_guest
    ),
    house_listings = format(
      house_listings,
      big.mark = ",",
      trim = TRUE
    )
  )
]

fwrite(table_c2, file.path(TABLE_DIR, "Table_C2_price_per_guest.csv"))

table_c2_gt <- gt(table_c2_display) |>
  tab_header(title = md("**Price per Guest by Capacity**")) |>
  cols_label(
    guest_capacity = "Guest capacity",
    apartment_price_per_guest = "Apartment: price per guest",
    apartment_listings = "Apartment: listings",
    house_price_per_guest = "House: price per guest",
    house_listings = "House: listings"
  ) |>
  style_gt_table(font_size = 12)

save_gt_png(
  table_c2_gt,
  "Table_C2_price_per_guest.png",
  width = 1450,
  zoom = 2
)

# 5.2 Figure 6.2
price_label <- "Nightly price (% difference)"
review_label <- "Recent reviews (% difference)"

combined_effects <- rbindlist(list(
  price_robust[
    ,
    .(
      term,
      estimate_pct = effect_pct,
      lower_pct = effect_ci_low,
      upper_pct = effect_ci_high,
      p_value,
      model = price_label
    )
  ],
  demand_robust[
    ,
    .(
      term,
      estimate_pct = effect_pct,
      lower_pct = effect_ci_low,
      upper_pct = effect_ci_high,
      p_value,
      model = review_label
    )
  ]
))

combined_effects[, model := factor(
  model,
  levels = c(price_label, review_label)
)]

combined_effects[, significance := factor(
  fifelse(p_value < 0.05, "Significant (p < 0.05)", "Not significant"),
  levels = c("Significant (p < 0.05)", "Not significant")
)]

host_term_labels <- c(
  "host_is_superhostTRUE" = "Superhost (vs non-superhost)",
  "host_scaleSingle" = "Single listing (1)",
  "host_scaleSmall (2-5)" = "Small host (2-5 listings)",
  "host_scaleMedium (6-20)" = "Medium host (6-20 listings)"
)

host_effects <- combined_effects[term %in% names(host_term_labels)]
host_effects[, label := factor(
  host_term_labels[term],
  levels = rev(unname(host_term_labels))
)]

superhost_note <- data.table(
  label = factor(
    host_term_labels["host_is_superhostTRUE"],
    levels = rev(unname(host_term_labels))
  ),
  model = factor(review_label, levels = c(price_label, review_label)),
  x = 0,
  note = "Not in model: badge requires completed stays"
)

figure_6_2 <- ggplot(
  host_effects,
  aes(x = estimate_pct, y = label)
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
  geom_linerange(
    aes(xmin = lower_pct, xmax = upper_pct),
    colour = "grey45",
    linewidth = 0.9
  ) +
  geom_point(aes(colour = significance), size = 3.4) +
  geom_text(
    aes(label = sprintf("%+.1f%%", estimate_pct)),
    vjust = -1,
    size = 3.9,
    colour = "grey20"
  ) +
  geom_label(
    data = superhost_note,
    aes(x = x, y = label, label = note),
    inherit.aes = FALSE,
    size = 3.2,
    linewidth = 0.2,
    fill = NOTE_FILL,
    colour = COL_DARK,
    hjust = 1.02
  ) +
  scale_colour_manual(
    values = c(
      "Significant (p < 0.05)" = COL_MAIN,
      "Not significant" = "grey65"
    ),
    name = NULL,
    drop = FALSE
  ) +
  scale_x_continuous(expand = expansion(mult = 0.2)) +
  facet_wrap(~ model, scales = "free_x") +
  labs(
    title = "Large operators charge slightly less but attract more guest activity",
    subtitle = "Host scale relative to large operators (21+ listings); 95% robust (HC3) confidence intervals",
    x = NULL,
    y = NULL,
    caption = "Superhost status is excluded from the reviews model because the badge itself requires a minimum number of completed stays."
  ) +
  theme_report

save_plot(
  figure_6_2,
  "Figure_6_2_host_scale.png",
  width = 10,
  height = 4.8
)

fwrite(host_effects, file.path(RESULT_DIR, "Figure_6_2_plot_data.csv"))

table_c3 <- rbindlist(lapply(names(host_term_labels), function(term_name) {
  price_row <- price_robust[term == term_name]
  review_row <- demand_robust[term == term_name]

  data.table(
    variable = unname(host_term_labels[term_name]),
    price_effect = if (nrow(price_row) > 0) format_effect(price_row$effect_pct) else "",
    price_confidence_interval = if (nrow(price_row) > 0) {
      sprintf(
        "[%+.1f%%, %+.1f%%]",
        price_row$effect_ci_low,
        price_row$effect_ci_high
      )
    } else {
      ""
    },
    price_p = if (nrow(price_row) > 0) format_p(price_row$p_value) else "",
    review_effect = if (nrow(review_row) > 0) {
      format_effect(review_row$effect_pct)
    } else {
      "Not in model"
    },
    review_confidence_interval = if (nrow(review_row) > 0) {
      sprintf(
        "[%+.1f%%, %+.1f%%]",
        review_row$effect_ci_low,
        review_row$effect_ci_high
      )
    } else {
      ""
    },
    review_p = if (nrow(review_row) > 0) format_p(review_row$p_value) else ""
  )
}))

fwrite(table_c3, file.path(TABLE_DIR, "Table_C3_host_effects.csv"))

table_c3_gt <- gt(table_c3) |>
  tab_header(title = md("**Superhost Status and Host Scale**")) |>
  cols_label(
    variable = "Variable",
    price_effect = "Price %",
    price_confidence_interval = "Price 95% CI",
    price_p = "Price p",
    review_effect = "Reviews %",
    review_confidence_interval = "Reviews 95% CI",
    review_p = "Reviews p"
  ) |>
  style_gt_table(font_size = 11)

save_gt_png(
  table_c3_gt,
  "Table_C3_host_effects.png",
  width = 1700,
  zoom = 2
)

# 5.3 Appendix C4: adjusted LGA associations
price_lga_effects <- price_robust[
  grepl("^lga_grouped", term),
  .(
    term,
    price_effect = effect_pct,
    price_p = p_value
  )
]

review_lga_effects <- demand_robust[
  grepl("^lga_grouped", term),
  .(
    term,
    review_effect = effect_pct,
    review_p = p_value
  )
]

lga_effects <- merge(
  price_lga_effects,
  review_lga_effects,
  by = "term",
  all = TRUE
)

lga_effects[, lga := sub("^lga_grouped", "", term)]
lga_effects <- lga_effects[order(-review_effect)]

table_c4 <- lga_effects[
  ,
  .(
    lga,
    price_vs_bayside = format_effect(price_effect),
    price_p = format_p(price_p),
    reviews_vs_bayside = format_effect(review_effect),
    reviews_p = format_p(review_p)
  )
]

fwrite(table_c4, file.path(TABLE_DIR, "Table_C4_lga_effects.csv"))

table_c4_gt <- gt(table_c4) |>
  tab_header(
    title = md("**Adjusted LGA Effects Relative to Bayside**")
  ) |>
  cols_label(
    lga = "LGA",
    price_vs_bayside = "Price vs Bayside",
    price_p = "Price p",
    reviews_vs_bayside = "Reviews vs Bayside",
    reviews_p = "Reviews p"
  ) |>
  style_gt_table(font_size = 12)

save_gt_png(
  table_c4_gt,
  "Table_C4_lga_effects.png",
  width = 1400,
  zoom = 2
)

# 5.4 Figure 6.3
distance_data <- L[!is.na(dist_cbd_km) & dist_cbd_km <= 60]
distance_data[, distance_band_midpoint :=
  floor(dist_cbd_km / 2.5) * 2.5 + 1.25]

price_distance_bands <- distance_data[
  !is.na(price_wins),
  .(
    value = median(price_wins),
    listings = .N
  ),
  by = distance_band_midpoint
]
price_distance_bands[, panel := "A. Median nightly price (AUD)"]

review_distance_bands <- distance_data[
  ,
  .(
    value = mean(review_velocity),
    listings = .N
  ),
  by = distance_band_midpoint
]
review_distance_bands[, panel := "B. Mean reviews in the last 12 months"]

distance_bands <- rbindlist(list(
  price_distance_bands,
  review_distance_bands
))

distance_bands <- distance_bands[listings >= 30]

distance_panel_levels <- c(
  "A. Median nightly price (AUD)",
  "B. Mean reviews in the last 12 months"
)

distance_bands[, panel := factor(panel, levels = distance_panel_levels)]
distance_bands[, value_label := fifelse(
  panel == distance_panel_levels[1],
  paste0("$", round(value)),
  sprintf("%.1f", value)
)]

distance_zones <- data.table(
  zone = c(
    "CBD core\n0-2.5 km",
    "Inner suburbs\n2.5-10 km",
    "Middle suburbs\n10-30 km",
    "Outer areas\n30-60 km"
  ),
  xmin = c(0, 2.5, 10, 30),
  xmax = c(2.5, 10, 30, 60),
  fill = c(ZONE_A, ZONE_B, ZONE_A, ZONE_B),
  label_x = c(1.15, 5.6, 18.5, 44)
)

distance_zone_summary <- rbindlist(lapply(
  seq_len(nrow(distance_zones)),
  function(row_number) {
    zone_rows <- distance_data[
      dist_cbd_km >= distance_zones$xmin[row_number] &
        dist_cbd_km < distance_zones$xmax[row_number]
    ]

    data.table(
      zone = gsub("\n", " ", distance_zones$zone[row_number]),
      label_x = distance_zones$label_x[row_number],
      listings = nrow(zone_rows),
      listings_with_price = sum(!is.na(zone_rows$price_wins)),
      median_price = safe_median(zone_rows$price_wins),
      mean_reviews = safe_mean(zone_rows$review_velocity)
    )
  }
))

zone_value_labels <- rbindlist(list(
  distance_zone_summary[
    ,
    .(
      x = label_x,
      y = 372,
      label = paste0("Median\n", dollar(round(median_price))),
      panel = distance_panel_levels[1]
    )
  ],
  distance_zone_summary[
    ,
    .(
      x = label_x,
      y = 25.5,
      label = paste0("Mean\n", sprintf("%.1f", mean_reviews)),
      panel = distance_panel_levels[2]
    )
  ]
))
zone_value_labels[, panel := factor(panel, levels = distance_panel_levels)]

zone_names <- data.table(
  x = distance_zones$label_x,
  y = 408,
  label = distance_zones$zone,
  panel = factor(distance_panel_levels[1], levels = distance_panel_levels)
)

middle_price_range <- price_distance_bands[
  distance_band_midpoint > 15 & distance_band_midpoint < 30,
  range(round(value))
]

distance_notes <- data.table(
  panel = factor(
    c(
      distance_panel_levels[1],
      distance_panel_levels[1],
      distance_panel_levels[2],
      distance_panel_levels[2]
    ),
    levels = distance_panel_levels
  ),
  x = c(18, 46, 6.5, 44),
  y = c(160, 225, 1.5, 2.5),
  label = c(
    sprintf(
      "Lowest prices: $%d-$%d\nin the middle suburbs (15-30 km)",
      middle_price_range[1],
      middle_price_range[2]
    ),
    "Prices recover beyond 30 km:\nouter LGAs such as the Yarra Ranges",
    "Guest activity is highest in the CBD core\nand falls through the middle suburbs",
    "Activity rises again\nbeyond 30 km"
  )
)

distance_arrows <- data.table(
  panel = distance_notes$panel,
  x = c(19, 42, 3, 44),
  y = c(168, 238, 4, 4.3),
  xend = c(21.25, 36.25, 1.3, 41.25),
  yend = c(181, 297, 12, 10)
)

distance_scale_points <- data.table(
  distance_band_midpoint = 0,
  value = c(150, 420, 0, 28),
  panel = factor(
    c(
      distance_panel_levels[1],
      distance_panel_levels[1],
      distance_panel_levels[2],
      distance_panel_levels[2]
    ),
    levels = distance_panel_levels
  )
)

figure_6_3 <- ggplot(
  distance_bands,
  aes(x = distance_band_midpoint, y = value)
) +
  geom_rect(
    data = distance_zones,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
    inherit.aes = FALSE
  ) +
  scale_fill_identity() +
  geom_vline(
    xintercept = c(2.5, 10, 30),
    colour = "grey70",
    linetype = "dotted"
  ) +
  geom_line(colour = COL_MAIN, linewidth = 1.2) +
  geom_point(aes(size = listings), colour = COL_MAIN, alpha = 0.85) +
  geom_text_repel(
    aes(label = value_label),
    size = 3.1,
    colour = "grey15",
    fontface = "bold",
    point.padding = 0.35,
    box.padding = 0.35,
    force = 3,
    segment.size = 0.2,
    segment.colour = "grey70",
    min.segment.length = 0.2,
    max.overlaps = Inf,
    seed = 11
  ) +
  geom_text(
    data = zone_names,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 3.6,
    fontface = "bold",
    colour = "grey25",
    lineheight = 0.9
  ) +
  geom_label(
    data = zone_value_labels,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 3.4,
    linewidth = 0.2,
    fill = "white",
    colour = "grey15",
    lineheight = 0.9
  ) +
  geom_segment(
    data = distance_arrows,
    aes(x = x, y = y, xend = xend, yend = yend),
    inherit.aes = FALSE,
    colour = COL_DARK,
    linewidth = 0.6,
    arrow = arrow(length = grid::unit(0.18, "cm"))
  ) +
  geom_label(
    data = distance_notes,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 3.4,
    linewidth = 0.25,
    fill = NOTE_FILL,
    colour = COL_DARK,
    lineheight = 0.95
  ) +
  geom_blank(data = distance_scale_points) +
  scale_size_area(
    max_size = 7,
    name = "Listings in 2.5 km band",
    labels = comma
  ) +
  scale_x_sqrt(
    breaks = c(0, 2.5, 5, 10, 20, 30, 40, 50, 60),
    labels = c("0", "2.5", "5", "10", "20", "30", "40", "50", "60"),
    expand = expansion(mult = c(0.005, 0.01))
  ) +
  facet_wrap(~ panel, scales = "free_y", ncol = 1) +
  labs(
    title = "Price and guest activity by distance from the CBD",
    subtitle = "Both are higher in the CBD core and beyond 30 km than in the middle suburbs, where both are lowest",
    x = "Distance to CBD (km, square-root scale to show the inner city); each point is a 2.5 km band",
    y = NULL,
    caption = "Unadjusted band averages with no controls for property type, size or host.\nBands with fewer than 30 listings are omitted; listings beyond 60 km are excluded.\nOnce LGA is controlled for, distance has no significant effect on price or guest activity."
  ) +
  theme_report +
  theme(
    panel.grid.major.x = element_blank(),
    strip.text = element_text(hjust = 0)
  )

save_plot(
  figure_6_3,
  "Figure_6_3_cbd_distance.png",
  width = 11,
  height = 11
)

fwrite(distance_bands, file.path(RESULT_DIR, "Figure_6_3_plot_data.csv"))

table_c5a <- distance_zone_summary[
  ,
  .(
    zone,
    listings,
    listings_with_price,
    median_price = round(median_price),
    mean_reviews = round(mean_reviews, 1)
  )
]

price_band_table <- price_distance_bands[
  listings >= 30,
  .(
    distance_band_midpoint,
    listings_with_price = listings,
    median_price = round(value)
  )
]

review_band_table <- review_distance_bands[
  listings >= 30,
  .(
    distance_band_midpoint,
    listings,
    mean_reviews = round(value, 1)
  )
]

table_c5b <- merge(
  review_band_table,
  price_band_table,
  by = "distance_band_midpoint",
  all = TRUE
)[order(distance_band_midpoint)]

table_c5b[, distance_band := sprintf(
  "%.1f-%.1f",
  distance_band_midpoint - 1.25,
  distance_band_midpoint + 1.25
)]

table_c5b <- table_c5b[
  ,
  .(
    distance_band,
    listings,
    listings_with_price,
    median_price,
    mean_reviews
  )
]

fwrite(table_c5a, file.path(TABLE_DIR, "Table_C5a_distance_zones.csv"))
fwrite(table_c5b, file.path(TABLE_DIR, "Table_C5b_distance_bands.csv"))

table_c5a_gt <- gt(table_c5a) |>
  tab_header(title = md("**Price and Guest Activity by Distance Zone**")) |>
  cols_label(
    zone = "Zone",
    listings = "Listings",
    listings_with_price = "Listings with price",
    median_price = "Median price",
    mean_reviews = "Mean reviews (12 months)"
  ) |>
  fmt_currency(columns = median_price, currency = "AUD", decimals = 0) |>
  style_gt_table(font_size = 12)

save_gt_png(
  table_c5a_gt,
  "Table_C5a_distance_zones.png",
  width = 1450,
  zoom = 2
)

table_c5b_gt <- gt(table_c5b) |>
  tab_header(title = md("**Values for Each 2.5 km Distance Band**")) |>
  cols_label(
    distance_band = "Band (km)",
    listings = "Listings",
    listings_with_price = "Listings with price",
    median_price = "Median price",
    mean_reviews = "Mean reviews (12 months)"
  ) |>
  fmt_currency(columns = median_price, currency = "AUD", decimals = 0) |>
  style_gt_table(font_size = 11)

save_gt_png(
  table_c5b_gt,
  "Table_C5b_distance_bands.png",
  width = 1450,
  zoom = 2
)

# 5.5 Figure 6.4: segments supported by both methods
agreed_figure_data <- copy(agreed_segments)

if (nrow(agreed_figure_data) == 0L) {
  stop(
    "No segments were ranked highly by both the observed screen and predictive model.",
    call. = FALSE
  )
}

agreed_figure_data[, segment_label := paste0(
  lga,
  " apartments, ",
  capacity_segment,
  "\n",
  "n = ",
  format(listings, big.mark = ",", trim = TRUE),
  "  |  predictive score ",
  sprintf("%.1f", predictive_score)
)]

agreed_figure_data[, segment_label := factor(
  segment_label,
  levels = segment_label[order(predictive_score)]
)]

agreed_long <- rbindlist(list(
  agreed_figure_data[
    ,
    .(
      segment_label,
      panel = "Median nightly price (AUD)",
      value = median_price,
      benchmark = benchmark_price
    )
  ],
  agreed_figure_data[
    ,
    .(
      segment_label,
      panel = "Median reviews, last 12 months",
      value = median_reviews,
      benchmark = benchmark_reviews
    )
  ]
))

agreed_long[, panel := factor(
  panel,
  levels = c(
    "Median nightly price (AUD)",
    "Median reviews, last 12 months"
  )
)]

agreed_long[, gap_pct := fifelse(
  benchmark > 0,
  100 * (value / benchmark - 1),
  NA_real_
)]

agreed_long[, value_label := fifelse(
  panel == "Median nightly price (AUD)",
  sprintf("$%.0f (%+.0f%%)", value, gap_pct),
  sprintf("%.1f (%+.0f%%)", value, gap_pct)
)]

agreed_long[, benchmark_label := fifelse(
  panel == "Median nightly price (AUD)",
  sprintf("$%.0f", benchmark),
  sprintf("%.1f", benchmark)
)]

figure_6_4 <- ggplot(
  agreed_long,
  aes(y = segment_label)
) +
  geom_segment(
    aes(x = benchmark, xend = value, yend = segment_label),
    colour = "grey75",
    linewidth = 2.2
  ) +
  geom_point(aes(x = benchmark), colour = "grey60", size = 4) +
  geom_point(aes(x = value), colour = COL_MAIN, size = 4.5) +
  geom_text(
    aes(x = value, label = value_label),
    hjust = -0.25,
    size = 3.9,
    colour = "grey15"
  ) +
  geom_text(
    aes(x = benchmark, label = benchmark_label),
    hjust = 1.35,
    size = 3.4,
    colour = "grey45"
  ) +
  facet_wrap(~panel, scales = "free_x") +
  scale_x_continuous(expand = expansion(mult = c(0.25, 0.75))) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0(
      nrow(agreed_figure_data),
      " segments ranked highly by both the segment screen and predictive model"
    ),
    subtitle = paste0(
      "Coral = segment median; grey = median for the same property type ",
      "and capacity in other LGAs"
    ),
    x = NULL,
    y = NULL,
    caption = paste0(
      "Segment medians from the observed screen. Percentages show the gap ",
      "to the like-for-like benchmark.\n",
      "Predictive scores come from Table B1. Yarra is inner city; ",
      "Yarra Ranges is outer east."
    )
  ) +
  theme_report +
  theme(
    axis.text.y = element_text(
      size = 11.5,
      colour = "grey15",
      lineheight = 1.1
    ),
    strip.text = element_text(size = 13, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

save_plot(
  figure_6_4,
  "Figure_6_4_agreed_segments.png",
  width = 11,
  height = 5.6
)

fwrite(
  agreed_long,
  file.path(RESULT_DIR, "Figure_6_4_plot_data.csv")
)

table_c6 <- agreed_figure_data[
  order(-predictive_score),
  .(
    lga,
    guests = sub(" guests", "", capacity_segment),
    listings,
    price = round(median_price),
    price_benchmark = round(benchmark_price),
    price_gap_pct = round(price_gap_pct, 1),
    reviews = round(median_reviews, 1),
    review_benchmark = round(benchmark_reviews, 1),
    review_gap_pct = round(review_gap_pct, 1),
    predictive_score = round(predictive_score, 1)
  )
]

fwrite(table_c6, file.path(TABLE_DIR, "Table_C6_agreed_segments.csv"))

table_c6_gt <- gt(table_c6) |>
  tab_header(
    title = md("**Segments Supported by Both Methods**"),
    subtitle = "Observed segment screen and predictive top 10"
  ) |>
  cols_label(
    lga = "LGA",
    guests = "Guests",
    listings = "Listings",
    price = "Price",
    price_benchmark = "Price benchmark",
    price_gap_pct = "Price gap",
    reviews = "Reviews",
    review_benchmark = "Review benchmark",
    review_gap_pct = "Review gap",
    predictive_score = "Predictive score"
  ) |>
  fmt_currency(
    columns = c(price, price_benchmark),
    currency = "AUD",
    decimals = 0
  ) |>
  fmt_percent(
    columns = c(price_gap_pct, review_gap_pct),
    decimals = 1,
    scale_values = FALSE
  ) |>
  style_gt_table(font_size = 10)

save_gt_png(
  table_c6_gt,
  "Table_C6_agreed_segments.png",
  width = 1900,
  zoom = 2
)



# 6. RUN SUMMARY

expected_figure_files <- c(
  "Figure_5_1_lga_price_activity.png",
  "Figure_5_2_property_profiles.png",
  "Figure_5_3_price_regression.png",
  "Figure_5_4_demand_regression.png",
  "Figure_5_5_predictive_opportunity.png",
  "Figure_5_6_observed_segment_screen.png",
  "Figure_6_1_price_per_guest.png",
  "Figure_6_2_host_scale.png",
  "Figure_6_3_cbd_distance.png",
  "Figure_6_4_agreed_segments.png"
)

expected_table_files <- c(
  "Table_5_1_summary_statistics.png",
  "Table_A1_price_regression_part1.png",
  "Table_A1_price_regression_part2.png",
  "Table_A2_demand_regression_part1.png",
  "Table_A2_demand_regression_part2.png",
  "Table_B1_market_entry_candidates_part1.png",
  "Table_B1_market_entry_candidates_part2.png",
  "Table_B2_demand_model_comparison.png",
  "Table_C1_observed_candidates.png",
  "Table_C2_price_per_guest.png",
  "Table_C3_host_effects.png",
  "Table_C4_lga_effects.png",
  "Table_C5a_distance_zones.png",
  "Table_C5b_distance_bands.png",
  "Table_C6_agreed_segments.png"
)

missing_outputs <- c(
  expected_figure_files[
    !file.exists(file.path(FIGURE_DIR, expected_figure_files))
  ],
  expected_table_files[
    !file.exists(file.path(TABLE_DIR, expected_table_files))
  ]
)

if (length(missing_outputs) > 0L) {
  warning(
    "The following expected report outputs were not created: ",
    paste(missing_outputs, collapse = ", ")
  )
}

output_manifest <- rbindlist(list(
  data.table(
    output_type = "figure",
    file = list.files(FIGURE_DIR, full.names = FALSE),
    folder = FIGURE_DIR
  ),
  data.table(
    output_type = "table",
    file = list.files(TABLE_DIR, full.names = FALSE),
    folder = TABLE_DIR
  ),
  data.table(
    output_type = "model result",
    file = list.files(RESULT_DIR, full.names = FALSE),
    folder = RESULT_DIR
  )
), fill = TRUE)

setorder(output_manifest, output_type, file)
fwrite(output_manifest, file.path(OUTPUT_DIR, "output_manifest.csv"))

writeLines(
  capture.output(sessionInfo()),
  file.path(OUTPUT_DIR, "session_info.txt")
)

cat("\nFinal report analysis complete.\n")
cat("Clean data:", normalizePath(CLEAN_DIR, winslash = "/"), "\n")
cat("Figures:", normalizePath(FIGURE_DIR, winslash = "/"), "\n")
cat("Tables:", normalizePath(TABLE_DIR, winslash = "/"), "\n")
cat("Model results:", normalizePath(RESULT_DIR, winslash = "/"), "\n")






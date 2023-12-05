library(dplyr)
year <- 2023

# date
# Date.
# 
# day_of_week
# Integer. 1 = Monday, 7 = Sunday
# 
# mon_to_fri
# Integer. 1 between Monday and Friday, 0 between Saturday and Sunday
# 
# sat_to_sun
# Integer. 1 between Saturday and Sunday, 0 between Monday and Friday
# 
# public_holiday
# Integer. 1 if public holiday (helligdag), 0 if not public holiday
# 
# freeday
# Integer. 1 if public holiday (helligdag) or sat_to_sun==1, 0 otherwise
# 
# workday
# Integer. 1 if freeday==0, 0 if freeday==1


get_holiday <- function (year, 
                          type = "all", 
                          trapped_days = "exclude", 
                          invert = FALSE) {
  
  ### ARGUMENT CHECKING ---- 
  # Object to store check-results
  checks <- checkmate::makeAssertCollection()
  
  # Perform checks
  datasource <- NVIcheckmate::match_arg(x = type,
                                        choices = c("easter", "holiday", "workday", 
                                                    "trapped", "weekend", "public", "sunday", "saturday",
                                                    "non-moveable", "pentacost"),
                                        several.ok = FALSE,
                                        ignore.case = TRUE,
                                        add = checks)
  
  # Report check-results
  checkmate::reportAssertions(checks)
  
  ### NATIONAL HOLIDAYS ---- 
  # Calculate Easter day
  # reference
  K <- floor(year/100)
  M <- 15 + floor((3 * K + 3)/4) - floor((8 * K + 13)/25)
  S <- 2 - floor((3 * K + 3)/4)
  A <- year %% 19
  D <- (19*A+M) %% 30
  R <- floor((D+A/11)/29)
  OG <- 21 + D - R
  SZ <- 7 - ((year + floor(year/4)+S) %% 7) 
  OE <- 7 - ((OG-SZ) %% 7) 
  
  easterday <- as.Date(paste0(year, "-03-01")) - 1 + OG + OE
  easter <- rep(easterday, 4) + c(-3, -2, 0, 1)
  pentacost <- rep(easterday, 3) + c(39, 49, 50)
  non_moveable <- as.Date(paste0(year, c("-01-01", "-05-01", "-05-17", "-12-25", "-12-26"))) 
  
  ### CATEGORISE INTO HOLIDAYS ---- 
  dates  <- as.data.frame(matrix(data = c(as.Date(paste0(year, "-01-01")):as.Date(paste0(year, "-12-31"))),
                                 dimnames = list(NULL, "date")))
  dates$date <- as.Date(dates$date, origin = "1970-01-01")
  dates <- dates %>%
    dplyr::mutate(weekday = lubridate::wday(.data$date, week_start=1)) %>%
    dplyr::mutate(holiday = dplyr::case_when(.data$weekday %in% c(6, 7) ~ as.character(.data$weekday),
                                             TRUE ~ "0" )) %>%
    dplyr::mutate(holiday = dplyr::case_when(.data$date %in% easter ~ "e",
                                             .data$date %in% pentacost ~ "p",
                                             .data$date %in% non_moveable ~ "n",
                                             TRUE ~ holiday)) %>% 
    dplyr::mutate(behind = dplyr::lag(holiday, 1)) %>%
    dplyr::mutate(ahead = dplyr::lead(holiday, 1)) %>%
    dplyr::mutate(holiday = dplyr::case_when(.data$ahead != 0 & .data$behind != 0 & .data$holiday == 0 ~ "t",
                                             TRUE ~ holiday)) 
  
  if ("easter" %in% type) {
    dates[which(dates$holiday == "e") , "select"] <- 1
  }
  if ("moving" %in% type) {
    dates[which(dates$holiday %in% c("e", "p")) , "select"] <- 1
  }
  if ("public" %in% type) {
    dates[which(dates$holiday %in% c("e", "p", "n")), "select"] <- 1
  }
  if ("sunday" %in% type) {
    dates[which(dates$weekday == 7) , "select"] <- 1
  }
  if ("saturday" %in% type) {
    dates[which(dates$weekday == 6), "select"] <- 1
  }
  if ("work" %in% type) {
    dates[which(dates$holiday %in% c("0")), "select"] <- 1
    if (trapped_days != "exclude") {dates[which(dates$holiday %in% c("t")), "select"] <- 1}
  }
  if ("holiday" %in% type) {
    dates[which(dates$holiday %in% c("e", "p", "n", "6", "7")) , "select"] <- 1
  }
  if ("trapped" %in% type) {
    dates[which(dates$holiday %in% c("t")), "select"] <- 1
  }
  if ("raw" == type) {
    dates[, "select"] <- 1
  }
  
  dates <- subset(dates, dates$select == 1)
  dates <- dates[, c("date", "weekday", "holiday")]
  return(dates)
}

# read_halo.R
library(tidyverse)


# activity_dir <- file.path("data-raw","Amazon Health Data","Activity")
# activity_daily_file <- list.files(activity_dir)[stringr::str_which(list.files(activity_dir),"Activity_DailyData")]
#

#' @title Halo daily data dataframe
#' @description  Return dataframe for all daily data
#' @param pathname path to the Amazon Halo toplevel directory
#' @importFrom magrittr %>%
#' @import readr
#' @import dplyr
#' @export
halo_daily_df <- function(pathname = getwd()){

  activity_dir <- file.path(pathname, "Activity")
  activity_daily_file <- list.files(activity_dir)[stringr::str_which(list.files(activity_dir),"Activity_DailyData")]


  Activity_DailyData <-
    read_csv(
      file.path(activity_dir, activity_daily_file),
      col_types = cols(
        Date = col_date(format = "%Y-%m-%d"),
        Steps = col_number(),
        `Resting Heart Rate (bpm)` = col_number()
      ),
      na = c("No Data")
    ) %>% bind_cols(sourceName = "Amazon Halo")

  return(Activity_DailyData)

}

# halo_daily_df("data-raw/Amazon Health Data")

#' @title Halo raw data dataframe
#' @description  Return dataframe for raw activity data
#' @param pathname path to the Amazon Halo toplevel directory
#' @importFrom magrittr %>%
#' @import readr
#' @import dplyr
#' @export
halo_raw_activity_df <- function(pathname = getwd()){

  activity_dir <- file.path(pathname, "Activity")
  activity_daily_file <- list.files(activity_dir)[stringr::str_which(list.files(activity_dir),"Activity_RawData")]


  Activity_RawData<- read_csv(file.path(activity_dir, activity_daily_file),

                              col_types = cols(`Start Time` = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"),
                                               `End Time` = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"))) %>%
    bind_cols(sourceName = "Amazon Halo")

  return(Activity_RawData)


}

# halo_raw_activity(file.path(getwd(), "data-raw", "Amazon Health Data"))


# heart rate is stored as a sequence of floating point numbers at particular intervals
# the intervals may overlap



#' @title Halo raw heart rate dataframe
#' @description  Return dataframe for raw heart rate data
#' @param pathname path to the Amazon Halo toplevel directory
#' @param sample frequency of rows to include (e.g. 0.2)
#' @importFrom magrittr %>%
#' @import readr
#' @import dplyr
#' @export
halo_heartrate_df <- function(pathname = getwd(), sample = NULL){

  # turn a time interval and a vector of chars into a single dataframe.
  time_df <-function(timeinterval, amounts) {
    amounts_vector <- as.numeric( stringr::str_split(amounts, ", ", simplify = TRUE) )

    df <- tibble(datetime = seq(lubridate::int_start(timeinterval), lubridate::int_end(timeinterval),
                                length.out = length(amounts_vector)),
                 value = amounts_vector)
    df

  }

  activity_df <- halo_raw_activity_df(pathname)
  t <- activity_df %>% dplyr::filter(.data$`Data Type` == "HEARTRATE") %>%
    select(.data$`Start Time`, .data$`End Time`, .data$Amounts) %>%
    transmute(timeinterval = lubridate::interval(.data$`Start Time`, .data$`End Time`),
              amounts = .data$Amounts)
  df <- NULL #tibble(datetime=now(), value = 1)
  for(i in 1:nrow(t)){
    df_new <- time_df(t$timeinterval[i], t$amounts[i])
    df <-bind_rows(df, df_new)

  }
  df %>% bind_cols(sourceName = "Amazon Halo")
}

# halo_heartrate_df(file.path(getwd(), "data-raw", "Amazon Health Data"))
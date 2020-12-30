# read_halo.R
library(tidyverse)


# activity_dir <- file.path("data-raw","Amazon Health Data","Activity")
# activity_daily_file <- list.files(activity_dir)[stringr::str_which(list.files(activity_dir),"Activity_DailyData")]
#


#' @title Halo sleep dataframe
#' @description  Return dataframe for all sleep data
#' @param pathname path to the Amazon Halo toplevel directory
#' @param normalize set to tidy and compatible variable names (default = FALSE)
#' @importFrom magrittr %>%
#' @import readr
#' @import dplyr
#' @export
halo_sleep_sessions_df <- function(pathname = getwd(), normalize = FALSE){

  sleep_dir <- file.path(pathname, "Sleep")
  sleep_sessions_dir <- list.files(sleep_dir)[stringr::str_which(list.files(sleep_dir),"Sleep_Sessions")]




  Sleep_Sessions <-
    read_csv(
      file.path(sleep_dir, sleep_sessions_dir),
      col_types = cols(
        `BedTime` = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"),
        `Sleep Start Time` = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"),
        `Wake Up Time` = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"),
        `Date Of Sleep` = col_date(format = "%Y-%m-%d")
      ),
      na = c("No Data")
    ) %>% mutate(BedTime = lubridate::with_tz(BedTime),
                 `Sleep Start Time` = lubridate::with_tz(`Sleep Start Time`, tzone = Sys.timezone()),
                 `Wake Up Time` = lubridate::with_tz(`Wake Up Time`, tzone = Sys.timezone()),
                 Z = `Total Sleep Duration (msec)` / 1000,
                 REM = `Total REM Sleep Duration (msec)` / 1000,
                 Light = `Total Light Sleep Duration (msec)` / 1000,
                 Deep = `Total Deep Sleep Duration (msec)` / 1000) %>%
    bind_cols(sourceName = "Amazon Halo")



  return (Sleep_Sessions)


}


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
    bind_cols(sourceName = "Amazon Halo") %>%
    mutate(`Start Time` = with_tz(`Start Time`, tzone = Sys.timezone()),
           `End Time` = with_tz(`End Time`, tzone = Sys.timezone()))

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


#' @title Halo raw tone utterances dataframe
#' @description  Return dataframe for tone utterances
#' @param pathname path to the Amazon Halo toplevel directory
#' @importFrom magrittr %>%
#' @import readr
#' @import dplyr
#' @export
halo_tone_utterances_df <- function(pathname = getwd(), sample = NULL){

  tone_dir <- file.path(pathname, "Tone")
  tone_utterances_file <- list.files(tone_dir)[stringr::str_which(list.files(tone_dir),"ToneUtterances")]

  tone_utterances <-
    readr::read_csv(
      file.path(tone_dir, tone_utterances_file),
      col_types = cols(
        `Start Time (UTC)` = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"),
        `Duration (ms)` = col_number()),
      na = c("No Data")) %>%
    transmute(StartTime = lubridate::with_tz(`Start Time (UTC)`, tzone = Sys.timezone()),
              Duration = `Duration (ms)`,
              Bin = factor(`Positivity/Energy Bin`),
              Positivity = Positivity,
              Energy = Energy,
              Descriptors = stringr::str_sub(Descriptors, start = 2, end = -2) %>%
                stringr::str_split(pattern = ",")
    )

  return (tone_utterances)
}


#' @title Halo Body Composition
#' @description  Return fat percentage
#' @param pathname path to the Amazon Halo toplevel directory
#' @importFrom magrittr %>%
#' @import readr
#' @import dplyr
#' @export
halo_body_df <- function(pathname = getwd()){

  body_dir <- file.path(pathname, "Body")
  body_file <- list.files(body_dir)[stringr::str_which(list.files(body_dir),"BodyComposition")]


  body_details <-
    readr::read_csv(
      file.path(body_dir, body_file),

      na = c("No Data")
    ) %>%
    transmute(Timestamp = Timestamp,
              Fat_Percentage = `Visual Body Fat Percentage`)
    bind_cols(sourceName = "Amazon Halo")

  return(body_details)

}



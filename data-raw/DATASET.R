## code to prepare `DATASET` dataset goes here

library(magrittr)
library(stringr)

activity_dir <- file.path("data-raw","Amazon Health Data","Activity")
activity_daily_file <- list.files(activity_dir)[stringr::str_which(list.files(activity_dir),"Activity_DailyData")]

Activity_DailyData <- read_csv(file.path(activity_dir, activity_daily_file),
col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                 Steps = col_number(),
                 `Resting Heart Rate (bpm)` = col_number()),
na = c("No Data")) %>% bind_cols(sourceName = "Amazon Halo")


activity_raw_file <- list.files(activity_dir)[stringr::str_which(list.files(activity_dir),"Activity_RawData")]



Activity_RawData<- read_csv(file.path(activity_dir, activity_raw_file),

                            col_types = cols(`Start Time` = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"),
                                             `End Time` = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"))) %>%
  bind_cols(sourceName = "Amazon Halo")

usethis::use_data(Activity_RawData, overwrite = TRUE)
usethis::use_data(Activity_DailyData, overwrite = TRUE)

rm(list = c("activity_dir", "activity_raw_file", "activity_daily_file"))

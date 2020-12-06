
# amazonhalor

<!-- badges: start -->
<!-- badges: end -->

Read files downloaded from Amazon Halo

## Installation

``` r
devtools::install_github("richardsprague/amazonhalor")
```

## Example

This is a basic example which shows you how to solve a common problem:

Download your Amazon Halo information following the instructions in the app. You will get a directory called `Amazon Health Data`.  

```r
library(amazonhalor)
## basic example code

halo_directory <- getwd() # or other name for the directory containing the Amazon Health Data folder.

```

```r

halo_heartrate_df <- amazonhalor::halo_heartrate_df(halo_directory)
halo_activity_daily_df <- amazonhalor::halo_daily_df(halo_directory)
halo_sleep_df <- amazonhalor::halo_sleep_sessions_df(halo_directory)

```


```r
halo_sleep_df %>%  pivot_longer(cols = c("Z","REM", "Deep", "Light"), names_to = "Phase", values_to = "Duration") %>% 
  transmute(date = `Date Of Sleep`, Phase = factor(Phase), Duration) %>% dplyr::filter(Phase != "Z") %>% 
  ggplot(aes(x=date, y = Duration/3600, fill = Phase)) + geom_col() +
  labs(title = "Sleep Duration", subtitle = "Amazon Halo", y = "Hours", x = "")
```



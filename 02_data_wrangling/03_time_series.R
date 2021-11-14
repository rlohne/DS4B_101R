# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# TIME-BASED MATH ----

library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Date & Lubridate Basics ----

# 1.1 Character vs Date/Datetime

order_date_tbl <- bike_orderlines_tbl %>% 
  select(order_date)

order_date_tbl %>% 
  pull() %>% 
  class()

# 1.2 Date Classes

order_date_tbl %>% 
  mutate(
    order_date_chr = as.character(order_date) 
  ) %>% 
  mutate(order_date_chr2 = order_date_chr %>% str_c(" 00:00:00")) %>% 
  mutate(order_date_date = order_date_chr %>% ymd()) %>% 
  mutate(order_date_dttm = order_date_chr2 %>% ymd_hms())








# 1.3 Lubridate Functions

# Conversion

"06/01/18" %>% 
  mdy()

"06/01/18 12:30:15" %>% 
  mdy_hms()


"January 1, 1985" %>% mdy()


# Extractor

"2011-01-01" %>%  ymd() %>% year()
"2011-01-01" %>%  ymd() %>% month(label = TRUE, abbr = FALSE)
"2011-01-01" %>%  ymd() %>% wday(label = TRUE, abbr = FALSE) 
"2011-01-01" %>%  ymd() %>% day()
"2011-01-01" %>%  ymd() %>% isoweek()


# Helpers
now() # datetime 
today() # date



# Periods & Durations - Add/subract time to/from a date
today() + days(12) # period
today() + ddays(12) # duration 

today() + years(8)
today() + dyears(8)

# Intervals - Calculate time-based distance 

i <- interval(today(), today() + ddays(12)) 

i / ddays(1)

i / dminutes(1)

order_date_tbl %>% 
  mutate(today = today()) %>% 
  mutate(diff_days = interval(order_date, today) / ddays(1))

# 2.0 Time-Based Data Grouping ----

bike_sales_y_tbl <- bike_orderlines_tbl %>% 
  select(order_date, total_price) %>% 
  
  # lubridate
  mutate(order_date = ymd(order_date)) %>% 
  mutate(year = year(order_date)) %>% 
  
  # grouping and summarise
  group_by(year) %>% 
  summarise(
    sales = sum(total_price)
  ) %>% 
  ungroup()

bike_sales_y_tbl

bike_sales_ym_tbl <- bike_orderlines_tbl %>% 
  select(order_date, total_price) %>% 
  
  # lubridate
  mutate(order_date = ymd(order_date)) %>% 
  mutate(
    year  = year(order_date),
    month = month(order_date, label = TRUE, abbr = TRUE)) %>% 
  
  # grouping and summarise
  group_by(year, month) %>% 
  summarise(
    sales = sum(total_price)
  ) %>% 
  ungroup()

bike_sales_ym_tbl

# 3.0 Measuring Change ----

# 3.1 Difference from most recent observation ----





# 3.2 Difference from first observation ----





# 4.0 Cumulative Calculations ----




# 5.0 Rolling Calculations ----



# 6.0 Filtering Date Ranges ---- 




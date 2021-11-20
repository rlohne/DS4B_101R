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



test_list <- list(1, 2)
test_df <- data.frame(
  A = 1:2,
  B = 3:4
)




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

bike_sales_m_tbl <- bike_orderlines_tbl %>% 
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


# Floor date

bike_orderlines_tbl %>% 
  select(order_date, total_price) %>% 
  mutate(year_month = floor_date(order_date, unit = "month")) %>% 
  group_by(year_month) %>% 
  summarise(
    sales = sum(total_price)
  )


# 3.0 Measuring Change ----

# 3.1 Difference from most recent observation ----

bike_sales_y_tbl %>% 
  mutate(
    sales_lag_1 = lag(sales, n = 1)
  ) %>% 
  
  # Handle NA
  mutate(sales_lag_1 = case_when(
    is.na(sales_lag_1) ~ sales,
    TRUE ~ sales_lag_1
  )) %>% 
  
  # Differences and pct differences
  
  mutate(diff_1 = sales - sales_lag_1) %>% 
  mutate(pct_diff_1 = diff_1 / sales_lag_1) %>% 
  mutate(pct_diff_1_chr = scales::percent(pct_diff_1))
  
  
calculate_pct_diff <- function(data, col) {
  
  data %>% 
  
  mutate(
    sales_lag_1 = lag({{col}}, n = 1)
  ) %>% 
    
    # Handle NA
    mutate(sales_lag_1 = case_when(
      is.na(sales_lag_1) ~ {{col}},
      TRUE ~ sales_lag_1
    )) %>% 
    
    # Differences and pct differences
    
    mutate(diff_1 = {{col}} - sales_lag_1) %>% 
    mutate(pct_diff_1 = diff_1 / sales_lag_1) %>% 
    mutate(pct_diff_1_chr = scales::percent(pct_diff_1))
    
  
}

calculate_pct_diff(bike_sales_m_tbl, sales)




# 3.2 Difference from first observation ----

bike_sales_y_tbl %>% 
  mutate(sales_2011 = first(sales)) %>% 
  mutate(diff_2011 = sales - sales_2011) %>% 
  mutate(pct_diff_2011 = diff_2011 / sales_2011) %>% 
  mutate(pct_diff_2011_chr = scales::percent(pct_diff_2011))


bike_sales_m_tbl %>% 
  group_by(year) %>% 
  mutate(sales_jan = first(sales)) %>%
  mutate(
    diff_jan = sales - sales_jan,
    pct_diff_jan = diff_jan / sales_jan,
    pct_diff_jan_chr = scales::percent(pct_diff_jan)) %>% 
  ungroup()


# 4.0 Cumulative Calculations ----

bike_sales_y_tbl %>% 
  mutate(cumulative_sales = cumsum(sales)) %>% 
  mutate(cumulative_sales_pct = cumulative_sales / sum(sales))

bike_sales_m_tbl %>%
  group_by(year) %>% 
  mutate(cumulative_sales = cumsum(sales)) %>% 
  mutate(cumulative_sales_pct = cumulative_sales / sum(sales)) %>% view()




# 5.0 Rolling Calculations ----

bike_sales_m_tbl %>% 
  mutate(roll_mean_3 = rollmean(sales, k = 3, na.pad = TRUE, align = "right", fill = NA)) %>% 
  mutate(roll_mean_6 = rollmean(sales, k = 6, na.pad = TRUE, align = "right", fill = NA))



# 6.0 Filtering Date Ranges ---- 

bike_orderlines_tbl %>% 
  mutate(order_date = ymd(order_date)) %>% 
  filter(order_date %>% between(ymd("2012-01-01"), ymd("2013-12-31")))


bike_orderlines_tbl %>% 
  mutate(order_date = ymd(order_date)) %>%
  filter(year(order_date) %in% c(2012, 2013))


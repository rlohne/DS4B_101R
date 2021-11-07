# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# JUMPSTART: First Sales Analysis----

# 1.0 Load libraries ----

# Work horse packages
library(tidyverse)
library(lubridate)

# theme_tq()
library(tidyquant)

# Excel Files
library(readxl)
library(writexl)




# 2.0 Importing Files ----
bikes_tbl <- read_excel(path = "00_data/bike_sales/data_raw/bikes.xlsx")
bikeshops_tbl <- read_excel(path = "00_data/bike_sales/data_raw/bikeshops.xlsx")
orderlines_tbl <- read_excel(path = "00_data/bike_sales/data_raw/orderlines.xlsx")



# 3.0 Examining Data ----

bikes_tbl
glimpse(bikes_tbl)
bikeshops_tbl
orderlines_tbl


# 4.0 Joining Data ----

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))


# 5.0 Wrangling Data ----

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  
  # Separate description into category.1, category.2 and frame.material
  separate(description, 
           into = c("category.1", "category.2", "frame.material"), 
           sep = " - ",
           remove = TRUE) %>%
  
  # Separate location into city and state
  
  separate(location, 
           into = c("city", "state"),
           sep = ", ", 
           remove = FALSE) %>%
  
  # Add total price (quantity * unit price)
  
  mutate(total.price = quantity * price) %>%
  
  # Reorganize 
  
  select(-...1, -location) %>%
  select(-ends_with(".id")) %>%
  
  bind_cols(bike_orderlines_joined_tbl %>%  select(order.id)) %>%
  
  # Reorder columns
  
  select(contains("date"), starts_with("order"), 
         quantity, price, total.price,
         everything()) %>%
  
  # Renaming
  rename(order_date = order.date) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

  
bike_orderlines_wrangled_tbl %>% glimpse()


# 6.0 Business Insights ----


# 6.1 Sales by Year ----

# Step 1 - Manipulate

sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>% 
  
  # Selecting columns to focus on and adding a year column
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  
  # Summarize sales by year
  group_by(year) %>%
  summarise(
    sales = sum(total_price)
  ) %>%
  ungroup() %>%
  
  # $ Format text
  mutate(sales_text = scales::dollar(sales))



# Step 2 - Visualize

sales_by_year_tbl %>%
  
  # Setup canvas with year (x-axis) and sales (y-axis)
  
  ggplot(aes(
    x = year,
    y = sales
  )) + 
  
  # Geometries
  
  geom_col(fill = "#2C3E50") + 
  geom_label(aes(
    label = sales_text)) + 
  geom_smooth(method = "lm", se = FALSE) +
  
  # Formatting
  theme_tq() + 
  scale_y_continuous(labels = scales::dollar) + 
  labs(
    title = "Revenue by Year",
    subtitle = "Upward trend",
    x = "",
    y = "Revenue"
  )

    

# 6.2 Sales by Year and Category 2 ----


# Step 1 - Manipulate
sales_by_year_cat_2_tbl <- bike_orderlines_wrangled_tbl %>%
  # Select columns and add a year column
  select(order_date, total_price, category_2) %>%
  mutate(year = year(order_date)) %>%
  
  # Group data and summarize year and cat2
  group_by(year, category_2) %>%
  summarise(
    sales = sum(total_price)
  ) %>%
  ungroup() %>%
  # Format  
  mutate(sales_text = scales::dollar(sales))


# Step 2 - Visualize

sales_by_year_cat_2_tbl %>%
  # Set up x, y, fill 
  ggplot(
    aes(
      x = year,
      y = sales,
      fill = category_2
    )) + 
  geom_col() +
  geom_smooth(method = "lm", se = FALSE) + 
  
  # Facetting by category 2
  facet_wrap(~ category_2, ncol = 3, scales = "free_y") + 
  
  # Formatting
  
  theme_tq() + 
  scale_fill_tq() + 
  scale_y_continuous(label = scales::dollar) + 
  labs(
    title = "Revenue by Year and Type of bike",
    subtitle = "Each product category has an upward trend",
    x = "",
    y = "Revenue",
    fill = "Product Secondary Category"
  )
  
  
  


# 7.0 Writing Files ----

fs::dir_create("00_data/bike_sales/data_wrangled_student")

# 7.1 Excel ----
bike_orderlines_wrangled_tbl %>%
  write_xlsx("00_data/bike_sales/data_wrangled_student/bike_orderlines.xlsx")

# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>%
  write_csv("00_data/bike_sales/data_wrangled_student/bike_orderlines.csv")

# 7.3 RDS ----

bike_orderlines_wrangled_tbl %>%
  write_rds("00_data/bike_sales/data_wrangled_student/bike_orderlines.rds")

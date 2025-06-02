library(dplyr)
library(readr)
library(httr)
library(ggplot2)
library(ggtext)
library(stringr)
library(zoo)
library(lubridate)
library(purrr)
library(rlang)
library(scales)
library(sf)
library(tidyr)
library(ggrepel)

# Importing R file with custom functions
source("functions.R")

### Objects Needed ###
# API Keys
con <- file(description = ".api_keys/fred.txt", open = "rt", blocking = F)
FRED_API_KEY <- readLines(con, n = 1)
close(con)

# Vector of the dates of the recessionary periods defined by the NBER from here:
# https://fred.stlouisfed.org/series/USREC
recession_dates_df <- get_fred_data("USREC", FRED_API_KEY)

recession_dates <- filter(recession_dates_df, value == 1L) %>% 
  pull(date)

# Vector of the six-digit NAICS codes for the BLS supersectors:
# https://www.bls.gov/sae/additional-resources/naics-supersectors-for-ces-program.htm
naics_supersectors <- c(
  "000000",
  "110099",
  "230000",
  "300000",
  "400000",
  "510000",
  "510099",
  "540099",
  "600000",
  "700000",
  "810000",
  "900000"
)

# Basic viz caption citation:
base_viz_caption <- "Seasonally adjusted as of MMM. 'YY\nSource: BLS Job Openings and Labor Turnover Survey | Chart: Adrian Nesta"

# State name, abbreviation, and FIPS reference file
us_state_name_abb_fips <- read_csv("./reference_files/us_state_name_abb_fips.csv",
         col_names = T,
         col_types = "cccc")

### Data Collection ###
# Grabbing BLS JOLTS full data file from here:
# https://download.bls.gov/pub/time.series/jt/jt.data.1.AllItems

user_email <- "govdata.decimeter618@passmail.net"

message("Grabbing BLS JOLTS data...")

jolts_raw <- get_bls_data(url = "https://download.bls.gov/pub/time.series/jt/jt.data.1.AllItems",
                        email = user_email)

# Getting accompanying code title reference files from the BLS parent directory
# here: https://download.bls.gov/pub/time.series/jt/
survey_abb <- "jt"
table_names <- c(
  "dataelement",
  "industry",
  "ratelevel",
  "seasonal",
  "sizeclass",
  "state"
)

# Looping through the various code title reference files and fetching
# them as data frames and putting them into a list.
ref_code_df_list <- map(table_names, function(x){
  ref_code_df <- get_bls_ref_code_table(survey_abb = survey_abb, x, email = user_email)
})

# Adding code columns from the `series_id` for each data aspect.
# Code locations from `series_id` referenced from here:
# https://www.bls.gov/help/hlpforma.htm#jt
jolts_raw_codes <- jolts_raw %>% 
  mutate(
    seasonal_code = str_sub(series_id, 3, 3),
    industry_code = str_sub(series_id, 4, 9),
    state_code = str_sub(series_id, 10, 11),
    area_code = str_sub(series_id, 12, 16),
    sizeclass_code = str_sub(series_id, 17, 18),
    dataelement_code = str_sub(series_id, 19, 20),
    ratelevel_code = str_sub(series_id, 21, 21),
    date = base::as.Date(paste0(year, "-", str_sub(period, 2, 3), "-01")),
    value = as.numeric(value)
  )

# Adding JOLTS data frame with code columns to the front of the list of code
# title reference data frames.
full_jt_df_list <- list_flatten(
  list(
    jolts_raw_codes,
    ref_code_df_list
  )
)

# Iteratively joining each code title reference file onto main data frame with
# `dplyr::reduce()`
jolts_full <- reduce(full_jt_df_list, left_join) %>% 
  mutate(dataelement_text = str_to_title(dataelement_text)) %>% 
  rename(
    data_element_text = dataelement_text,
    metric_text = ratelevel_text,
    region_text = state_text,
    seas_adj_text = seasonal_text
  )

### Analysis & Visualizations ###
## Time Series Line Graphs ##
# Hiring Rate, Job Openings Rate, Layoffs & Discharges Rate, Quits Rate, and 
# Unemployed to Job Openings Rate
# Explanation of how rates are calculated here:
# https://www.bls.gov/help/def/jt.htm#rate/level

# Creating list of data frames of measures that I want to create time-series
# line graphs for. Removing erroneously named "Ratio" from UO dataelement_text
# column for "Unemployed to Job Openings Ratio"

##### TODO: Start here renaming variables per specifications in the README.md
hi_jo_ld_qu_uo_df_list <- jolts_full %>% 
  filter(!is.na(date), 
         seasonal_code == "S",
         industry_code == "000000",
         state_code == "00",
         area_code == "00000",
         sizeclass_code == "00",
         dataelement_code %in% c("HI", "JO", "LD", "QU", "UO"),
         ratelevel_code == "R"
  ) %>% 
  select(date, value, data_element_text, metric_text, industry_text, region_text, sizeclass_text) %>%
  mutate(
    data_element_text = str_remove(data_element_text, "\\s+Ratio"),
    val_type_text = "ts_line"
    ) %>%
  group_split(data_element_text, .keep = T)

# Creating list of non-recession averages of all measures in the list of data frames
hi_jo_ld_qu_uo_non_recession_avg_list <- map(hi_jo_ld_qu_uo_df_list, 
                                             ~get_avg_col_val(
                                               .x, recession_dates, value, "exclusive"))

# Creating list of recession averages of all measures in the list of data frames
hi_jo_ld_qu_uo_recession_avg_list <- map(hi_jo_ld_qu_uo_df_list, 
                                             ~get_avg_col_val(
                                               .x, recession_dates, value, "inclusive"))
# Combining these lists
hi_jo_ld_qu_uo_avg_list <- map2(
  hi_jo_ld_qu_uo_non_recession_avg_list, 
  hi_jo_ld_qu_uo_recession_avg_list, ~c(.x, .y))

# Editing data frame list to calculate a trailing three month average column and
# filter dates to only past five years.
hi_jo_ld_qu_uo_ts_df_list <- map(hi_jo_ld_qu_uo_df_list, 
                                 ~make_viz_df_trail_three(.x))

# Writing out each data frame that will be visualized in a time series chart
# to a CSV
walk(hi_jo_ld_qu_uo_ts_df_list, ~econ_csv_write_out(.x, "./data"))

# Making a list of ggplot line charts from the list of time series data frames
hi_jo_ld_qu_uo_ts_viz_list <- map2(
  hi_jo_ld_qu_uo_ts_df_list, 
  hi_jo_ld_qu_uo_avg_list,
  function(x, y) {
    non_rec_avg <- y[1]
    rec_avg <- y[2]
    
    make_ts_line_chart(
      viz_df = x,
      x_col = date,
      y_col_one = value_trail_three,
      second_y_col = T,
      y_col_two = value,
      rec_avg_line = rec_avg,
      non_rec_avg_line = non_rec_avg,
      y_data_type = "number",
      viz_subtitle = "<b style=\"color: #a6cee3\">Latest</b> and <b style = \"color: #1f78b4\">3 month trailing average</b>",
      viz_caption = paste("Average lines for data since Dec. '00.",
                          base_viz_caption)
    )
  }
  )

# Saving list of ggplot line charts to PNGs
walk(hi_jo_ld_qu_uo_ts_viz_list, ~save_chart(.x))

# Creating a data frame of quits to layoffs a.k.a labor leverage ratio
ll_df <- jolts_full %>% 
  filter(!is.na(date), 
         seasonal_code == "S",
         industry_code == "000000",
         state_code == "00",
         area_code == "00000",
         sizeclass_code == "00",
         dataelement_code %in% c("QU", "LD"),
         ratelevel_code == "L"
  ) %>% 
  group_by(date, industry_text, region_text, sizeclass_text) %>% 
  summarize(value = round(value[dataelement_code == "QU"] / value[dataelement_code == "LD"], 2), .groups = "drop") %>% 
  mutate(data_element_text = "Labor Leverage", 
         metric_text = "Ratio",
         val_type_text = "ts_line") %>% 
  relocate(c(value, data_element_text, metric_text), .after = date)

# Getting non-recession average of labor leverage ratio of from entire range of 
# JOLTS data
ll_non_recession_avg <- get_avg_col_val(ll_df, recession_dates, value, "exclusive")

# Getting recession average of labor leverage ratio of from entire range of 
# JOLTS data
ll_recession_avg <- get_avg_col_val(ll_df, recession_dates, value, "inclusive")

ll_avg <- c(ll_non_recession_avg, ll_recession_avg)

# Making data frame with column of trailing three-month average of labor leverage
# ratio
ll_ts_df <- make_viz_df_trail_three(ll_df)

# Writing out labor leverage ratio data frame to CSV
econ_csv_write_out(ll_ts_df, "./data")

# Making labor leverage ratio time series line chart
ll_plt <- make_ts_line_chart(
  viz_df = ll_ts_df,
  x_col = date,
  y_col_one = value_trail_three,
  second_y_col = T,
  y_col_two = value,
  rec_avg_line = ll_avg[2],
  non_rec_avg_line = ll_avg[1],
  y_data_type = "number",
  viz_subtitle = "<b style=\"color: #a6cee3\">Latest</b> and <b style = \"color: #1f78b4\">3 month trailing average</b>",
  viz_caption = paste("Average lines for data since Dec. '00.",
                      base_viz_caption)
)

# Adding black horizontal line at 1 value on y-axis to highlight even
# labor leverage ratio. Values above 1 show more employee optimism about labor
# market, below 1 show more pessimism. 
# https://www.bls.gov/opub/btn/volume-7/measuring-employer-and-employee-confidence-in-the-economy.htm

ll_plt_added_hline <- ll_plt +
  geom_hline(yintercept = 1,
             color = "black",
             linewidth = 1.2,
             linetype = "solid"
  )

# Saving labor leverage ratio time series line chart to PNG
save_chart(ll_plt_added_hline)

## Current 3 Month Trailing Average Bar Graphs ##
# Filtering to only the `naics_supersectors` for the hiring rate, job openings
# rate, layoffs and discharges rate, and quits rate.
hi_jo_ld_qu_naics_ss_trail_three_df <- jolts_full %>% 
  filter(seasonal_code == "S",
         industry_code %in% naics_supersectors,
         state_code == "00",
         area_code == "00000",
         sizeclass_code == "00",
         dataelement_code %in% c("HI", "JO", "LD", "QU"),
         ratelevel_code == "R"
  ) %>% 
  select(date, value, data_element_text, metric_text, industry_text, region_text, sizeclass_text) %>%
  arrange(data_element_text, industry_text, desc(date)) %>% 
  group_by(industry_text, data_element_text) %>% 
  mutate(value = rollmean(value, 3, fill = NA, align = "left"), .after = value) %>% 
  ungroup()

# Calculating the quits to layoff ratio a.k.a the labor leverage ratio for 
# each industry supersector
ll_naics_ss_trail_three_df <- jolts_full %>%
  filter(seasonal_code == "S",
         industry_code %in% naics_supersectors,
         state_code == "00",
         area_code == "00000",
         sizeclass_code == "00",
         dataelement_code %in% c("QU", "LD"),
         ratelevel_code == "L"
  ) %>%
  group_by(industry_text, date, region_text, sizeclass_text) %>%
  summarize(value = round(value[dataelement_code == "QU"] / value[dataelement_code == "LD"], 2), .groups = "drop") %>%
  mutate(data_element_text = "Labor Leverage", 
         metric_text = "Ratio") %>% 
  relocate(c(value, data_element_text, metric_text, industry_text), .after = date) %>% 
  arrange(data_element_text, industry_text, desc(date)) %>% 
  mutate(value = rollmean(value, 3, fill = NA, align = "left"), .after = value)

# Combining labor leverage ratio with other rates
hi_jo_ld_ll_qu_naics_ss_trail_three_df <- bind_rows(hi_jo_ld_qu_naics_ss_trail_three_df, 
                                                 ll_naics_ss_trail_three_df) 

# Filtering for a data frame of only the most recent month of data
hi_jo_ld_ll_qu_naics_ss_cur_df_list <- hi_jo_ld_ll_qu_naics_ss_trail_three_df %>% 
  filter(date == max(date, na.rm = T)) %>% 
  arrange(data_element_text, desc(value)) %>% 
  mutate(val_type_text = "cur_bar") %>% 
  group_split(data_element_text, .keep = T)

# Writing out each most recent date data frame that will be visualized in a bar chart
# to a CSV
walk(hi_jo_ld_ll_qu_naics_ss_cur_df_list, ~econ_csv_write_out(.x, "./data"))

# Making a list of ggplot current bar graphs from the list of data frames
hi_jo_ld_ll_qu_naics_ss_cur_viz_list <- map(
  hi_jo_ld_ll_qu_naics_ss_cur_df_list, 
  ~make_bar_chart(
    viz_df = .x,
    x_col = value,
    y_col = industry_text,
    viz_subtitle = "Trailing 3 month average.",
    viz_caption = base_viz_caption
    )
  )

# Saving the list of ggplot bar charts to PNGs
walk(hi_jo_ld_ll_qu_naics_ss_cur_viz_list, ~save_chart(.x))

## Year-over-year 3 Month Trailing Average Bar Graph ##
hi_jo_ld_ll_qu_naics_ss_yoy_df_list <- hi_jo_ld_ll_qu_naics_ss_trail_three_df %>% 
  filter(date %in% c(max(date, na.rm = T), max(date, na.rm = T) %m-% months(12))) %>% 
  group_by(data_element_text, industry_text, metric_text, region_text, sizeclass_text) %>% 
  summarize(
    value = (value[date == max(date)] / value[date != max(date)]) - 1,
    date = max(date),
    .groups = "drop",
  ) %>% 
  relocate(c(date, value), .before = data_element_text) %>% 
  relocate(metric_text, .after = data_element_text) %>% 
  mutate(val_type_text = "yoy_bar") %>% 
  arrange(data_element_text, desc(value)) %>% 
  group_split(data_element_text, .keep = T)

# Writing out each year-over-year change by NAICS supersector to CSV
walk(hi_jo_ld_ll_qu_naics_ss_yoy_df_list, ~econ_csv_write_out(.x, "./data"))

# Making a list of ggplot YoY bar charts from the list of data frames
hi_jo_ld_ll_qu_naics_ss_yoy_viz_list <- map(
  hi_jo_ld_ll_qu_naics_ss_yoy_df_list, 
  ~make_pct_chg_bar_chart(
    viz_df = .x,
    x_col = value,
    y_col = industry_text,
    viz_subtitle = "Year-over-year change of trailing 3 month average.",
    viz_caption = base_viz_caption
    )
  )

# Saving list of ggplot bar charts to PNGs
walk(hi_jo_ld_ll_qu_naics_ss_yoy_viz_list, ~save_chart(.x))

## Current 3 Month Trailing Average by State Maps ##
# Importing US states shapefile retrieved from the US Census Bureau via tidycensus
# at 500K resolution
us_states_and_dc_shp <- st_read(
  "./shapefiles/us_states_and_dc_with_ak_hi_resized_bottom_shifted.geojson"
) %>% 
  rename(region_text = NAME)

# Filtering to only the 50 US states + DC for hires rate, job opening rate,
# layoffs and discharges rate, and quits rate.
hi_jo_ld_qu_state_trail_three_df <- jolts_full %>% 
  filter(seasonal_code == "S",
         industry_code == "000000",
         state_code %nin% c("00", "MW", "NE", "SO", "WE"),
         area_code == "00000",
         sizeclass_code == "00",
         dataelement_code %in% c("HI", "JO", "LD", "QU"),
         ratelevel_code == "R"
  ) %>% 
  select(date, value, data_element_text, metric_text, industry_text, region_text, sizeclass_text) %>%
  arrange(data_element_text, region_text, desc(date)) %>% 
  group_by(region_text, data_element_text) %>% 
  mutate(value = rollmean(value, 3, fill = NA, align = "left"), .after = value) %>% 
  ungroup()

# Calculating the quits to layoff ratio a.k.a the labor leverage ratio for 
# each state
ll_state_trail_three_df <- jolts_full %>%
  filter(seasonal_code == "S",
         industry_code == "000000",
         state_code %nin% c("00", "MW", "NE", "SO", "WE"),
         area_code == "00000",
         sizeclass_code == "00",
         dataelement_code %in% c("QU", "LD"),
         ratelevel_code == "L"
  ) %>%
  group_by(date, industry_text, region_text, sizeclass_text) %>%
  summarize(value = round(value[dataelement_code == "QU"] / value[dataelement_code == "LD"], 2), .groups = "drop") %>%
  mutate(data_element_text = "Labor Leverage", 
         metric_text = "Ratio") %>% 
  relocate(c(value, data_element_text, metric_text, industry_text), .after = date) %>% 
  arrange(data_element_text, region_text, desc(date)) %>% 
  mutate(value = rollmean(value, 3, fill = NA, align = "left"), .after = value)

# Combining labor leverage ratio with other rates
hi_jo_ld_ll_qu_state_trail_three_df <- bind_rows(hi_jo_ld_qu_state_trail_three_df, 
                                                    ll_state_trail_three_df) 

# Filtering for a data frame of only the most recent month of data
hi_jo_ld_ll_qu_state_cur_df_list <- hi_jo_ld_ll_qu_state_trail_three_df %>% 
  filter(date == max(date, na.rm = T)) %>% 
  arrange(data_element_text, desc(value)) %>% 
  mutate(val_type_text = "cur_map") %>% 
  group_split(data_element_text, .keep = T)

# Writing out each data frame that will be visualized in a map to a CSV
walk(hi_jo_ld_ll_qu_state_cur_df_list, ~econ_csv_write_out(.x, "./data"))

# Creating each choropleth map of the rate for the most recent month of data
# for each dataframe in the list
hi_jo_ld_ll_qu_state_cur_viz_list <- map(
  hi_jo_ld_ll_qu_state_cur_df_list, 
  ~make_cur_map(
    viz_df = .x, 
    shp_df = us_states_and_dc_shp,
    fill_col = value,
    geo_col = geometry,
    viz_subtitle = "Trailing 3 month average.",
    viz_caption = base_viz_caption
    
    )
  )

# Saving the list of ggplot maps to PNGs
walk(hi_jo_ld_ll_qu_state_cur_viz_list, ~save_chart(.x))

## Year-over-year 3 Month Trailing Average by State Maps ##
# Calculating year-over-year change in the trailing three-month average of
# each rate by state.
hi_jo_ld_ll_qu_state_yoy_df_list <- hi_jo_ld_ll_qu_state_trail_three_df %>% 
  filter(date %in% c(max(date, na.rm = T), max(date, na.rm = T) %m-% months(12))) %>% 
  group_by(data_element_text, industry_text, metric_text, region_text, sizeclass_text) %>% 
  summarize(
    value = (value[date == max(date)] / value[date != max(date)]) - 1,
    date = max(date),
    .groups = "drop",
  ) %>% 
  relocate(c(date, value), .before = data_element_text) %>% 
  relocate(metric_text, .after = data_element_text) %>% 
  mutate(val_type_text = "yoy_map") %>% 
  arrange(data_element_text, desc(value)) %>% 
  group_split(data_element_text, .keep = T)

# Writing out each year-over-year change by state to CSV
walk(hi_jo_ld_ll_qu_state_yoy_df_list, ~econ_csv_write_out(.x, "./data"))

# Creating each choropleth map of the year-over-year change in the rate 
# for each dataframe in the list
hi_jo_ld_ll_qu_state_yoy_viz_list <- map(
  hi_jo_ld_ll_qu_state_yoy_df_list, 
  ~make_pct_chg_map(
    viz_df = .x,
    shp_df = us_states_and_dc_shp,
    fill_col = value,
    geo_col = geometry,
    viz_subtitle = "Year-over-year change of trailing 3 month average.",
    viz_caption = base_viz_caption
    )
  )

# Saving list of ggplot maps of yoy change to PNGs
walk(hi_jo_ld_ll_qu_state_yoy_viz_list, ~save_chart(.x))

## Quits Rate & Layoffs and Discharges Rate Scatter Plot ##
# Filtering list of current rate dataframes to only Layoffs and Discharges and Quits
ld_qu_state_cur_df <- hi_jo_ld_ll_qu_state_cur_df_list %>% 
  keep(\(x) unique(x$data_element_text) %in% c("Layoffs And Discharges", "Quits")) %>% 
  list_rbind() %>% 
  mutate(data_element_text = paste(str_remove(data_element_text, "\\s+And\\s+Discharges"), "Rate")) %>% 
  select(date, value, data_element_text, region_text) %>% 
  pivot_wider(names_from = data_element_text, values_from = value) %>% 
  inner_join(us_state_name_abb_fips) %>% 
  mutate(data_element_text = "Labor Leverage",
         metric_text = "Ratio", 
         .after = date) %>% 
  mutate(val_type_text = "cur_scatter")

# Getting most recent state JOLTS data month to use to filter national
# data for the same month to put as reference dashed line in the scatter plot.
latest_state_date <- unique(ld_qu_state_cur_df$date)

# Getting national Layoffs & Discharges & Quits Rates
national_ld_for_states <- hi_jo_ld_qu_uo_ts_df_list %>% 
  keep(\(x) unique(x$data_element_text) == "Layoffs And Discharges") %>% 
  nth(1) %>% 
  filter(date == latest_state_date) %>% 
  pull(value)

national_qu_for_states <- hi_jo_ld_qu_uo_ts_df_list %>% 
  keep(\(x) unique(x$data_element_text) == "Quits") %>% 
  nth(1) %>% 
  filter(date == latest_state_date) %>% 
  pull(value)

# Writing out labor leverage ratio by state scatter plot data frame to CSV
econ_csv_write_out(ld_qu_state_cur_df, "./data")

# Making labor leverage ratio by state scatter plot
ld_qu_state_cur_plt <- make_state_scatter(
  viz_df = ld_qu_state_cur_df,
  x_col = `Quits Rate`,
  y_col = `Layoffs Rate`,
  color_col = region_text,
  label_col = state_abb,
  x_intercept = national_qu_for_states,
  y_intercept = national_ld_for_states,
  viz_title = "Labor Leverage Ratio by State",
  viz_subtitle = "Trailing three-month averages of Quits and Layoffs Rates.",
  viz_caption = paste(
    "Dashed lines are national averages.",
    base_viz_caption
  )
  )

# Saving labor leverage ratio by state scatter plot to PNG
save_chart(ld_qu_state_cur_plt)
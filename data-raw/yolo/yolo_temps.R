library(tidyverse)
library(readxl)
library(lubridate)

# clean water temp data from RST at Knights Landing calfish.org---------------------

y04 <- read_excel('data-raw/yolo/2004 Knights Landing RST Seasonal Catch Data Summary Sheet.xls',
                  skip = 3) %>%
  select(date = Date, water_temp_f = `Water T (F)`) %>%
  filter(!is.na(date))


y05 <- read_excel('data-raw/yolo/2005 Knights Landing RST Seasonal Catch Data Summary Sheet.xlsx',
                  skip = 3) %>%
  select(date = Date, water_temp_f = `Water T (F)`) %>%
  filter(!is.na(date))

y06 <- read_excel('data-raw/yolo/2006 Knights Landing RST Seasonal Catch Data Summary Sheet.xls',
                  skip = 3) %>%
  select(date = Date, water_temp_f = `Water T (F)`) %>%
  filter(!is.na(date))

y07 <- read_excel('data-raw/yolo/2007 Knights Landing RST Seasonal Catch Data Summary Sheet.xlsx',
                  skip = 3) %>%
  select(date = Date, water_temp_f = `Water T (F)`) %>%
  filter(!is.na(date))

y08 <- read_excel('data-raw/yolo/2008 Knights Landing RST Seasonal Catch Data Summary Sheet.xls',
                  skip = 3) %>%
  select(date = Date, water_temp_f = `Water T (F)`) %>%
  filter(!is.na(date))

y09 <- read_excel('data-raw/yolo/2009 Knights Landing RST Seasonal Catch Data Summary Sheet.xls',
                  skip = 3) %>%
  select(date = Date, water_temp_f = `Water T (F)`) %>%
  filter(!is.na(date))

y10 <- read_excel('data-raw/yolo/2010 Knights Landing RST Seasonal Catch Data Summary Sheet.xls',
                  skip = 3) %>%
  select(date = Date, water_temp_f = `Water T (F)`) %>%
  filter(!is.na(date))

y11 <- read_excel('data-raw/yolo/2011 Knights Landing RST Seasonal Catch Data Summary Sheet.xls',
                  skip = 3) %>%
  select(date = Date, water_temp_f = `Water T (F)`) %>%
  filter(!is.na(date))

y12 <- read_excel('data-raw/yolo/2012 Knights Landing RST Seasonal Catch Data Summary.xls',
                  skip = 3) %>%
  select(date = Date, water_temp_f = `Water T (F)`) %>%
  filter(!is.na(date))

y13 <- read_excel('data-raw/yolo/2013 Knights Landing RST Seasonal Catch Data Summary.xls',
                  skip = 3) %>%
  select(date = Date, water_temp_f = `Water T (F)`) %>%
  filter(!is.na(date))

y14 <- read_excel('data-raw/yolo/2014 Knights Landing RST Seasonal Catch Data Summary.xlsx',
                  skip = 5, col_names = FALSE) %>%
  select(date = ...3, water_temp_f = ...14) %>%
  group_by(date) %>%
  summarise(water_temp_f = mean(water_temp_f, na.rm = TRUE)) %>%
  filter(!is.na(water_temp_f))

y15 <- read_excel('data-raw/yolo/2015 Knights Landing RST Seasonal Catch Data Summary.xlsx',
                  skip = 6, col_names = FALSE) %>%
  select(date = ...1, water_temp_f = ...12) %>%
  group_by(date) %>%
  summarise(water_temp_f = mean(water_temp_f, na.rm = TRUE)) %>%
  filter(!is.na(water_temp_f))

y16 <- read_excel('data-raw/yolo/2016 Knights Landing RST Seasonal Catch Data Summary.xlsx',
                  skip = 7, col_names = FALSE) %>%
  select(date = ...1, water_temp_f = ...12) %>%
  group_by(date) %>%
  summarise(water_temp_f = mean(water_temp_f, na.rm = TRUE)) %>%
  filter(!is.na(water_temp_f))

y16 %>%
  ggplot(aes(x = date, y = water_temp_f)) +
  geom_col()

which(letters == 'l')

y17 <- readxl::read_excel('data-raw/yolo/Knights_Landing_RST_Catch_Data_2016-2017.xlsx', skip = 7, col_names = FALSE, na = 'NA') %>%
  select(date = ...1, water_temp_f = ...12) %>%
  group_by(date) %>%
  summarise(water_temp_f = mean(water_temp_f, na.rm = TRUE)) %>%
  filter(!is.na(water_temp_f))


y18 <- readxl::read_excel('data-raw/yolo/Knights_Landing_RST_Catch_Data_2017-2018.xlsx', skip = 7, col_names = FALSE, na = 'N/A') %>%
    select(date = ...1, water_temp_f = ...13) %>%
    group_by(date) %>%
    summarise(water_temp_f = mean(water_temp_f, na.rm = TRUE)) %>%
    filter(!is.na(water_temp_f))

y18 %>%
  ggplot(aes(date, water_temp_f)) +
  geom_col()

bind_rows(y04, y05, y06, y08, y09, y10, y11, y12, y13, y14, y15, y16, y17, y18) %>%
  write_csv('data-raw/yolo/cleaned_yolo_water_temp04_18.csv')

# read in cleaned water temp data
yolo_water_temp <- read_csv('data-raw/yolo/cleaned_yolo_water_temp04_18.csv') %>%
  mutate(water_temp_c = (water_temp_f - 32) * 5/9) %>%
  group_by(month = month(date)) %>%
  summarise(mean_temp_c = mean(water_temp_c, na.rm = TRUE)) %>%
  bind_rows(tibble(month = 7, mean_temp_c = 20)) %>%
  arrange(month)

tibble(
  date = seq(as.Date('1979-01-01'), as.Date('2000-12-31'), by = 'month'),
  watershed = 'Yolo Bypass',
  monthly_mean_temp_c = rep(yolo_water_temp$mean_temp_c, times = 22)
) %>% write_rds('data-raw/yolo/yolo_bypass_water_temp_c.rds')

library(tidyverse)
library(lubridate)
library(CDECRetrieve)
library(rnoaa)
library(forecast)

# Check out what datasets are present for "GES" station
cdec_datasets(station = "GES")

# GES only has duration = "Event" and starts "2009-12-03" so we will retrive "E" starting at "2009-12-03"
ges_station <- cdec_query(station = 'GES', sensor_num = '25', dur_code = 'E',
                      start_date = '2009-12-03', end_date = '2021-02-23')

glimpse(ges_station)

# Group by date to find the mean daily temp, filter weird temps, and mutate to give C values
GES_north_delta <- ges_station %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(mean_temp_f = mean(parameter_value, na.rm = TRUE)) %>%
  filter(mean_temp_f < 100, mean_temp_f > 32) %>% # Filters temp
  mutate(mean_temp_c = (mean_temp_f - 32) * 5/9)

annotate_years <- function(year) {
  annotate("rect", xmin = ymd(paste0(year,'-06-01')), xmax = ymd(paste0(year, '-09-01')),
           ymin = 0, ymax = Inf, alpha = 0.3)
}

ggplot(GES_north_delta, aes(x = date, y = mean_temp_c)) +
  geom_line(color = 'red') +
  annotate_years(2009:2017) +
  geom_hline(yintercept = 18, linetype = 2, size = .2) +
  geom_hline(yintercept = 20, linetype = 2, size = .2) +
  labs(title = '(GES) Sac River Below Georgiana Slough', y = 'monthly mean (°C)') +
  theme_minimal()


# NOAA access token
token = Sys.getenv("token")

# Replaced antioch air temp with lodi
antioch1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00040232', datatypeid = 'TAVG',
                        startdate = '1979-01-01', enddate = '1979-12-31', token = token, limit = 12)
antioch2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00040232', datatypeid = 'TAVG',
                        startdate = '1980-01-01', enddate = '1989-12-31', token = token, limit = 130)
antioch3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00040232', datatypeid = 'TAVG',
                        startdate = '1990-01-01', enddate = '1999-12-31', token = token, limit = 130)
antioch_training <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00040232', datatypeid = 'TAVG',
                        startdate = '2009-12-03', enddate = '2017-12-31', token = token, limit = 130)
antioch4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00040232', datatypeid = 'TAVG',
                        startdate = '2000-01-01', enddate = '2000-12-31', token = token, limit = 130)

# load in air temp values from noaa for use with temperature model to predict water temperature
# Air temp values for training model
# lodi1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045032', datatypeid = 'TAVG',
#                      startdate = '1979-01-01', enddate = '1979-12-31', token = token, limit = 130)
# lodi2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045032', datatypeid = 'TAVG',
#                      startdate = '1980-01-01', enddate = '1989-12-31', token = token, limit = 130)
# lodi3 <-rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045032', datatypeid = 'TAVG',
#                     startdate = '1990-01-01', enddate = '1999-12-31', token = token, limit = 130)
#
# lodi_training <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045032', datatypeid = 'TAVG',
#                              startdate = '2009-01-01', enddate = '2017-12-31', token = token, limit = 130)
# lodi_training2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045032', datatypeid = 'TAVG',
#                               startdate = '2018-01-01', enddate = '2021-02-23', token = token, limit = 130)

lodi1$data %>%
  bind_rows(lodi2$data) %>%
  bind_rows(lodi3$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  ggplot(aes(x = date, y = value)) +
  geom_col()

lodi_training$data %>%
  bind_rows(lodi_training2$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  ggplot(aes(x = date, y = value)) +
  geom_col()

# air_temp_training <- lodi_training$data %>%
#   bind_rows(lodi_training2$data) %>%
#   mutate(date = as_date(ymd_hms(date))) %>%
#   select(date, air_temp_c = value)

air_temp_training <- antioch_training$data %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, air_temp_c = value)

water_temp_training <- GES_north_delta %>%
  group_by(year = year(date), month = month(date)) %>%
  drop_na() %>%
  summarise(water_temp_c = mean(mean_temp_c, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>%
  ungroup() %>%
  select(date, water_temp_c)

# Group air and water training together and plot to see linear relationship
water_temp_training %>%
  left_join(air_temp_training) %>%
  filter(!is.na(air_temp_c)) %>%
  ggplot(aes(x = air_temp_c, y = water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

# Create GES Training
GES_north_delta_training <- water_temp_training %>%
  left_join(air_temp_training) %>%
  filter(!is.na(air_temp_c))


# model for GES
GES_north_delta_temp_model <- lm(water_temp_c ~ air_temp_c, GES_north_delta_training)
summary(GES_north_delta_temp_model)

# Lodi temp data
# GES_north_delta_air_temp <- lodi1$data %>%
#   bind_rows(lodi2$data) %>%
#   bind_rows(lodi3$data) %>%
#   mutate(date = as_date(ymd_hms(date))) %>%
#   select(date, air_temp_c = value) %>%
#   bind_rows(
#     tibble(date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
#            air_temp_c = 0)
#   ) %>%
#   group_by(date) %>%
#   summarise(air_temp_c = max(air_temp_c)) %>%
#   ungroup() %>%
#   mutate(air_temp_c = ifelse(air_temp_c == 0, NA, air_temp_c))

# antioch temp data
GES_north_delta_air_temp <- antioch1$data %>%
  bind_rows(antioch2$data) %>%
  bind_rows(antioch3$data) %>%
  bind_rows(antioch4$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, air_temp_c = value) %>%
  bind_rows(
    tibble(date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
           air_temp_c = 0)
  ) %>%
  group_by(date) %>%
  summarise(air_temp_c = max(air_temp_c)) %>%
  ungroup() %>%
  mutate(air_temp_c = ifelse(air_temp_c == 0, NA, air_temp_c))

# need to imupte values for missing air temperature values between 1980-1999 for predicting water temp----------
GES_ts_north_delta_at <- ts(GES_north_delta_air_temp$air_temp_c, start = c(1979, 1), end = c(2000, 12), frequency = 12)
GES_ts_north_delta_at

na.interp(GES_ts_north_delta_at) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(GES_ts_north_delta_at, series = 'Original')

GES_north_delta_air_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  air_temp_c = as.numeric(na.interp(GES_ts_north_delta_at)))
# use air temp (with impute values) to predict water temp---------
GES_north_delta_air_pred <- predict(GES_north_delta_temp_model, GES_north_delta_air_temp_c)

GES_north_delta_water_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  `GES North Delta` = GES_north_delta_air_pred) %>% glimpse()

GES_north_delta_water_temp_c %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = `GES North Delta`)) +
  geom_hline(yintercept = 18, size = .2) +
  geom_hline(yintercept = 20, size = .2) +
  theme_minimal()

# Write into rds
write_rds(GES_north_delta_water_temp_c, 'data-raw/deltas/ges_north_delta_water_temp_c.rds')

#look at days above 20 degrees
percent_temp_above_tewnty <- nrow(GES_north_delta_water_temp_c %>% filter(`GES North Delta` > 20)) /
  (nrow(GES_north_delta_water_temp_c))

# Compare EMM to GES
diff_from_EMM <- GES_north_delta_water_temp_c %>%
  left_join(north_delta_water_temp_c) %>%
  rename("GES Temp Measures" = `GES North Delta`, "EMM Temp Measures" = `North Delta`) %>%
  gather(spill, temp, -date) %>%
  ggplot(aes(x = date, y = temp, colour = spill)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 20, linetype = 2, size = .2) +
  ggtitle("Emanton and Ges gauge temperature measures from 1980 - 2020") +
  theme_minimal()
diff_from_EMM

emm_and_ges_temps <- GES_north_delta_water_temp_c %>%
  left_join(north_delta_water_temp_c) %>%
  rename("GES Temp Measures" = `GES North Delta`, "EMM Temp Measures" = `North Delta`) %>% glimpse()

write_csv(emm_and_ges_temps, "data-raw/deltas/emm_and_ges_temps.csv")

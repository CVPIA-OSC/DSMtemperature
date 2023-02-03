library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(CDECRetrieve)
library(forcats)
library(forecast)
library(rnoaa)
library(caret)

# This uses CDEC RPN station water temperature data and NOAA NCDC Modesto Airport air temperature data.
# Alternative sources of data are noted in comments and code associated w/ alternative sources is commented out.

# Pull water temperature data --------------------------------------------------

# NMT - new melones temp
nmt <- CDECRetrieve::cdec_query(station = 'NMT', sensor_num = '25', dur_code = 'h',
                                start_date = '2000-09-16', end_date = '2021-12-06')

tdp <- CDECRetrieve::cdec_query(station = 'TDP', sensor_num = '25', dur_code = 'h',
                                start_date = '2000-09-16', end_date = '2021-12-06')

gdc <- CDECRetrieve::cdec_query(station = 'GDC', sensor_num = '25', dur_code = 'h',
                                start_date = '2000-09-16', end_date = '2021-12-06')

nmt_water_temp <- nmt |> janitor::clean_names() |>
  select(datetime, temp_f = parameter_value) |>
  filter(between(temp_f, 10, 100)) |>
  group_by(year= year(datetime), month = month(datetime)) |>
  summarise(mean_water_temp_f = mean(temp_f, na.rm = TRUE)) |>
  ungroup() |>
  mutate(date = ymd(paste(year, month, '01', sep = '-')),
         mean_water_temp_c = (mean_water_temp_f - 32) * 5 / 9) |>
  select(date, mean_water_temp_c) |>
  filter(!is.na(date)) |> glimpse()

tdp_water_temp <- tdp |> janitor::clean_names() |>
  select(datetime, temp_f = parameter_value) |>
  filter(between(temp_f, 10, 100)) |>
  group_by(year= year(datetime), month = month(datetime)) |>
  summarise(mean_water_temp_f = mean(temp_f, na.rm = TRUE)) |>
  ungroup() |>
  mutate(date = ymd(paste(year, month, '01', sep = '-')),
         mean_water_temp_c = (mean_water_temp_f - 32) * 5 / 9) |>
  select(date, mean_water_temp_c) |>
  filter(!is.na(date)) |> glimpse()

gdc_water_temp <- gdc |> janitor::clean_names() |>
  select(datetime, temp_f = parameter_value) |>
  filter(between(temp_f, 10, 100)) |>
  group_by(year= year(datetime), month = month(datetime)) |>
  summarise(mean_water_temp_f = mean(temp_f, na.rm = TRUE)) |>
  ungroup() |>
  mutate(date = ymd(paste(year, month, '01', sep = '-')),
         mean_water_temp_c = (mean_water_temp_f - 32) * 5 / 9) |>
  select(date, mean_water_temp_c) |>
  filter(!is.na(date)) |> glimpse()

nmt_water_temp |>
  ggplot(aes(x = date, y = mean_water_temp_c)) +
  geom_col()

tdp_water_temp |>
  ggplot(aes(x = date, y = mean_water_temp_c)) +
  geom_col()

gdc_water_temp |>
  ggplot(aes(x = date, y = mean_water_temp_c)) +
  geom_col()

# 3 options for water temp gages that are upstream by dams -
# SET this to the one we want to look at
stan_water <- gdc_water_temp


# air temperature data near stream ---------------------------------------------
token <- Sys.getenv("token") #noaa cdo api token saved in .Renviron file

# Currently using
# Modesto airport used because new melones stops collecting data in early 2000 no
# overlap with stream gage data

# model training data 1/2011-12/2021
modesto_airport <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023258',
                               startdate = '2002-01-01', datatypeid = 'TAVG',
                               enddate = '2011-12-31', token = token, limit = 120)


stan_air_temp <- modesto_airport$data %>%
  bind_rows(modesto_airport$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value) %>% glimpse()

stan <- stan_water %>%
  left_join(stan_air_temp) %>%
  filter(!is.na(mean_air_temp_c))

stan %>%
  ggplot(aes(x = mean_air_temp_c, mean_water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_hline(yintercept = 18, alpha = .3) +
  geom_hline(yintercept = 20, alpha = .3)


# linear regression model -------------------------------------------------

stan_model <- lm(mean_water_temp_c ~ mean_air_temp_c, data = stan)
summary(stan_model)

stan_model$coefficients
# air temp thresholds
y <- c(18, 20)
stan_temp_thresholds <- (y - stan_model$coefficients[[1]]) / stan_model$coefficients[[2]]

pred <- broom::augment(stan_model) %>% pull(.fitted)
truth <- stan$mean_water_temp_c
xtab <- table(pred > 18, truth > 18)
xtab <- table(pred > 20, truth > 20)
confusionMatrix(xtab)


# Sticking with modesto airport for now, may want to update later
modesto_airport2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023258',
                                startdate = '1979-01-01', datatypeid = 'TAVG',
                                enddate = '1979-12-31', token = token, limit = 12)

modesto_airport3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023258',
                                startdate = '1980-01-01', datatypeid = 'TAVG',
                                enddate = '1989-12-31', token = token, limit = 120)

modesto_airport4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023258',
                                startdate = '1990-01-01', datatypeid = 'TAVG',
                                enddate = '1999-12-31', token = token, limit = 120)

# Add year 2000
modesto_airport5 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023258',
                                startdate = '2000-01-01', datatypeid = 'TAVG',
                                enddate = '2000-12-31', token = token, limit = 120)

modesto_airport2$data %>%
  bind_rows(modesto_airport3$data) %>%
  bind_rows(modesto_airport4$data) %>%
  bind_rows(modesto_airport5$data) %>%
  mutate(date = ymd_hms(date), year = year(date),
         month = factor(month.abb[month(date)],
                        levels = c(month.abb[10:12], month.abb[1:9]), ordered = TRUE)) %>%
  select(date, month, mean_air_temp_c = value) %>%
  ggplot(aes(x = month, y = mean_air_temp_c)) +
  geom_boxplot() +
  geom_point(alpha = .5, pch = 1, size = 1) +
  labs(y = 'monthly average air temperature (°C)') +
  theme_minimal()

modesto_air_temp <- modesto_airport2$data %>%
  bind_rows(modesto_airport3$data) %>%
  bind_rows(modesto_airport4$data) %>%
  bind_rows(modesto_airport5$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value) %>%
  bind_rows(
    tibble(date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
           mean_air_temp_c = 0)
  ) %>%
  group_by(date) %>%
  summarise(mean_air_temp_c = max(mean_air_temp_c)) %>%
  ungroup() %>%
  mutate(mean_air_temp_c = ifelse(mean_air_temp_c == 0, NA, mean_air_temp_c))


ts_modesto <- ts(modesto_air_temp$mean_air_temp_c, start = c(1979, 1), end = c(2000, 12), frequency = 12)
ts_modesto

na.interp(ts_modesto) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_modesto, series = 'Original')

stanislaus_air_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  mean_air_temp_c = as.numeric(na.interp(ts_modesto)))


modesto_air_temp %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col(fill = 'darkgoldenrod2') +
  geom_col(data = stanislaus_air_temp_c, aes(x = date, y = mean_air_temp_c)) +
  theme_minimal() +
  labs(y = 'monthly mean air temperature (°C)')

stan_pred_water_temp <- predict(stan_model, stanislaus_air_temp_c)

stan_water_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  watershed = 'Stanislaus River',
  monthly_mean_temp_c = stan_pred_water_temp)

stan_water_temp_c %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = monthly_mean_temp_c))

write_rds(stan_water_temp_c, 'data-raw/stanislaus_river/stan_temp_at_dam.rds')


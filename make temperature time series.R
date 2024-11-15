# make temperature time series

rm(list = ls())

library(tidyverse)

biomass_ts <- readRDS("Data/complete_ts.RDS")

sample_days <- unique(biomass_ts$day)
all_days <- 0:max(sample_days)

temps_18_21 <- c(18, 18, 18, 19.5, 21, 21, 21, 19.5)

temperature_ts1 <- tibble(day = all_days,
                          temperature_ts = rep(temps_18_21, length.out = length(all_days)),
                          temperature = "18-21 °C")
temperature_ts2 <- tibble(day = all_days,
                          temperature_ts = rep(temps_18_21+4, length.out = length(all_days)),
                          temperature = "22-25 °C")
temperature_ts3 <- tibble(day = all_days,
                          temperature_ts = rep(temps_18_21+7, length.out = length(all_days)),
                          temperature = "25-28 °C")
temperature_ts <- bind_rows(temperature_ts1, temperature_ts2, temperature_ts3)

saveRDS(temperature_ts, "Data/temperature_ts.RDS")
rm(list = ls())

library(tidyverse)

biomass_ts <- readRDS("Data/complete_ts.RDS")
temperature_ts <- readRDS("Data/temperature_ts.RDS")
biomass_ts <- biomass_ts %>% left_join(temperature_ts)

biomass_ts <- biomass_ts %>% 
  group_by(day, sample_ID, temperature, nutrients) %>%
  summarise(biomass = sum(biomass, na.rm = TRUE))

biomass_ts |> 
  filter(nutrients == "0.75 g/L") |> 
  ggplot(aes(x = day, y = biomass)) +
  geom_line() +
  facet_wrap(~sample_ID) +
  theme_minimal() 

## detrend time series by sample_ID use a variable width smoother
## to remove the trend from the time series
biomass_ts <- biomass_ts %>% 
  group_by(sample_ID) %>%
  arrange(sample_ID, day) %>%
  mutate(biomass_detrended = biomass - ksmooth(x = day, y = biomass, kernel = "normal", bandwidth = 5,
                                               x.points = day)$y)

biomass_ts |>
  filter(nutrients == "0.75 g/L") |> 
  ggplot(aes(x = day, y = biomass)) +
  geom_line() +
  geom_line(aes(y = biomass_detrended), color = "red") +
  facet_wrap(~sample_ID)

ksmooth(x = biomass_ts$day[biomass_ts$sample_ID=="CDLP_Tmp_18_21_Nut0.35_1"],
        y = biomass_ts$biomass[biomass_ts$sample_ID=="CDLP_Tmp_18_21_Nut0.35_1"],
        kernel = "normal", bandwidth = 5, x.points = biomass_ts$day[biomass_ts$sample_ID=="CDLP_Tmp_18_21_Nut0.35_1"])


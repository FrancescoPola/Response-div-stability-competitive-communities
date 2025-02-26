rm(list = ls())

library(tidyverse)
library(broom)
library(patchwork)

## bring in and merge the data
biomass_ts <- readRDS("Data/complete_ts.RDS")
temperature_ts <- readRDS("Data/temperature_ts.RDS")
complete_aggr <- readRDS("Data/complete_aggr.RDS")

## get some rolling sums of temperatures
temperature_ts <- temperature_ts |> 
  group_by(temperature) |> 
  mutate(temperature_ts_rollsum = zoo::rollsumr(temperature_ts, 3, fill = NA))
biomass_ts <- biomass_ts %>% left_join(temperature_ts)

## calculate total biomass
biomass_ts <- biomass_ts %>% 
  group_by(day, sample_ID, temperature, nutrients, temperature_ts, temperature_ts_rollsum) %>%
  summarise(biomass = sum(biomass, na.rm = TRUE))

## detrend time series by sample_ID use a variable width smoother
biomass_ts <- biomass_ts %>% 
  group_by(sample_ID) %>%
  arrange(sample_ID, day) %>%
  mutate(biomass_detrended = biomass - ksmooth(x = day, y = biomass, kernel = "normal", bandwidth = 5,
                                               x.points = day)$y)


## calculate the temperature sensitivity of total biomass
temp_sens <- biomass_ts |> 
  nest(data = c(day, temperature_ts, temperature_ts_rollsum, biomass, biomass_detrended)) |>
  mutate(model = map(data, ~ lm(biomass ~ temperature_ts_rollsum, data = .))) |> 
  mutate(tidy_model = map(model, tidy)) %>%
  unnest(tidy_model) |> 
  filter(term == "temperature_ts_rollsum")
temp_sens_to_merge_rs <- temp_sens %>%
  select(sample_ID,
         temperature_sensitivity_rs = estimate)


## merge the temperature sensitivity with the complete_aggr data
complete_aggr <- complete_aggr %>%
  left_join(temp_sens_to_merge_rs) 


## plot some time series

## plot the temperature time series
temperature_ts |> 
  ggplot(aes(x = day, y = temperature_ts, col = temperature)) +
  geom_line()

## raw biomass data
biomass_ts |> 
  filter(nutrients == "0.75 g/L") |> 
  ggplot(aes(x = day, y = biomass)) +
  geom_line() +
  facet_wrap(~sample_ID) +
  theme_minimal() 
## detrended biomass data
biomass_ts |>
  filter(nutrients == "0.75 g/L") |> 
  ggplot(aes(x = day, y = biomass)) +
  geom_line() +
  geom_line(aes(y = biomass_detrended), color = "red") +
  facet_wrap(~sample_ID)

## detrended biomass data
biomass_ts |>
  filter(nutrients == "0.75 g/L") |> 
  ggplot(aes(x = day, y = biomass)) +
  geom_line(aes(y = scale(biomass_detrended)), color = "black") +
  geom_line(aes(y = lag(scale(temperature_ts)), K = 1), color = "blue") +
  facet_wrap(~sample_ID)


## temperature sensitivity
biomass_ts  |> 
  filter(nutrients == "0.75 g/L") |> 
  ggplot(aes(x = temperature_ts_rollsum, y = biomass)) +
  geom_point() +
  facet_wrap(~sample_ID)

## histogram of the temperature sensitivity
ggplot(complete_aggr, aes(x = temperature_sensitivity_rs)) +
  geom_histogram()
ggplot(complete_aggr, aes(x = temperature_sensitivity_rs)) +
  geom_histogram() +
  facet_grid(temperature~nutrients) +
  geom_vline(xintercept = 0, color = "red")

## temperature sensitivity vs. balance
ggplot(complete_aggr, aes(x = sum_slopes, y = temperature_sensitivity_rs)) +
  geom_point() +
  geom_smooth(method = "lm")
ggplot(complete_aggr, aes(x = sum_slopes, y = temperature_sensitivity_rs)) +
  geom_point() +
  facet_grid(temperature~nutrients, scales = "free") +
  geom_smooth(method = "lm")




## nutrient levels
nutrient_levs <- unique(complete_aggr$nutrients)
temperature_levs <- unique(complete_aggr$temperature)



## a loop that makes a graph of each combination of nutrient level and temperature level
plots1 <- list()
counter <- 1
for (nutrient_oi in nutrient_levs) {
  for (temperature_oi in temperature_levs) {
    plots1[[counter]] <- complete_aggr |> 
      filter(nutrients == nutrient_oi, temperature == temperature_oi) |> 
      ggplot(aes(x = sum_slopes, y = temperature_sensitivity_rs)) +
      geom_point() +
      ggtitle(paste("nutrient level:", nutrient_oi, "temperature level:", temperature_oi)) + 
      geom_smooth(method = "lm")
    counter <- counter + 1
  }
}
wrap_plots(plots1, ncol = 3)


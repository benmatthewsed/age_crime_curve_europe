# load packages

library(tidyverse)

# download the data if file doesn't exist


if (!file.exists(here::here("data", "sg_convictions.Rdata"))) {
  download.file(
    url = "https://github.com/jamiedo/sg-age-crime/raw/master/convictions_data.RData",
    destfile = here::here("data", "sg_convictions.Rdata")
  )
}

# read in the data

load(here::here("data", "sg_convictions.Rdata"))


# I guess just use the rates from SG because why not eh

soi <- 
allCrimes |> 
  janitor::clean_names()

soi |> 
  mutate(age = as.numeric(as.character(age))) |> 
  filter(gender != "All",
         str_detect(measurement_type, "Rate"),
                    crime_type == "All crimes and offences",
         age < 50) |> 
  ggplot(aes(x = year, y = as.numeric(age), fill = convictions)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_wrap(~ gender) +
  coord_equal()


soi |> 
  mutate(age = as.numeric(as.character(age))) |> 
  filter(gender != "All",
         str_detect(measurement_type, "Rate"),
         crime_type == "All crimes and offences",
         age < 50) |> 
  ggplot(aes(x = age, y = convictions, colour = year, group = year)) +
  geom_line() +
  facet_wrap(~ gender)


soi |> 
  group_by(year)

# k-s test

soi_tmp <- 
soi |> 
  mutate(age = as.numeric(as.character(age))) |> 
  filter(gender != "All",
         str_detect(measurement_type, "Rate"),
         crime_type == "All crimes and offences",
         age < 50)


soi_tmp <- 
soi_tmp |> 
  group_by(year, gender) |> 
  nest()



ks_test <- function(data, data2){
  
 tmp <-  stats::ks.test(data$convictions,
                 data2$convictions)
  
tibble(p.value = tmp$p.value,
       statistic = tmp$statistic)
 
}

soi_tmp_m <- 
soi_tmp |> 
  filter(gender == "Male") |> 
  mutate(ks_result = map(data, ~ ks_test(.x, soi_tmp$data[[1]])))

soi_tmp_m |> 
  unnest(ks_result) |> 
  ggplot(aes(x = year, y = statistic)) +
  geom_line() +
  geom_hline(yintercept = 0.05)

soi_tmp_m |> 
  unnest(data) |> 
  group_by(year) |> 
  mutate(prop = convictions / sum(convictions)) |> 
  ggplot(aes(x = age, y = prop, group = year, colour = as.numeric(year))) +
  geom_line()  


soi_tmp_m |> 
  unnest() |> 
  group_by(year) |> 
  mutate(prop = convictions / sum(convictions),
         cum_prop = cumsum(prop)) |> 
  filter(age >= 16) |> 
  ggplot(aes(x = as.numeric(year), y = age, fill = cum_prop)) +
  geom_tile() +
  coord_equal() +
  geom_contour(aes(z = cum_prop)) +
  scale_fill_viridis_c()

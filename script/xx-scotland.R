# load packages

library(tidyverse)
library(gganimate)

source(here::here("script", "xx-functions.R"))

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
# and just focusing on overall ACCs

soi <- 
allCrimes |> 
  janitor::clean_names()

soi_total <- 
soi |>   
  filter(age != "All") |> 
  mutate(age = as.numeric(as.character(str_sub(age, 1, 2)))) |> 
  filter(gender == "All",
         str_detect(measurement_type, "Count"),
         crime_type == "All crimes and offences") |> 
  group_by(year) |> 
  summarise(conv = sum(convictions)) |> 
  mutate(country = "Scotland")

saveRDS(soi_total,
        here::here("results",
                   "scotland_total_convictions.rds"))

soi_subset <- 
soi |> 
  filter(age != "All") |> 
  mutate(age = as.numeric(as.character(str_sub(age, 1, 2)))) |> 
  filter(gender == "All",
         str_detect(measurement_type, "Rate"),
         crime_type == "All crimes and offences",
         age >= 15 & age < 60) |> 
  mutate(rowname = age,
         country = "Scotland")

soi_subset <- 
soi_subset |> 
  select(rowname, age, year, convictions, country)
  
 
saveRDS(
  soi_subset,
  here::here("results",
             "scotland_acc_rate.rds"))


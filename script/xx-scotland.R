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

soi <- 
allCrimes |> 
  janitor::clean_names()

soi |>   
  filter(age != "All") |> 
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
  filter(age != "All") |> 
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
  filter(age != "All") |> 
  mutate(age = as.numeric(as.character(age))) |> 
  filter(age != "All") |> 
  filter(gender != "All",
         str_detect(measurement_type, "Rate"),
         crime_type == "All crimes and offences",
         age < 50)


soi_tmp <- 
soi_tmp |> 
  group_by(year, gender) |> 
  nest()



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

soi_anim <- 
soi |> 
  filter(age != "All") |> 
  mutate(age = as.numeric(as.character(str_sub(age, 1, 2)))) |> 
  filter(gender == "All",
         str_detect(measurement_type, "Rate"),
         crime_type == "All crimes and offences",
         age > 15 & age <= 50) |> 
  ggplot(aes(x = age, y = convictions, group = year)) +
  geom_bar() +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {as.integer(frame_time)}', x = 'Age', y = 'Convicton rate') +
  transition_time(year)


gganimate::animate(soi_anim,
                   renderer = gifski_renderer())

anim_save(filename = "soi_anim.gif",
          path = here::here("outputs", "figures"))

soi_tmp <- 
soi |> 
  filter(age != "All") |> 
  mutate(age = as.numeric(as.character(str_sub(age, 1, 2)))) |> 
  filter(gender == "All",
         str_detect(measurement_type, "Rate"),
         crime_type == "All crimes and offences",
         age > 15 & age <= 50) |> 
  ggplot(aes(x = year, y = age, fill = convictions)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma") +
  coord_equal()

ggsave(
  here::here("outputs", "figures", "soi_tmp_lexis.png"),
  soi_tmp
)




# descriptives ------------------------------------------------------------



scot_sum <- 
soi |> 
  filter(age != "All") |> 
  mutate(age = as.numeric(as.character(age))) |> 
  filter(gender == "All",
         str_detect(measurement_type, "Rate"),
         crime_type == "All crimes and offences",
         age < 50) |> 
  as_tibble() |> 
  group_by(year) |> 
  summarise(mean_age = weighted.mean(age, convictions),
            median_age = matrixStats::weightedMedian(age, convictions),
            skew = moments::skewness(convictions),
            kurtosis = moments::kurtosis(convictions)) |> 
  pivot_longer(-year,
               names_to = "measure",
               values_to = "value") |>
  mutate(country = "Scotland")


bind_rows(scot_sum, switz_sum)

scot_sum |> 
  ggplot(aes(x = year, y = value, group = measure)) +
  geom_line() +
  facet_wrap(~ measure, scales = "free")

soi |> 
  filter(age != "All") |> 
  mutate(age = as.numeric(as.character(str_sub(age, 1, 2)))) |> 
  filter(gender == "All",
         str_detect(measurement_type, "Rate"),
         crime_type == "All crimes and offences",
         age > 15 & age <= 50) |> 
  as_tibble() |> 
  mutate(convictions = convictions / max(convictions)) |> 
  ggplot(aes(x = age, y = fct_rev(factor(year)), height = convictions)) + 
  geom_ridgeline(stat = "identity", 
                 scale = 0.9, draw_baseline = FALSE)

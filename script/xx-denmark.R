# https://www.statbank.dk/20338

# convictions data for denmark


# https://www.statbank.dk/20021

# population data



library(readxl)
library(tidyverse)
library(ungroup)

source(here::here("script",
                  "xx-functions.R"))


denmark <- read_csv("data/202333181848410289597STRAF4066505949901.csv",
skip = 2)


denmark <- 
  denmark |> 
  select(-...1, -...2) |> 
  fill(...3) |> 
  filter(!is.na(...4)) |> 
  pivot_longer(`1980`:`2021`,
               names_to = "year",
               values_to = "convictions") |> 
  rename(sex = ...3,
         age = ...4)

denmark_all <- 
denmark |> 
  group_by(age, year) |> 
  summarise(convictions = sum(convictions)) |> 
  ungroup()


ages <- 
  denmark_all |> 
  pivot_wider(names_from = c(year),
              values_from = convictions) |>
  select(age) |> 
  mutate(age = as.numeric(stringr::str_sub(age, 1, 2)))

wid_dat <- 
  denmark_all |> 
  pivot_wider(names_from = c(year),
              values_from = convictions) |> 
  select(-age)

denmark |> 
  count(age)

res <- 
  pclm2D(
    x = ages$age,
    y = wid_dat,
    control = list(lambda = c(NA, NA)),
    nlast = 20
  )




# adding the demographic data ---------------------------------------------

den_pop_1980_2011 <- read_csv("data/denmark_population_1980_2011.csv", 
                               skip = 2)

den_pop_1980_2011 <- 
den_pop_1980_2011 |> 
  select(-...1) |> 
  fill(...2) |> 
  filter(!is.na(...3)) |> 
  pivot_longer(`1980`:`2011`,
               names_to = "year",
               values_to = "pop") |> 
  rename(sex = ...2,
         age = ...3) |> 
  mutate(age = as.numeric(str_extract(age, "[0-9]+")))


den_pop_2012_2023 <- read_csv("data/denmark_population_2012_2023.csv",
                              skip = 2)

den_pop_2012_2023 <- 
den_pop_2012_2023 |> 
  select(-...1) |> 
  fill(...2) |> 
  filter(!is.na(...3)) |> 
  pivot_longer(`2012`:`2023`,
               names_to = "year",
               values_to = "pop") |> 
  rename(sex = ...2,
         age = ...3) |> 
  mutate(age = as.numeric(str_extract(age, "[0-9]+")))


den_pop <- 
  bind_rows(den_pop_1980_2011, den_pop_2012_2023)

den_pop_all <- 
den_pop |> 
  filter(sex == "Total")


den_pop_agg <- aggregate_demog(denmark_all,
                               den_pop_all)


den_pop_agg |> 
  count(age)

den_pop_wide <- 
den_pop_agg |> 
  mutate(year = as.numeric(year)) |> 
  filter(year >= min(denmark_all$year) & year <= max(denmark_all$year)) |> 
  group_by(age, year) |> 
  summarise(pop = sum(pop)) |> 
  ungroup() |> 
  pivot_wider(names_from = c(year),
              values_from = pop) |> 
  select(-age)


max_age <- max(den_pop$age)

as.data.frame(wid_dat) |> View()

den_res <- 
  pclm2D(
    x = ages$age,
    y = as.data.frame(wid_dat),
    nlast = 120 - max_age,
    offset = as.data.frame(den_pop_wide) / 1000,
    )


saveRDS(
  den_res,
  here::here("results",
             "denmark_all_model.rds")
)




den_fitted <- 
  den_res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1980`:`2021`,
               values_to = "convictions",
               names_to = "year") |> 
  filter(age < 60 & age >=15) |> 
  mutate(country = "Denmark")


saveRDS(
  den_fitted,
  here::here("results",
             "denmark_fitted_acc.rds")
)


den_fitted |> 
  filter(age == min(age))


den_total <- 
denmark_all |> 
  mutate(year = as.numeric(year)) |> 
  group_by(year) |> 
  summarise(conv = sum(convictions)) |> 
  mutate(country = "Denmark")


saveRDS(den_total,
        here::here("results",
                   "denmark_total_convictions.rds"))

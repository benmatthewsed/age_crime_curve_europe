# https://www.statbank.dk/20338

# convictions data for denmark


# https://www.statbank.dk/20021

# population data



library(readxl)
library(tidyverse)
library(ungroup)

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


denmark |> 
  ggplot(aes(x = age, y = convictions, group = year)) +
  geom_col(fill = "black") +
  facet_grid(year ~ sex)

ages <- 
  denmark |> 
  filter(sex == "Men") |> 
  pivot_wider(names_from = c(year),
              values_from = convictions) |>
  select(age) |> 
  mutate(age = as.numeric(stringr::str_sub(age, 1, 2)))

wid_dat <- 
  denmark |> 
  filter(sex == "Men") |> 
  pivot_wider(names_from = c(year),
              values_from = convictions) |> 
  select(-sex, -age)

res <- 
  pclm2D(
    x = ages$age,
    y = wid_dat,
    control = list(lambda = c(NA, NA)),
    nlast = 20
  )


res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1980`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  ggplot(aes(x = as.numeric(year), y = age, fill = n)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c()


res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1980`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  mutate(cohort = as.numeric(year) - age) |> 
  ggplot(aes(x = age, y = n, colour = as.numeric(cohort), group = cohort)) +
  geom_line()




wid_dat_w <- 
  denmark |> 
  filter(sex == "Women") |> 
  pivot_wider(names_from = c(year),
              values_from = convictions) |> 
  select(-sex, -age)

res_w <- 
  pclm2D(
    x = ages$age,
    y = wid_dat_w,
    control = list(lambda = c(NA, NA)),
    nlast = 20
  )


res_w$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1980`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  ggplot(aes(x = as.numeric(year), y = age, fill = n)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c()


str(res)


res$ci$upper

res$goodness.of.fit$standard.errors |> str()


ci_upp <- 
res$ci$upper |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1980`:`2021`,
               values_to = "conf_upp",
               names_to = "year") |> 
  mutate(cohort = as.numeric(year) - age)


ci_low <- 
  res$ci$lower |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1980`:`2021`,
               values_to = "conf_low",
               names_to = "year") |> 
  mutate(cohort = as.numeric(year) - age)

bind_rows(ci_low, ci_upp)

res_fit <- 
res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1980`:`2021`,
               values_to = "n",
               names_to = "year")

full_res <- 
left_join(res_fit, ci_low) |> 
  left_join(ci_upp)

full_res |> 
  filter(age == max(age)) |> 
  tail()

full_res |> 
  mutate(cohort = as.numeric(year) - age) |> 
  arrange(desc(age)) |>
  filter(age < 50) |> 
  ggplot(aes(x = age, y = n, group = year)) +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_upp,
                  fill = as.numeric(year)),
              alpha = 0.5,
              ) +
  geom_line(aes(colour = as.numeric(year)))


plot(res)



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



denm <- read_csv("data/202333181848410289597STRAF4066505949901.csv",
                    skip = 2)

denm <- 
  denm |> 
  select(-...1, -...2) |> 
  fill(...3) |> 
  filter(!is.na(...4)) |> 
  pivot_longer(`1980`:`2021`,
               names_to = "year",
               values_to = "convictions") |> 
  rename(sex = ...3,
         age = ...4)

denm |> 
  mutate(age_first = as.numeric(str_extract(age, "[0-9]+")),
         age_last = str_extract(str_sub(age, 3, str_length(age)),
                       "[0-9]+")) |> tail(50)

denm_ages <- 
denm |> 
  distinct(age) |> 
  mutate(age_first = as.numeric(str_extract(age, "[0-9]+")),
         age_last = str_extract(str_sub(age, 3, str_length(age)),
                                "[0-9]+"),
         age_last = case_when(
           is.na(age_last) & as.numeric(age_first) != max(as.numeric(age_first)) ~ as.numeric(age_first),
           is.na(age_last) & as.numeric(age_first) == max(as.numeric(age_first)) ~ 99,
           TRUE ~ as.numeric(age_last)
         ),
         age_range = map2(age_first, age_last, ~ seq(.x, .y, 1))) |> 
  unnest(age_range) |> 
  rename(age_group = age,
         age = age_range)

denm_ages_agg <- 
left_join(den_pop, denm_ages) |> 
  filter(!is.na(age_group),
         sex == "Men") |> 
  group_by(age_group, year) |> 
  summarise(pop = sum(pop)) |> 
  ungroup() |> 
  rename(age = age_group)

denmark <- denmark |> left_join(denm_ages_agg)

den_m_conv <- 
denmark |> 
  filter(sex == "Men") |> 
  select(-pop) |> 
  pivot_wider(names_from = c(year),
              values_from = convictions) |> 
  select(-sex, -age)

den_m_pop <- 
  denmark |> 
  filter(sex == "Men") |> 
  select(-convictions) |> 
  pivot_wider(names_from = c(year),
              values_from = pop) |> 
  select(-sex, -age)



res_m_rate <- 
  pclm2D(
    x = ages$age,
    y = as.data.frame(den_m_conv),
    nlast = 20,
    offset = as.data.frame(den_m_pop) / 1000,
    )

plot(res_m_rate)



res_m_rate$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1980`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  ggplot(aes(x = as.numeric(year), y = age, fill = n)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c()


den_pop |> 
  ggplot(aes(x = as.numeric(year), y = age, fill = pop)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c() +
  facet_wrap(~ sex)


denmark |> 
  filter(sex == "Men") |> 
  mutate(rate = convictions / pop * 1000) |> 
  ggplot(aes(x = age, y = rate, group = year, colour = as.numeric(year))) +
  geom_line()


res_m_rate$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1980`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  ggplot(aes(x = age, y = n, group = year, colour = as.numeric(year))) +
  geom_line()

# including offset doesn't seem to work with these data?
# ... or actually it totally did work?


# and now for danish women


den_w_conv <- 
  denmark |> 
  filter(sex == "Women") |> 
  select(-pop) |> 
  pivot_wider(names_from = c(year),
              values_from = convictions) |> 
  select(-sex, -age)

den_w_pop <- 
  denmark |> 
  filter(sex == "Women") |> 
  select(-convictions) |> 
  pivot_wider(names_from = c(year),
              values_from = pop) |> 
  select(-sex, -age)

res_w_rate <- 
  pclm2D(
    x = ages$age,
    y = as.data.frame(den_w_conv),
    nlast = 20,
    offset = as.data.frame(den_w_pop) / 1000,
  )

plot(res_w_rate)


res_w_rate$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1980`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  ggplot(aes(x = age, y = n, group = year, colour = as.numeric(year))) +
  geom_line()


res_w_rate$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1980`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  ggplot(aes(x = as.numeric(year), y = age, fill = n)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c()


res_w_rate$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1980`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  ggplot(aes(x = age, y = n, group = year, colour = as.numeric(year))) +
  facet_wrap(~ year) +
  geom_line()



# k-stest

den_fit <- 
res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1980`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50)


den_fit |> 
  unnest() |> 
  group_by(year) |> 
  mutate(prop = n / sum(n)) |> 
  ggplot(aes(x = age, y = prop, group = year, colour = as.numeric(year))) +
  geom_line()  


den_fit |> 
  unnest() |> 
  group_by(year) |> 
  mutate(prop = n / sum(n),
         cum_prop = cumsum(prop)) |> 
  ggplot(aes(x = as.numeric(year), y = age, fill = cum_prop)) +
  geom_tile() +
  coord_equal() +
  geom_contour(aes(z = cum_prop)) +
  scale_fill_viridis_c()

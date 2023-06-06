library(tidyverse)
library(readxl)

library(ungroup)

source(here::here("script",
                  "xx-functions.R"))

# https://www.ssb.no/en/statbank/table/10623

# remove on the spot fines?
# https://www.ssb.no/en/sosiale-forhold-og-kriminalitet/kriminalitet-og-rettsvesen/statistikk/straffereaksjoner

nor <- 
read_excel(
  here::here("data",
             "10623_20230505-183302.xlsx"),
  range = "D3:Y53"
)

nor <- 
nor |> 
  rename(age = ...1, sex = ...2) |> 
  fill(age, .direction = "down") |> 
  pivot_longer(cols = `2002`:`2021`,
               names_to = "year",
               values_to = "convs") |> 
  filter(age != "Total")

nor <- 
nor |> 
  filter(age != "All ages 15 years or older",
         sex != "Total")

ages <- 
nor |> 
  filter(sex == distinct(nor, sex)[[1]][[1]]) |> 
  pivot_wider(names_from = c(year),
              values_from = convs) |>
  select(age) |> 
  mutate(age = as.numeric(stringr::str_sub(age, 1, 2)))


wid_dat <- 
  nor |> 
  filter(sex == "Total") |> 
  pivot_wider(names_from = c(year),
              values_from = convs) |> 
  select(-sex, -age)


# res <-
#   pclm2D(
#     x = ages$age,
#     y = wid_dat,
#     control = list(lambda = c(NA, NA)),
#     nlast = 40
#   )


# res$fitted |> 
#   as.data.frame() |> 
#   tibble::rownames_to_column() |> 
#   as_tibble() |> 
# #   mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
# #   pivot_longer(cols = `2002`:`2021`,
# #                values_to = "n",
# #                names_to = "year") |> 
# #   filter(age < 50) |> 
# #   ggplot(aes(x = as.numeric(year), y = age, fill = n)) +
# #   geom_tile() +
# #   coord_equal() +
# #   scale_fill_viridis_c()
# 
# 
# res$fitted |> 
#   as.data.frame() |> 
#   tibble::rownames_to_column() |> 
#   as_tibble() |> 
#   mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
#   pivot_longer(cols = `2002`:`2021`,
#                values_to = "n",
#                names_to = "year") |> 
#   filter(age < 50) |> 
#   ggplot(aes(x = age, y = n, group = year, colour = as.numeric(year))) +
#   geom_line()


nor


# population data from 
# https://www.ssb.no/en/statbank/table/07459/tableViewLayout1/

nor_pop <- 
read_excel(
  here::here("data",
             "07459_20230509-145812.xlsx"),
  range = "B4:E8059",
  col_names = c("sex",
            "age",
            "year",
            "pop")
)

nor_pop <- 
nor_pop |> 
  fill(sex, .direction = "down") |> 
  fill(age, .direction = "down") |> 
  mutate(age = as.numeric(str_extract(age, "[0-9]+")))

max_age <- max(nor_pop$age)

nor_pop

nor_pop_agg <- aggregate_demog(nor,
                nor_pop)


nor_pop_agg_w <- 
  nor_pop_agg |> 
  mutate(year = as.numeric(year)) |> 
  filter(year >= 2002 & year <= 2021) |> 
  pivot_wider(names_from = c(year),
              values_from = pop) |> 
  select(-age)


nor_m <- 
  nor |> 
  filter(sex == "Males") |> 
  pivot_wider(names_from = c(year),
              values_from = convs) |> 
  select(-sex, -age)


pop_m <- as.data.frame(nor_pop_agg_w |> 
                         filter(sex == "Males") 
                       |> select(-sex))

res <- 
  pclm2D(
    x = ages$age,
    y = nor_m,
    offset = pop_m / 1000,
    control = list(lambda = c(NA, NA)),
    nlast = 120 - max_age
  )


res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `2002`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  group_by(year) |> 
  ggplot(aes(x = year, y = age, fill = n)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c()


res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `2002`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  ggplot(aes(x = age, y = n, colour = factor(year))) +
  geom_line()



nor_w <- 
  nor |> 
  filter(sex == "Females") |> 
  pivot_wider(names_from = c(year),
              values_from = convs) |> 
  select(-sex, -age)


pop_w <- as.data.frame(nor_pop_agg_w |> 
                         filter(sex == "Females") 
                       |> select(-sex))

res <- 
  pclm2D(
    x = ages$age,
    y = nor_w,
    offset = pop_w / 1000,
    control = list(lambda = c(NA, NA)),
    nlast = 120 - max_age
  )


res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `2002`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  group_by(year) |> 
  ggplot(aes(x = year, y = age, fill = n)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c()


res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `2002`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  ggplot(aes(x = age, y = n, colour = factor(year))) +
  geom_line()


nor

last_age <- 120

nor |> 
  mutate(age_first = as.numeric(str_extract(age, "[0-9]+")),
         age_last = str_extract(str_sub(age, 3, str_length(age)),
                                "[0-9]+"),
         age_last = case_when(
           is.na(age_last) & as.numeric(age_first) != max(as.numeric(age_first)) ~ as.numeric(age_first),
           is.na(age_last) & as.numeric(age_first) == max(as.numeric(age_first)) ~ 99,
           TRUE ~ as.numeric(age_last)
         ),
         age_last = if_else(is.na(age_last), last_age, age_last),
         age_range = map2(age_first, age_last, ~ seq(.x, .y, 1))) |> 
  unnest(age_range) |> 
  rename(age_group = age,
         age = age_range) |> 
  mutate(range = age_last - age_first + 1,
         ave_convs = convs / range) |> 
  filter(age <= 50) |> 
  ggplot(aes(x = year, y = age, fill = ave_convs)) +
  geom_tile() +
  coord_equal() +
  facet_wrap(~ sex) +
  scale_fill_viridis_c()


nor |> 
  mutate(age_first = as.numeric(str_extract(age, "[0-9]+")),
         age_last = str_extract(str_sub(age, 3, str_length(age)),
                                "[0-9]+"),
         age_last = case_when(
           is.na(age_last) & as.numeric(age_first) != max(as.numeric(age_first)) ~ as.numeric(age_first),
           is.na(age_last) & as.numeric(age_first) == max(as.numeric(age_first)) ~ 99,
           TRUE ~ as.numeric(age_last)
         ),
         age_last = if_else(is.na(age_last), last_age, age_last),
         age_range = map2(age_first, age_last, ~ seq(.x, .y, 1))) |> 
  unnest(age_range) |> 
  rename(age_group = age,
         age = age_range) |> 
  mutate(range = age_last - age_first + 1,
         ave_convs = convs / range) |> 
  filter(age <= 50) |> 
  ggplot(aes(x = age, y = ave_convs,
             colour = as.numeric(year),
             group = year)) +
  geom_line() +
  facet_wrap(~ sex)

nor_pop |> 
  mutate(year = as.numeric(year)) |> 
  ggplot(aes(x = year, y = age, fill = pop)) +
  geom_tile() +
  coord_equal() +
  facet_wrap(~ sex) +
  scale_fill_viridis_c()



# ks test


nor_tmp <- 
res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `2002`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  rename(convictions = n) |> 
  group_by(year) |> 
  nest()

nor_tmp |> 
  ungroup() |> 
  mutate(ks_result = map(data, ~ ks_test(.x, nor_tmp$data[[1]]))) |> 
  unnest(ks_result) |> 
  mutate(year = as.integer(year)) |> 
  ggplot(aes(x = year, y = p.value)) +
  geom_line() +
  geom_hline(yintercept = 0.05)


nor_tmp |> 
  unnest() |> 
  ungroup() |> 
  group_by(year) |> 
  mutate(prop = convictions / sum(convictions)) |> 
  ggplot(aes(x = age, y = prop, group = year, colour = as.numeric(year))) +
  geom_line()


nor_tmp |> 
  unnest() |> 
  group_by(age) |> 
  mutate(index_convictions = convictions / convictions[year == "2002"]) |> 
  ggplot(aes(x = as.numeric(year), y = age, fill = index_convictions)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c()

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
             "10623_20230531-172447.xlsx"),
  range = "B3:Y1103"
)

nor <- 
nor |> 
  rename(sanction_type = ...1,
         x = ...2, 
         age = ...3,
         sex = ...4) |> 
  fill(sanction_type, x, age, .direction = "down") |> 
  pivot_longer(cols = `2002`:`2021`,
               names_to = "year",
               values_to = "convs") |> 
  filter(age != "Total")

all_nor <- 
nor |> 
  filter(sanction_type == "All types of sanctions")

nor_fine <- 
nor |> 
  filter(sanction_type == "On the spot fine") |> 
  rename(fines = convs) |> 
  select(-sanction_type)

nor <- 
left_join(all_nor, nor_fine, join_by(x == x,
                                     age == age,
                                     sex == sex,
                                     year == year)) |> 
  mutate(new_total = convs - fines)

nor <- 
nor |> 
  filter(age != "All ages 15 years or older",
         sex != "Total") |> 
  filter(age != "Unknown age") |> 
  select(-x)

nor <- 
nor |> 
  select(-sanction_type,
         -convs,
         -fines) |> 
  rename(convs = new_total)

ages <- 
nor |> 
  filter(sex == distinct(nor, sex)[[1]][[1]]) |> 
  pivot_wider(names_from = c(year),
              values_from = convs) |>
  select(age) |> 
  mutate(age = as.numeric(stringr::str_sub(age, 1, 2)))


nor |> 
  count(age)



wid_dat <- 
  nor |> 
  filter(sex == "Both sexes") |> 
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


nor_pop_agg <- aggregate_demog(nor,
                nor_pop)


nor_pop_agg_w <- 
  nor_pop_agg |> 
  mutate(year = as.numeric(year)) |> 
  filter(year >= 2002 & year <= 2021) |> 
  group_by(age, year) |> 
  summarise(pop = sum(pop)) |> 
  ungroup() |> 
  pivot_wider(names_from = c(year),
              values_from = pop) |> 
  select(-age)


nor_wide <- 
  nor |> 
  filter(sex == "Both sexes") |> 
  pivot_wider(names_from = c(year),
              values_from = convs) |> 
  select(-sex, -age)


pop_wide <- as.data.frame(nor_pop_agg_w)

nor_all_res <- 
  pclm2D(
    x = ages$age,
    y = nor_wide,
    offset = pop_wide / 1000,
    control = list(lambda = c(NA, NA)),
    nlast = 120 - max_age
  )


saveRDS(
  nor_all_res,
  here::here("results",
             "norway_all_model.rds")
)


nor_fitted <- 
nor_all_res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `2002`:`2021`,
               values_to = "convictions",
               names_to = "year") |> 
  filter(age < 60) |> 
  mutate(country = "Norway")

saveRDS(
  nor_fitted,
  here::here("results",
             "norway_fitted_acc.rds")
)


nor_convs_all <- 
  nor |> 
  filter(sex == "Both sexes") |> 
  group_by(year) |> 
  summarise(conv = sum(convs)) |> 
  ungroup() |> 
  mutate(year = as.double(year),
         country = "Norway")


saveRDS(
  nor_convs_all,
  here::here("results",
             "norway_total_convictions.rds"))



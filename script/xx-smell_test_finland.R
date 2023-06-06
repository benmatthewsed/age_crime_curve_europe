library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(readr)
library(stringr)

library(ungroup)

finland <- 
read_csv(
  here::here("data", "001_12d2_2021_20221220-124936.csv"),
  skip = 2
)

finland <- janitor::clean_names(finland)

finland <- 
finland |> 
  select(-principal_offence)

finland <- 
finland |> 
  filter(age != "Total")


finland |> 
  ggplot(aes(x = age, y = convicted_number, group = year)) +
  geom_col(fill = "black") +
  facet_grid(year ~ gender)



Dx <- ungroup.data$Dx
Ex <- ungroup.data$Ex


x      <- c(0, 1, seq(5, 85, by = 5))
nlast  <- 26
n      <- c(diff(x), nlast)
group  <- rep(x, n)
y      <- aggregate(Dx, by = list(group), FUN = "sum")[, 2:10]
offset <- aggregate(Ex, by = list(group), FUN = "sum")[, 2:10]


wid_dat <- 
finland |> 
  pivot_wider(names_from = c(year),
              values_from = convicted_number) |> 
  filter(gender == "Total") |> 
  select(-gender, -age)

ages <- 
finland |> 
  pivot_wider(names_from = c(year),
              values_from = convicted_number) |> 
  filter(gender == "Total") |> 
  select(age) |> 
  mutate(age = as.numeric(stringr::str_sub(age, 1, 2)))

res <- 
pclm2D(
  x = ages$age,
  y = wid_dat,
  control = list(lambda = c(NA, NA)),
  nlast = 20
)


str(res)

res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1990`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  ggplot(aes(x = year, y = age, fill = n)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c()


res$goodness.of.fit$standard.errors |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1990`:`2021`,
               values_to = "se",
               names_to = "year") |> 
  filter(age < 50) |> 
  ggplot(aes(x = year, y = age, fill = se)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c()

res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1990`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  ggplot(aes(x = age, y = n, colour = as.numeric(year), group = year)) +
  geom_line()

# standardizing within each year

res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1990`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  group_by(year) |> 
  mutate(std_n = n / max(n)) |> 
  ggplot(aes(x = age, y = std_n, colour = as.numeric(year), group = year)) +
  geom_line()

res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1990`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  group_by(year) |> 
  mutate(std_n = n / max(n)) |> 
  ungroup() |> 
  ggplot(aes(x = year, y = age, fill = std_n)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c()


res$goodness.of.fit$standard.errors |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble()

res$deep$H



# population --------------------------------------------------------------


# data from https://pxdata.stat.fi/PxWeb/pxweb/en/StatFin/StatFin__vaerak/statfin_vaerak_pxt_11rd.px/table/tableViewLayout1/

fin_pop <- read_csv(here::here("data", "001_11rd_2022_20230504-160652.csv"))

fin_pop <- janitor::clean_names(fin_pop)

# seems to just take the first two values which is what we want

fin_pop_long <- 
fin_pop |> 
  select(-2) |> 
  pivot_longer(cols = !year,
               names_to = c("sex", "age"),
               names_sep = "_",
               values_to = "pop",
               )

fin_pop_long <- 
fin_pop_long |> 
  filter(year >= 1990,
         sex != "total",
         age != "total")

# hard codes oldest age to be 120

fin_ages <- 
  finland |> 
  distinct(age) |> 
  filter(age != "Total") |> 
  mutate(age_first = as.numeric(str_extract(age, "[0-9]+")),
         age_last = str_extract(str_sub(age, 3, str_length(age)),
                                "[0-9]+"),
         age_last = case_when(
           is.na(age_last) & as.numeric(age_first) != max(as.numeric(age_first)) ~ as.numeric(age_first),
           is.na(age_last) & as.numeric(age_first) == max(as.numeric(age_first)) ~ 99,
           TRUE ~ as.numeric(age_last)
         ),
         age_last = if_else(is.na(age_last), 120, age_last),
         age_range = map2(age_first, age_last, ~ seq(.x, .y, 1))) |> 
  unnest(age_range) |> 
  rename(age_group = age,
         age = age_range)

# watch out for coding of sex

fin_ages_agg <- 
  left_join(fin_ages, fin_pop_long |> mutate(age = as.numeric(age))) |> 
  filter(!is.na(age_group),
         sex == "males") |> 
  group_by(age_group, year) |> 
  summarise(pop = sum(pop)) |> 
  ungroup() |> 
  rename(age = age_group)

finland_pop <- finland |> left_join(fin_ages_agg)




wid_dat_m <- 
  finland |> 
  pivot_wider(names_from = c(year),
              values_from = convicted_number) |> 
  filter(gender == "Males") |> 
  select(-gender, -age)

ages <- 
  finland |> 
  pivot_wider(names_from = c(year),
              values_from = convicted_number) |> 
  filter(gender == "Total") |> 
  select(age) |> 
  mutate(age = as.numeric(stringr::str_sub(age, 1, 2)))

# need consistent gender/sex naming

fin_m_pop <- 
  finland_pop |> 
  filter(gender == "Males") |> 
  select(-convicted_number) |> 
  pivot_wider(names_from = c(year),
              values_from = pop) |> 
  select(-gender, -age)


fin_m_rate <- 
  pclm2D(
    x = ages$age,
    y = as.data.frame(wid_dat_m),
    nlast = 20,
    offset = as.data.frame(fin_m_pop) / 1000,
  )

plot(fin_m_rate)

fin_m_rate$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1990`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  ggplot(aes(x = as.numeric(year), y = age, fill = n)) +
  geom_tile() +
  geom_contour(aes(z = n)) +
  coord_equal() +
  scale_fill_viridis_c()


fin_m_rate$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1990`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  ggplot(aes(x = age, y = n, group = year, colour = as.numeric(year))) +
  geom_line()



fin_m_tmp <- 
fin_m_rate$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1990`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50)

fin_m_tmp <- 
fin_m_tmp |> 
  mutate(convictions = n) |> 
  group_by(year) |> 
  nest() |> 
  ungroup()

fin_m_tmp |> 
  mutate(ks_result = map(data, ~ ks_test(.x, fin_m_tmp$data[[1]]))) |> 
  unnest(ks_result) |> 
  mutate(year = as.integer(year)) |> 
  ggplot(aes(x = year, y = statistic)) +
  geom_line() +
  geom_hline(yintercept = 0.05)


fin_m_tmp |> 
  unnest() |> 
  group_by(year) |> 
  mutate(prop = n / sum(n)) |> 
  ggplot(aes(x = age, y = prop, group = year, colour = as.numeric(year))) +
  geom_line()



fin_m_tmp |> 
  unnest() |> 
  group_by(age) |> 
  mutate(index_convictions = convictions / convictions[year == "1990"]) |> 
  ggplot(aes(x = as.numeric(year), y = age, fill = index_convictions)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c()

# just index change

fin_m_tmp |> 
  unnest() |> 
  group_by(age) |> 
  mutate(delta_convictions = convictions / convictions[year == "1990"]) |> 
  ggplot(aes(x = as.numeric(year), y = age, fill = index_convictions)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c()




# cumulative ACC

fin_m_tmp |> 
  unnest() |> 
  group_by(year) |> 
  mutate(prop = convictions / sum(convictions),
         cum_prop = cumsum(prop)) |> 
  ggplot(aes(x = as.numeric(year), y = age, fill = cum_prop)) +
  geom_tile() +
  coord_equal() +
  geom_contour(aes(z = cum_prop)) +
  scale_fill_viridis_c()

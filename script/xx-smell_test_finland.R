library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(readr)

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

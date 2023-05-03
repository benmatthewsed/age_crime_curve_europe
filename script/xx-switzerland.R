library(tidyverse)
library(readxl)
library(ungroup)

switz <- read_excel("data/je-e-19.03.02.02.02.01.02b.xlsx", 
                                         sheet = "Total", skip = 4)

switz <- 
switz |> 
  janitor::clean_names() |> 
  rename(year = x1,
         convictions = x2,
         adults_convicted = x3) |> 
  select(year, contains("between")) |> 
  filter(!is.na(year)) |> 
  pivot_longer(cols = -year,
               names_to = "age",
               values_to = "conv") |> 
  mutate(age = str_remove_all(age, c("between_")),
         age = str_remove_all(age, c("_and")),
         age = str_remove_all(age, c("_years")))


switz_wid <- 
switz |> 
  filter(str_length(year) == 4) |> 
  mutate(year = as.integer(year),
         conv = as.integer(conv)) |> 
  pivot_wider(names_from = c(year),
              values_from = conv)


ages <- 
  switz_wid |> 
  select(age) |> 
  mutate(age = as.numeric(stringr::str_sub(age, 1, 2)))         



res <- 
  pclm2D(
    x = ages$age,
    y = switz_wid |> select(-age),
    control = list(lambda = c(NA, NA)),
    nlast = 20
  )



res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1984`:`2007`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  ggplot(aes(x = year, y = age, fill = n)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c()

all.equal(sum(res$fitted),
          sum(as.integer(switz$conv), na.rm = TRUE))

res$goodness.of.fit$standard.errors |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1984`:`2007`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  ggplot(aes(x = year, y = age, fill = n)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c()

res_fit <- 
res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1984`:`2007`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50)

res_se <- 
res$goodness.of.fit$standard.errors |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1984`:`2007`,
               values_to = "se",
               names_to = "year") |> 
  filter(age < 50) 

res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1984`:`2007`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  ggplot(aes(x = age, y = n, colour = factor(year))) +
  geom_line()



res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1990`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  group_by(year) |> 
  mutate(std_n = n / max(n)) |> 
  ungroup() |> 
  ggplot(aes(x = year, y = age, fill = std_n)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c()

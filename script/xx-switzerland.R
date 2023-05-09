library(tidyverse)
library(readxl)
library(ungroup)

source(here::here("script",
                  "xx-functions.R"))

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



# populaion ---------------------------------------------------------------


# https://www.bfs.admin.ch/bfs/en/home/statistics/catalogues-databases/data.assetdetail.23284918.html

switz_pop <- 
read_excel(here::here("data",
                      "px-x-0102030000_101_20230505-144659.xlsx"),
           skip = 2) |> 
  janitor::clean_names()

switz_pop <- 
switz_pop |> 
  fill(x1, .direction = "down") |> 
  mutate(x1 = str_remove_all(x1, "Sex - "),
         x2 = str_remove_all(x2, "([A-Za-z]+)"),
         x2 = str_remove_all(x2, " ")) |> 
  filter(x1 == "total") |> 
  select(x1:x2007) |> 
  pivot_longer(x1984:x2007,
               names_to = "year",
               values_to = "pop") |> 
  rename(sex = x1,
         age = x2) |> 
  mutate(year = as.numeric(str_remove_all(year, "x")),
         age = as.numeric(age))

switz_pop <- 
switz_pop |> 
  mutate(sex = "males")

# at the moment you need to have a variable sex == males
# in the pop data

switz_pop_agg <- 
aggregate_demog(convs_data = switz,
                demog_data = switz_pop)

switz_pop_agg_w <- 
switz_pop_agg |> 
  pivot_wider(names_from = c(year),
              values_from = pop) |> 
  select(-age)

res <- 
  pclm2D(
    x = ages$age,
    y = switz_wid |> select(-age),
    offset = as.data.frame(switz_pop_agg_w) / 1000,
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
  pivot_longer(cols = `1984`:`2007`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50) |> 
  ggplot(aes(x = age, y = n, colour = factor(year))) +
  geom_line()

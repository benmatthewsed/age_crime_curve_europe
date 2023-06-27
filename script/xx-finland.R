library(tidyverse)

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

finland_all <- 
finland |> 
  filter(age != "Total",
         gender == "Total")




fin_wide <- 
finland_all |> 
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
         sex == "total",
         age != "total")

# hard codes oldest age to be 120

fin_pop_agg <- aggregate_demog(finland_all,
                               fin_pop_long)


fin_pop_wide <- 
  fin_pop_agg |> 
  mutate(year = as.numeric(year)) |> 
  filter(year >= min(finland_all$year) & year <= max(finland_all$year)) |> 
  group_by(age, year) |> 
  summarise(pop = sum(pop)) |> 
  ungroup() |> 
  pivot_wider(names_from = c(year),
              values_from = pop) |> 
  select(-age)


finland_wide <- 
  finland_all |> 
  pivot_wider(names_from = c(year),
              values_from = convicted_number) |> 
  select(-gender, -age)


# need consistent gender/sex naming

fin_rate <- 
  pclm2D(
    x = ages$age,
    y = as.data.frame(finland_wide),
    nlast = 20,
    offset = as.data.frame(fin_pop_wide) / 1000,
  )


saveRDS(
  fin_rate,
  here::here("results",
             "finland_all_model.rds")
)



fin_fitted <- 
fin_rate$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `1990`:`2021`,
               values_to = "convictions",
               names_to = "year") |> 
  filter(age < 60) |> 
  mutate(country = "Finland")

saveRDS(
  fin_fitted,
  here::here("results",
             "finland_fitted_acc.rds")
)


fin_total <- 
  finland_all |> 
  mutate(year = as.numeric(year)) |> 
  group_by(year) |> 
  summarise(conv = sum(convicted_number)) |> 
  mutate(country = "Finland")

saveRDS(fin_total,
        here::here("results",
                   "finland_total_convictions.rds"))

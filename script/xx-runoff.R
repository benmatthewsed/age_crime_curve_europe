

# graphing code
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



# descriptives

soi_subset |> 
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




# crude average calculations ----------------------------------------------


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
         ave_convs = convs / range) 




# looking at confidence intervals -----------------------------------------


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

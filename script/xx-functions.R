
# turns demographic data file into the same aggregation
# as convictions data

aggregate_demog <- function(convs_data,
                            demog_data,
                            last_age = 120){

demog_data <- demog_data |> mutate(age = as.numeric(age))
  
dat_ages <- 
  convs_data |> 
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
         age_last = if_else(is.na(age_last), last_age, age_last),
         age_range = map2(age_first, age_last, ~ seq(.x, .y, 1))) |> 
  unnest(age_range) |> 
  rename(age_group = age,
         age = age_range)

# watch out for coding of sex

left_join(dat_ages, demog_data |> mutate(age = as.numeric(age))) |> 
  filter(!is.na(age_group)) |> 
  group_by(age_group, sex, year) |> 
  summarise(pop = sum(pop)) |> 
  ungroup() |> 
  rename(age = age_group)

}

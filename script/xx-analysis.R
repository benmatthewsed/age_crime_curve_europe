library(tidyverse)
library(gganimate)
library(ggridges)
library(fs)
library(metR)


# load data ---------------------------------------------------------------

total_dat <- 
tibble(
  files = fs::dir_ls(here::here("results"))
) |> 
  filter(str_detect(files, "_total_convictions")) |> 
  mutate(data = map(files, readRDS)) |> 
  select(-files)


total_plot <- 
total_dat |> 
  unnest(data) |> 
  ggplot(aes(x = year, y = conv, colour = country)) +
  geom_line() +
  geom_vline(xintercept = 1990) +
  labs(x = "Year",
       y = "Number convicted")

ggsave(
  here::here("outputs",
             "figures",
             "overall_conviction_trends.png"),
  total_plot,
  width = 10,
  height = 6
)       



# ACCs --------------------------------------------------------------------

fitted_accs <- 
  tibble(
    files = fs::dir_ls(here::here("results"))
  ) |> 
  filter(str_detect(files, "_fitted")) |> 
  mutate(data = map(files, readRDS)) |> 
  select(-files) |> 
  unnest(data) |> 
  mutate(year = as.double(year))

soi <- readRDS(here::here("results",
             "scotland_acc_rate.rds"))

soi <- 
soi |> 
  mutate(rowname = as.character(rowname))

total_accs <- 
   bind_rows(fitted_accs, soi)

# standardize within countries

total_accs <- 
total_accs |> 
  group_by(country) |> 
  mutate(std_convictions = convictions / max(convictions)) |> 
  group_by(country, year) |> 
  mutate(std_convictions_year = convictions / max(convictions)) |> 
  ungroup()

total_accs |> 
  ggplot(aes(x = year, y = age)) +
  geom_tile(aes(fill = std_convictions)) +  
  metR::geom_contour2(aes(z = std_convictions,
                                         label = after_stat(level))) +
  coord_equal() +
  facet_wrap(~ country) +
  scale_fill_viridis_c()
  
# joyplots - for standardizing within year

total_accs |> 
  ggplot(aes(x = age, y = fct_rev(factor(year)), height = std_convictions_year)) +
  ggridges::geom_ridgeline() +
  facet_wrap(~ country, ncol = 4)




# descriptives ------------------------------------------------------------

summary_stats <- 
total_accs |> 
  group_by(year, country) |> 
  summarise(mean_age = weighted.mean(age, convictions),
            median_age = matrixStats::weightedMedian(age, convictions),
            skew = moments::skewness(convictions),
            kurtosis = moments::kurtosis(convictions)) |> 
  ungroup()


modal_age <- 
total_accs |> 
  group_by(country, year) |> 
  filter(convictions == max(convictions)) |> 
  ungroup() |> 
  select(modal_age = age, year, country)


summary_stats <- left_join(summary_stats, modal_age)


summary_stats <- 
summary_stats |> 
  pivot_longer(cols = mean_age:modal_age,
             names_to = "measure",
             values_to = "value")

summary_stats |> 
  ggplot(aes(x = year, y = value, colour = country)) +
  geom_line() +
  facet_wrap(~ measure, scales = "free")




# index age ---------------------------------------------------------------


total_accs |> 
  group_by(age, country) |> 
  mutate(index_convictions = convictions / convictions[year == min(year)]) |> 
  ggplot(aes(x = year, y = age)) +
  geom_tile(aes(fill = index_convictions)) +
  metR::geom_contour2(aes(z = index_convictions,
                          label = after_stat(level))) +
  coord_equal() +
  facet_wrap(~ country) +
  scale_fill_viridis_c()



# comparing comparable years ----------------------------------------------

total_accs |> 
  filter(year == 2002 | year == 2018) |> 
  ggplot(aes(x = age, y = std_convictions_year, colour = factor(year))) +
  geom_line() +
  facet_wrap(~ country)


total_accs |> 
  group_by(age, country) |> 
  mutate(index_convictions = convictions / convictions[year == min(year)]) |> 
  filter(year == 2018) |> 
  ggplot(aes(x = age, y = index_convictions, colour = factor(year))) +
  geom_line() +
  facet_wrap(~ country) +
  geom_hline(yintercept = 1)


total_accs |> 
  group_by(age, country) |> 
  mutate(index_convictions = convictions / convictions[year == min(year)]) |> 
  ggplot(aes(x = age, y = index_convictions, colour = year, group = year)) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ country) +
  scale_colour_viridis_c()



index_anim <- 
total_accs |> 
  group_by(age, country) |> 
  mutate(index_convictions = convictions / convictions[year == min(year)]) |> 
  ggplot(aes(x = age, y = index_convictions, group = year)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_line() +
  facet_wrap(~ country) +
  labs(title = 'Year: {as.integer(frame_time)}', x = 'Age', y = 'Convicton rate') +
  transition_time(year)
  
  
  # graphing code
  gganimate::animate(index_anim,
                     renderer = gifski_renderer())

  anim_save(filename = "soi_anim.gif",
          path = here::here("outputs", "figures"))

library(tidyverse)
library(gganimate)
library(ggridges)
library(fs)
library(metR)
library(RColorBrewer)
library(ggokabeito)

source(here::here("script",
                  "xx-functions.R"))


# load data ---------------------------------------------------------------

total_dat <- 
tibble(
  files = fs::dir_ls(here::here("results"))
) |> 
  filter(str_detect(files, "_total_convictions")) |> 
  mutate(data = map(files, readRDS)) |> 
  select(-files)

# only from 1990 onwards

total_dat <- 
total_dat |> 
  unnest(data) |> 
  filter(year >= 1990)

total_plot <- 
total_dat |> 
  ggplot(aes(x = year, y = conv, colour = country)) +
  geom_line() +
  labs(x = "Year",
       y = "Number convicted",
       colour = "Country") +
  theme_minimal() +
  scale_color_okabe_ito()

ggsave(
  here::here("figures",
             "overall_conviction_trends.png"),
  total_plot,
  width = 8,
  height = 5
)       

ggsave(
  here::here(
    "docs", "figures",
             "overall_conviction_trends.png"),
  total_plot,
  width = 8,
  height = 5
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

total_accs <- 
total_accs |> 
  filter(year >= 1990)

# standardize within countries

  
# joyplots - for standardizing within year

# just overall ACCs

acc_fig <- 
total_accs |> 
  filter(year >= 1990)  |> 
  ggplot(aes(x = age, y = convictions, colour = year, group = year)) +
  geom_line() +
  facet_wrap(~ country, scales = "free") +
  scale_colour_viridis_c() +
  labs(x = 'Age',
       y = 'Convicton rate',
       colour = "Year",
       title = "") +
  theme_minimal()

ggsave(here::here("figures", "acc_overall_fig.png"),
       acc_fig,
       height = 5, 
       width = 8)

ggsave(here::here("docs", "acc_overall_fig.png"),
       acc_fig,
       height = 5, 
       width = 8)


acc_anim <- 
total_accs |> 
  filter(year >= 1990)  |> 
  ggplot(aes(x = age, y = convictions, colour = year, group = year)) +
  geom_line() +
  facet_wrap(~ country, scales = "free") +
  scale_colour_viridis_c() +
  labs(title = 'Year: {as.integer(frame_time)}',
       x = 'Age',
       y = 'Convicton rate',
       colour = "Year") +
  transition_time(year) +
  theme_minimal()


gganimate::animate(acc_anim,
                   renderer = gifski_renderer(),
                   height = 5, 
                   width = 8, 
                   units = "in",
                   res = 300)

anim_save(filename = "acc_anim.gif",
          path = here::here("docs", "figures"))


anim_save(filename = "acc_anim.gif",
          path = here::here("figures"))




total_accs <- 
  total_accs |> 
  group_by(country) |> 
  mutate(std_convictions = convictions / max(convictions)) |> 
  group_by(country, year) |> 
  mutate(std_convictions_year = convictions / max(convictions)) |> 
  ungroup()



total_accs |> 
  filter(year == 2002 | year == 2018) |> 
  ggplot(aes(x = age, y = std_convictions_year, colour = factor(year))) +
  geom_line() +
  facet_wrap(~ country) +
  theme_minimal()

selected_years_plot <- 
total_accs |> 
  group_by(country) |> 
  filter(year == 2002 | year == 2018) |> 
  ggplot(aes(x = age, y = std_convictions_year, colour = country,
             linetype = fct_rev(factor(year)))) +
  geom_line(size = 1) +
  facet_wrap(~ country) +
  theme_minimal() +
  scale_colour_okabe_ito() +
  labs(x = "Age",
       y = "Standardized conviction rate",
       colour = "Country",
       linetype = "Year")


ggsave(here::here("figures",
                  "selected_years.png"),
       selected_years_plot,
       width = 8,
       height = 6)

ggsave(here::here("docs",
                  "figures",
                  "selected_years.png"),
       selected_years_plot,
       width = 8,
       height = 6)

acc_standardized_surface <- 
total_accs |> 
  ggplot(aes(x = year, y = age)) +
  geom_tile(aes(fill = std_convictions)) +  
  metR::geom_contour2(aes(z = std_convictions,
                          label = after_stat(level))) +
  coord_equal() +
  facet_wrap(~ country) +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(x = "Year",
       y = "Age",
       fill = "Convictions\n(Country standardized)")

ggsave(
  here::here("outputs", "figures", "acc_std_facet.png"),
  acc_standardized_surface,
  height = 5,
  width = 5
)

plot_ridgeline <- 
total_accs |> 
  ggplot(aes(x = age,
             y = fct_rev(factor(year)),
             height = std_convictions_year,
             fill = country)) +
  ggridges::geom_ridgeline() +
  facet_wrap(~ country, ncol = 4) +
  theme_minimal() +
  labs(x = "Age",
       y = "Year",
       caption = "Age crime curves are standardized within year",
       fill = "Country") +
  scale_fill_okabe_ito()

ggsave(
  here::here("outputs", "figures", "acc_ridgeline.png"),
  plot_ridgeline,
  height = 5,
  width = 5
)


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

summary_stats <- 
summary_stats |> 
  mutate(measure = fct_relevel(measure,
                               "mean_age",
                               "median_age",
                               "modal_age",
                               "skew",
                               "kurtosis"))


measure_names <- c(
  `kurtosis` = "Kurtosis",
  `mean_age` = "Mean Age",
  `median_age` = "Median Age",
  `modal_age` = "Modal Age",
  `skew` = "Skew"
)


summary_age_graph <- 
summary_stats |> 
  filter(str_detect(measure, "age")) |> 
  ggplot(aes(x = year, y = value, colour = country)) +
  geom_line() +
  facet_wrap(~ measure,
             labeller = as_labeller(measure_names)) +
  theme_minimal() +
  labs(y = "Age",
       x = "Year",
       colour = "Country") +
  scale_color_okabe_ito()


ggsave(
  here::here("outputs", "figures", "summary_statistics.png"),
  height = 5,
  width = 9
)



# index age ---------------------------------------------------------------

acc_index_surface <- 
total_accs |> 
  group_by(age, country) |> 
  mutate(index_convictions = convictions / convictions[year == min(year)]) |> 
  ggplot(aes(x = year, y = age)) +
  geom_tile(aes(fill = index_convictions)) +
  metR::geom_contour2(aes(z = index_convictions,
                          label = after_stat(level))) +
  coord_equal() +
  facet_wrap(~ country) +
  scale_fill_gradient2(
    low = 'blue', mid = 'white', high = 'red',
    midpoint = 1, guide = 'colourbar', aesthetics = 'fill'
  ) +
  theme_minimal() +
  labs(x = "Year",
       y = "Age",
       fill = "Convictions\n(Age indexed)")

ggsave(
  here::here("outputs", "figures", "acc_age_index_surface.png"),
  acc_index_surface,
  height = 5,
  width = 6
)


total_accs |> 
  group_by(age, country) |> 
  mutate(index_convictions = convictions / convictions[year == min(year)]) |> 
  filter(year == 2018) |> 
  ggplot(aes(x = age, y = index_convictions, colour = factor(year))) +
  geom_line() +
  facet_wrap(~ country) +
  geom_hline(yintercept = 1) +
  theme_minimal()

acc_index_facet <-
total_accs |> 
  group_by(age, country) |> 
  mutate(index_convictions = convictions / convictions[year == min(year)]) |> 
  ggplot(aes(x = age, y = index_convictions, colour = year, group = year)) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ country) +
  scale_colour_viridis_c() +
  theme_minimal() +
  labs(title = "",
       colour = "Year",
       x = "Age",
       y = "Convictions (Age indexed)")

ggsave(
  here::here("figures", "acc_age_index_facet.png"),
  acc_index_facet,
  height = 5,
  width = 8
)

ggsave(
  here::here("docs", "figures", "acc_age_index_facet.png"),
  acc_index_facet,
  height = 5,
  width = 8
)



index_anim <- 
  total_accs |> 
  group_by(age, country) |> 
  mutate(index_convictions = convictions / convictions[year == min(year)]) |> 
  ungroup() |> 
  ggplot(aes(x = age, y = index_convictions, colour = year, group = year)) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~ country) +
  scale_colour_viridis_c() +
  theme_minimal() +
  labs(title = 'Year: {as.integer(frame_time)}', x = 'Age',
       y = 'Convictions (Age indexed)') +
  transition_time(year)



gganimate::animate(index_anim,
                   renderer = gifski_renderer(),
                   height = 5, 
                   width = 8, 
                   units = "in",
                   res = 300)

anim_save(filename = "index_anim.gif",
          path = here::here("figures"))

anim_save(filename = "index_anim.gif",
          path = here::here("docs", "figures"))

# graphing code
gganimate::animate(index_anim,
                   renderer = gifski_renderer())

anim_save(filename = "soi_anim.gif",
          path = here::here("outputs", "figures"))

# cohort representation ------

total_accs |> 
  mutate(cohort = year - age) |> 
  filter(year >= 1990,
         country == "Scotland")  |> 
  ggplot(aes(x = age, y = convictions, colour = cohort, group = cohort)) +
  geom_point() +
  facet_wrap(~ cohort) +
  scale_colour_viridis_c() +
  labs(x = 'Age',
       y = 'Convicton rate',
       colour = "Year",
       title = "") +
  theme_minimal()
  
cohort_acc_anim <- 
total_accs |> 
  mutate(cohort = year - age) |> 
  filter(year >= 1990,
         cohort >= 1970)  |> 
  ggplot(aes(x = age, y = convictions, colour = cohort, group = cohort)) +
  geom_line() +
  facet_wrap(~ country, scales = "free") +
  scale_colour_viridis_c() +
  theme_minimal() +
  labs(title = 'Cohort: {as.integer(frame_time)}', x = 'Age',
       y = 'Convictions',
       colour = "Cohort") +
  transition_time(cohort)

cohort_anim <- gganimate::animate(cohort_acc_anim,
                   renderer = gifski_renderer(),
                   height = 5, 
                   width = 8, 
                   units = "in",
                   res = 300)


anim_save(filename = "cohort_anim.gif",
          path = here::here("docs", "figures"))

anim_save(filename = "cohort_anim.gif",
          path = here::here("figures"))

total_accs |> 
  mutate(cohort = year - age) |> 
  filter(year >= 1990)  |> 
  ggplot(aes(x = cohort, y = age, fill = std_convictions)) +
  geom_tile() +
  facet_wrap(~ country, scales = "free") +
  scale_fill_viridis_c() +
  labs(x = 'Age',
       y = 'Convicton rate',
       colour = "Cohort",
       title = "") +
  theme_minimal()

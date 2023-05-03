# code to make animated figures of change in the age
# distribution of people in prison in Scotland 2010-2021

# load packages

library(readxl)
library(dplyr)
library(purrr)
library(ggplot2)
library(gganimate)
library(tweenr)
library(tidyr)
library(gridtext)
library(stringr)
library(ungroup)

# download the data

url <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2022/06/scottish-prison-population-statistics-2020-21/documents/spps-2020-21-supplementary-tables/spps-2020-21-supplementary-tables/govscot%3Adocument/spps-2020-21-supplementary-tables.xlsx"

download.file(url,
              destfile = here::here("data", "sg_prisons.xlsx"),
              mode = "wb")

# read in the dataset

sps <- 
  read_excel(
    here::here("data", "sg_prisons.xlsx"),
    sheet = 5,
    range = "B6:P213",
    col_names = c("gender", "age", "type", glue::glue("y{seq(2010, 2021, 1)}"))
  )

# data wrangling

sps <- 
  sps %>% 
  filter(type == "individuals") |> 
  pivot_longer(y2010:y2021) %>% 
  spread(type, value)


invalid_ages <- c("(Missing)",
               #   "All",
                  "75 or over",
                  "Under 16")

# they used to have age split by gender but now you have to look
# overall for age distributions


sps <- 
  sps %>% 
  filter(gender == "All",
         !age %in% invalid_ages)
  
  

sps <- 
  sps %>% 
  mutate(year = as.numeric(str_sub(name, 2, 5)))

# calculate average number of people per year of age
# within age-band
# this is a very crude measure and almost certainly wrong

sps_crude <- 
  sps %>% 
  mutate(age_start = as.numeric(str_sub(age, 1, 2)),
         age_end = as.numeric(str_sub(age, 4, 5)),
         long_age = map2(age_start, age_end, seq),
         length = map_dbl(long_age, length),
         ave_n = individuals / length,
         mid = map_dbl(long_age, mean)) %>% 
  select(gender, age, year, long_age, ave_n, mid) %>% 
  unnest(long_age) %>% 
  mutate(method = "crude") %>% 
  select(year, gender, age = long_age, est = ave_n, method)


# we can estimate a smooth curve to join up the mid-points
# between age-bands using spline interpolation
# this is slightly more sophisticated than the previous
# approach, but is still definitely wrong

# define the function

interp <- function(data, max_age){
  spline(data$mid, data$ave_n, n = 100000) %>% 
    as_tibble() %>% 
    filter(round(x, 3) %in% seq(16, max_age)) %>% 
    mutate(x = round(x, 0)) %>% 
    group_by(x) %>% 
    summarise(y = mean(y)) %>% 
    rename(age = x,
           approx_spline = y)
  
}

# apply the spline function to each ageband

sps_spline <- 
  sps %>% 
  mutate(age_start = as.numeric(str_sub(age, 1, 2)),
         age_end = as.numeric(str_sub(age, 4, 5)),
         long_age = map2(age_start, age_end, seq),
         length = map_dbl(long_age, length),
         ave_n = individuals / length,
         mid = map_dbl(long_age, mean)) %>% 
  select(gender, age, year, long_age, ave_n, mid) %>% 
  group_by(year, gender) %>% 
  nest() %>% 
  mutate(spline = map(data, interp, 70)) %>% 
  unnest(spline) %>% 
  mutate(method = "spline") %>% 
  select(year, gender, age, est = approx_spline, method)


# plot crude data ---------------------------------------------------------

plot_crude <- 
  sps_crude %>% 
  mutate(year = as.integer(year),
         year = glue::glue("{year}/{year+1}")) %>% 
  ggplot(aes(x = age, y = est, group = interaction(method, year))) +
  geom_density(stat = "identity",
               alpha = 0.4,
               fill = "#009dc6", # Understanding Inequalities blue
               colour = "#009dc6") +
  facet_wrap(~ gender) +
  theme_minimal(base_size = 20) +
  theme(plot.title.position = "plot") +
  scale_x_continuous(breaks = c(20, 30, 40, 50, 60, 70))

# add animation

animation_crude <- 
  plot_crude +
  labs(title = 'Estimated number of people in Scottish prisons by age: {closest_state}', 
       x = 'Age', 
       y = '',
       caption = "Data from Scottish Government. Estimates by @benmatthewsed.") +
  transition_states(year) +
  ease_aes('linear')

# render animation

anim_crude <- 
  animate(
    animation_crude,
    width = 600, height = 400, res = 60,
    type = "cairo",
    end_pause = 25
  )

# save figure

anim_save(
  here::here("figures", "sps-age-crude.gif"),
  anim_crude
)


# maybe now try this scaled by the total number of people
# in prison in each year (so it's the % within age)

plot_crude_percent <- 
sps_crude %>% 
  mutate(year = as.integer(year),
         year = glue::glue("{year}/{year+1}")) %>% 
  group_by(year) |> 
  mutate(age_prop = est / sum(est)) |> 
  ungroup() |> 
  ggplot(aes(x = age, y = age_prop, group = interaction(method, year))) +
  geom_density(stat = "identity",
               alpha = 0.4,
               fill = "#009dc6", # Understanding Inequalities blue
               colour = "#009dc6") +
  facet_wrap(~ gender) +
  theme_minimal(base_size = 20) +
  theme(plot.title.position = "plot") +
  scale_x_continuous(breaks = c(20, 30, 40, 50, 60, 70))



animation_crude_percent <- 
  plot_crude_percent +
  labs(title = 'Estimated proportion of people in Scottish prisons by age: {closest_state}', 
       x = 'Age', 
       y = '',
       caption = "Data from Scottish Government. Estimates by @benmatthewsed.") +
  transition_states(year) +
  ease_aes('linear')

# render animation

anim_crude_percent <- 
  animate(
    animation_crude_percent,
    width = 600, height = 400, res = 60,
    type = "cairo",
    end_pause = 25
  )

# save figure

anim_save(
  here::here("figures", "sps-age-crude-percent.gif"),
  anim_crude_percent
)


# splot the spline interpolation

plot_spline <- 
  sps_spline %>% 
  ungroup() %>% 
  mutate(year = as.integer(year),
         year = glue::glue("{year}/{year+1}")) %>% 
  ggplot(aes(x = age, y = est, group = interaction(method, year))) +
  geom_density(stat = "identity",
               alpha = 0.4,
               fill = "#009dc6",
               colour = "#009dc6") +
  facet_wrap(~ gender) +
  theme_minimal(base_size = 20) +
  theme(plot.title.position = "plot") +
  scale_x_continuous(breaks = c(20, 30, 40, 50, 60, 70))

# add animations

animation_spline <- 
  plot_spline +
  labs(title = 'Estimated number of people in Scottish prisons by age: {closest_state}', 
       x = 'Age', 
       y = '',
       caption = "Data from Scottish Government. Estimates by @benmatthewsed.") +
  transition_states(year) +
  ease_aes('linear')


# render animation

anim_spline <- 
  animate(
    animation_spline,
    width = 600, height = 400, res = 60,
    type = "cairo",
    end_pause = 25
  )

# save figure

anim_save(
  here::here("figures", "sps-age-spline-interpolation.gif"),
  anim_spline
)




plot_spline_percent <- 
  sps_spline %>% 
  ungroup() %>% 
  mutate(year = as.integer(year),
         year = glue::glue("{year}/{year+1}")) %>% 
  group_by(year) |> 
  mutate(age_prop = est / sum(est)) |> 
  ungroup() |> 
  ggplot(aes(x = age, y = age_prop, group = interaction(method, year))) +
  geom_density(stat = "identity",
               alpha = 0.4,
               fill = "#009dc6",
               colour = "#009dc6") +
  facet_wrap(~ gender) +
  theme_minimal(base_size = 20) +
  theme(plot.title.position = "plot") +
  scale_x_continuous(breaks = c(20, 30, 40, 50, 60, 70))

# add animations

animation_spline_percent <- 
  plot_spline_percent +
  labs(title = 'Estimated number of people in Scottish prisons by age: {closest_state}', 
       x = 'Age', 
       y = '',
       caption = "Data from Scottish Government. Estimates by @benmatthewsed.") +
  transition_states(year) +
  ease_aes('linear')


# render animation

anim_spline_percent <- 
  animate(
    animation_spline_percent,
    width = 600, height = 400, res = 60,
    type = "cairo",
    end_pause = 25
  )

# save figure

anim_save(
  here::here("figures", "sps-age-spline-interpolation-percent.gif"),
  anim_spline_percent
)


# modelling approach ------------------------------------------------------


sps <- 
  read_excel(
    here::here("data", "sg_prisons.xlsx"),
    sheet = 5,
    range = "B6:P213",
    col_names = c("gender", "age", "type", glue::glue("y{seq(2010, 2021, 1)}"))
  )

# data wrangling

sps2 <- 
  sps %>% 
  filter(type == "individuals") |> 
  pivot_longer(y2010:y2021) %>% 
  spread(type, value)


invalid_ages <- c("(Missing)",
                  #   "All",
               #   "75 or over",
                  "Under 16")

sps2 <- 
  sps2 %>% 
  filter(gender == "All",
         !age %in% invalid_ages)

sps2 <- 
sps2 %>% 
  mutate(age_start = as.numeric(str_sub(age, 1, 2)))

mod_res <- 
pclm(
  x = sps2$age_start,
  y = sps2$individuals,
  nlast = 25,
  offset = NULL,
  out.step = 1,
  ci.level = 95,
  verbose = FALSE,
  control = list()
)

get_pclm_ests <- function(dat){
  # note this only gets the estimates not the uncertainty
  
  pclm(
    x = dat$age_start,
    y = dat$individuals,
    nlast = 25,
    offset = NULL,
    out.step = 1,
    ci.level = 95,
    verbose = FALSE,
    control = list()
  )$fitted |> as_tibble() |> 
    tibble::rownames_to_column() |> 
    mutate(age = as.numeric(rowname) + 15)
  
  
}

plcm_res <- 
sps |> 
  group_by(name) |> 
  nest() |> 
  mutate(fitted = map(data, get_pclm_ests))

plcm_res |> 
  unnest(fitted) |> 
  ggplot(aes(x = age, y = value)) +
  geom_col() +
  facet_wrap(~name)


plcm_res |> 
  unnest(fitted) |> 
  mutate(year = as.numeric(str_sub(name, 2, 5))) |> 
  ggplot(aes(x = age, y = value)) +
  geom_col() +
  geom_col(data = sps_spline, aes(y = est), fill = "red") +
  facet_wrap(~ year)




plcm_res |> 
  unnest(fitted) |> 
  mutate(year = as.numeric(str_sub(name, 2, 5))) |> 
  filter(age < 50) |> 
  ggplot(aes(x = year, y = age, fill = value)) +
  geom_tile() +
  coord_equal()



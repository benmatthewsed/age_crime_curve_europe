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

fitted_accs <- 
  tibble(
    files = fs::dir_ls(here::here("results"))
  ) |> 
  filter(str_detect(files, "_fitted")) |> 
  mutate(data = map(files, readRDS)) |> 
  select(-files) |> 
  unnest(data) |> 
  mutate(year = as.double(year))

title_figure <- 
fitted_accs |> 
  ggplot(aes(x = age, y = convictions, group = interaction(year, country))) +
  geom_line(colour = "black", alpha = 0.1) +
  theme_void() +
  theme(panel.background = element_rect(fill = "grey90")) +
  scale_y_continuous(limits = c(0, 200))

ggsave(here::here("resources",
                  "background.png"),
       title_figure,
       width = 16,
       height = 9)

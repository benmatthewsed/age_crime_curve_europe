bind_rows(
  soi_total,
  nor_total,
  fin_total,
  den_total,
  switz_total
) |> 
  ggplot(aes(x = year, y = conv, colour = country)) +
  geom_line()

dat <- 
bind_rows(
  soi_total,
  nor_total,
  fin_total,
  den_total,
  switz_total
) |> 
  group_by(country) |> 
  mutate(prop_conv = conv / max(conv)) |> 
  ungroup()

dat |> 
  ggplot(aes(x = year, y = prop_conv, colour = country)) +
  geom_line() +
  geom_smooth(data = dat |> filter(year > 1989),
              method = "lm",
              se = FALSE)


switz_res 

soi_tmp <- 
  soi |> 
  filter(age != "All") |> 
  mutate(age = as.numeric(as.character(str_sub(age, 1, 2)))) |> 
  as_tibble() |> 
  filter(gender == "All",
         str_detect(crime_type, "All"),
         str_detect(measurement_type, "Rate"),
         crime_type == "All crimes and offences",
         age > 16 & age <= 50)

comb <- 
bind_rows(
switz_res |> 
  select(age, year, convictions = n) |> 
  mutate(country = "Switzerland",
         year = as.numeric(year)),

soi_tmp |> 
  select(age, year, convictions) |> 
  mutate(country = "Scotland")
)


comb |> 
  group_by(country) |> 
  mutate(rel_conv = convictions / max(convictions)) |> 
  ggplot(aes(x = year, y = age, fill = rel_conv)) +
  facet_wrap(~ country) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c()



comb |> 
  group_by(country) |> 
  mutate(rel_conv = convictions / max(convictions)) |> 
  ungroup() |> 
  mutate(diff = rel_conv - rel_conv[country == "Switzerland"]) |> 
  ggplot(aes(x = year, y = age, fill = diff)) +
  facet_wrap(~ country) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c()

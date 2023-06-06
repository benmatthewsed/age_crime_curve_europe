install.packages("fasano.franceschini.test")
install.packages("ks")

library(fasano.franceschini.test)
library(ks)
library(philentropy)

set.seed(8192)
samp <- 1000
x <- rnorm.mixt(n=samp, mus=0, sigmas=1, props=1)
y <- rnorm.mixt(n=samp, mus=0, sigmas=1, props=1)
kde.test(x1=x, x2=y)

S1 <- data.frame(x = rnorm(n = 50, mean = 0, sd = 1),
                 y = rnorm(n = 50, mean = 0, sd = 3))
S2 <- data.frame(x = rnorm(n = 100, mean = 0, sd = 1),
                 y = rnorm(n = 100, mean = 0, sd = 3))

fasano.franceschini.test(x, y, seed = 0)


fasano.franceschini.test(S1, S2, seed = 0)

kde.test(x1=S1, x2=S2)


S1 <- cbind(rgamma(n = 43, shape = 2),
            rpois(n = 43, lambda = 5),
            rpois(n = 43, lambda = 3.5))
S2 <- cbind(rgamma(n = 72, shape = 2),
            rpois(n = 72, lambda = 5),
            rpois(n = 72, lambda = 3.5))

fasano.franceschini.test(S1, S2, seed = 1)


soi <- 
soi |> 
  mutate(age = as.numeric(as.character(age))) |> 
  filter(gender == "Male",
         str_detect(measurement_type, "Rate"),
         crime_type == "All crimes and offences",
         age < 50) 

nor <- 
res$fitted |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  as_tibble() |> 
  mutate(age = as.numeric(stringr::str_sub(rowname, 2, 3))) |> 
  pivot_longer(cols = `2002`:`2021`,
               values_to = "n",
               names_to = "year") |> 
  filter(age < 50)
  

fasano.franceschini.test(soi |> select(age, year, convictions)
                         |> mutate(age = as.numeric(age),
                                   year = as.numeric(year)), nor |> select(age, year, n) |> 
                           mutate(year = as.numeric(year)), seed = 1)

kde.test(soi |> select(age, year, convictions)
                         |> mutate(age = as.numeric(age),
                                   year = as.numeric(year)), nor |> select(age, year, n) |> 
                           mutate(year = as.numeric(year)))

KL(cbind(nor, soi))

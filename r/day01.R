library(tidyverse)

################################################################

# Read Data

input.prod <- read_file("../data/day01/prod.txt")
input.test <- read_file("../data/day01/dev.txt")

input <- input.test
input <- input.prod

input <- strsplit(input, "\n")[[1]] %>%
  as.numeric()

################################################################

# Part 1
df <- as.vector(rbind(input, 2020 - input)) %>% 
  table()

# Take the ones that repeat
which(df == 2) %>% 
  names() %>% 
  as.numeric() %>% 
  prod()

################################################################

# Part 2
tidyr::crossing(x = input, 
                y = input,
                z = input) %>%
  dplyr::mutate(
    sum = x + y + z
  ) %>%
  dplyr::filter(
    sum == 2020
  ) %>%
  head(1) %>%
  select(x, y, z) %>%
  prod()

################################################################

# Other solutions

# From David Robinson: https://twitter.com/drob/status/1334108367665688579

# Part 1
intersect(input, 2020 - input)
prod(intersect(input, 2020 - input))

# Part 2
intersect(input, 2020 - outer(input, input, "+"))
prod(intersect(input, 2020 - outer(input, input, "+")))

# https://github.com/riinuots/advent2020/blob/main/solutions/day01/report_repair.R

################################################################

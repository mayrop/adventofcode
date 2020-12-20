library(tidyverse)
library(data.table)

input.test <- read_file("data/day02/dev.txt")
input.prod <- read_file("data/day02/prod.txt")

input <- input.test
input <- input.prod

# Part 1
vars <- c("min", "max", "letter", "string")

df <- strsplit(input, "\n")[[1]] %>%
  as_tibble() %>%
  tidyr::extract(value, into=vars, "(\\d+)-(\\d+) (\\w+): (\\w+)") %>%
  dplyr::mutate(across(c("min", "max"), as.numeric)) %>%
  dplyr::mutate(
    n = str_count(string, letter), 
    is_valid = n >= min & n <= max
  )

df
sum(df$is_valid)

################################################

# Part 2
df <- df %>%
  dplyr::mutate(
    char_min = substr(string, min, min),
    char_max = substr(string, max, max),
    char_min_equals = char_min == letter,
    char_max_equals = char_max == letter,
    is_valid_2 = xor(char_min_equals, char_max_equals)
  )

df
sum(df$is_valid_2)

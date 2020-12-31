library(tidyverse)

################################################################

# Read Data

input.test <- read_file("../data/day02/dev.txt")
input.prod <- read_file("../data/day02/prod.txt")

input <- input.test
input <- input.prod

# Transform Data

vars <- c("min", "max", "letter", "string")

df <- strsplit(input, "\n")[[1]] %>%
  as_tibble() %>%
  tidyr::extract(value, 
                 into = vars, 
                 "(\\d+)-(\\d+) (\\w+): (\\w+)",
                 convert = TRUE)

################################################################

# Part 1
df %>%
  dplyr::mutate(
    n = str_count(string, letter), 
    is_valid = n >= min & n <= max
  ) %>%
  dplyr::filter(
    is_valid
  ) %>%
  nrow()

################################################################

# Part 2
df %>%
  dplyr::mutate(
    char_min = substr(string, min, min),
    char_max = substr(string, max, max),
    char_min_equals = char_min == letter,
    char_max_equals = char_max == letter,
    is_valid = xor(char_min_equals, char_max_equals)
  ) %>%
  dplyr::filter(
    is_valid
  ) %>%
  nrow()

################################################################

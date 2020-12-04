library(tidyverse)
library(data.table)

input.prod <- read_file("data/day01/prod.txt")
input.test <- read_file("data/day01/dev.txt")

input <- input.test
input <- input.prod

# Part 1
find_sum <- function(df, val) {
  df %>%
    dplyr::mutate(y = val - value) %>%
    dplyr::group_by(value) %>%
    dplyr::mutate(min = min(value, y)) %>%
    dplyr::ungroup() %>%
    dplyr::add_count(min) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::filter(n == 2) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(
      ans = value * y
    )
}

df <- strsplit(input, "\n")[[1]] %>% 
  as.numeric() %>% 
  as_tibble()

find_sum(df, 2020)
  
################################################

# Part 2
df2 <- df %>%
  dplyr::mutate(
    sum2 = 2020 - df$value
  )

for (i in 1:nrow(df2)) {
  vals = find_sum(df, df2[i, ]$sum2)
  if (nrow(vals) == 1) {
    print(vals)
    print(df[i, ])
    print(df[i, ] * vals$ans)
    break;
  }
}


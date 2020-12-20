library(tidyverse)
library(memoise)
library(glue)

################################################################
# Functions
get_patterns_as_list <- function(rules) {
  temp <- rules %>%
    dplyr::filter(!is_rule) %>%
    dplyr::mutate(
      string = gsub("\\W", "", string)
    )
  split(temp, temp$id)
}

# This function will return a named list.
# Each list will look like:
# 
# $`134`
# # A tibble: 1 x 4
# id           string   is_rule         parts     
# <int>         <chr>     <lgl>        <list>    
# 134   129 5 | 76 92      TRUE    <list [2]>
#  
# Then parts will be a list that will look like:
# [[1]]
# [[1]][[1]]
# [1] 129   5
# 
# [[1]][[2]]
# [1] 76 92
get_rules_as_list <- function(rules) {
  # This function will convert something like "92 5 | 5 5" into:
  #  list(
  #    c(92, 5),
  #    c(5, 5)
  #  )
  convert_string_to_list_of_numbers <- function(.x) {
    strsplit(.x, "\\s*\\|\\s*")[[1]] %>% 
      purrr::map(., ~strsplit(., " ")[[1]] %>% as.double())
  }

  temp <- rules %>%
    dplyr::filter(is_rule) %>%
    dplyr::mutate(
      parts = purrr::map(string, convert_string_to_list_of_numbers) 
    )

  split(temp, temp$id)
}

# This function will check if the required rule N is a pattern
# And if not, it will find it's contents (recursively)
find_string <- function(num) { 
  num <- as.character(num)
  
  if (num %in% names(rules_list)) {
    row <- rules_list[[num]]
    y <- find_string_for_row(row)
    
    return(get_combos(y))
  }
  
  patterns_list[[num]]$string
}

find_string_for_row <- memoise(function(row) {
  purrr::map(row$parts[[1]], ~lapply(., find_string))  
})

get_combos <- function(lists) {
  purrr::map(lists, ~expand.grid(.) %>% 
               tidyr::unite("comb", sep="") %>% 
               dplyr::pull(comb)) %>% 
    unlist()
}

get_regexes <- function(lists) {
  purrr::map(lists, ~glue::glue("(", glue::glue_collapse(., sep="|"), ")")) %>%
    glue::glue_collapse(.)
}

################################################################

# Goal:
# To explore and practice apply and map functions

# Read Data
input.prod <- read_file("../data/day19/prod.txt")
input.test <- read_file("../data/day19/dev.txt")

input <- input.test
input <- input.prod

# Transform Data

parts <- strsplit(input, "\n\n")[[1]]
rules <- strsplit(parts[[1]], "\n")[[1]]
strings <- strsplit(parts[[2]], "\n")[[1]]

rules <- dplyr::as_tibble(rules) %>% 
  tidyr::extract(value, regex="(\\d+): (.+)", into=c("id", "string"), convert=TRUE) %>%
  dplyr::mutate(
    is_rule = grepl("\\d", string)
  )

patterns_list <- get_patterns_as_list(rules)

rules_list <- get_rules_as_list(rules)

strings_list <- purrr::map(rules_list, find_string_for_row)
 

#################################################################
# Part 1

# Naive solution to get all the combinations and see which ones match
# Bad performance, but it works :)
final_rules <- get_combos(strings_list[["0"]])
intersect(strings, final_rules) %>% 
  length()

#################################################################

# Part 2

# Solution 1 wouldn't work since there are too many combinations
# So going into regexp approach
regex_42 <- purrr::map(strings_list[["42"]], get_regexes) %>% unlist()
regex_31 <- purrr::map(strings_list[["31"]], get_regexes) %>% unlist()

# This one took me a while to understand
# For: 11: 42 31 | 42 11 31
# It seems that the regexp is "42 42{N}31{N} 31" instead of "42 (42 31)+ 31" (oh well)
# This is the only way the string "aaabbbbbbaaaabaababaabababbabaaabbababababaaa"
# will pass (in here, N = 4)
regex_part2 <- glue::glue_collapse(
  c(
    "^(",
    glue::glue("(", glue::glue_collapse(regex_42, sep="|"), ")+"),
    ")(",
    glue::glue("(", glue::glue_collapse(regex_42, sep="|"), "){{X}}"),
    glue::glue("(", glue::glue_collapse(regex_31, sep="|"), "){{X}}"),
    ")$"
  )
)

combos <- sapply(1:10, function(num) { 
  gsub("X", num, regex_part2) 
})

matches <- sapply(combos, function(combo) {
  grepl(combo, strings)
})

# How many do at least pass 1 rule?
sum(matches %>% t() %>% colSums() >= 1)

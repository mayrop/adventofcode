library(tidyverse)

################################################################

# Read Data
input.prod <- read_file("../data/day04/prod.txt")
input.test <- read_file("../data/day04/dev.txt")

input <- input.prod
input <- input.test

# Transform Data
df <- strsplit(input, "\n\n")[[1]] %>%
  as_tibble() %>%
  dplyr::mutate(value = gsub("\\n", " ", value)) %>%
  tidyr::extract(value, "byr", regex="byr:([^ ]+)", remove=FALSE) %>%
  tidyr::extract(value, "iyr", regex="iyr:([^ ]+)", remove=FALSE) %>%
  tidyr::extract(value, "eyr", regex="eyr:([^ ]+)", remove=FALSE) %>%
  tidyr::extract(value, "hgt", regex="hgt:([^ ]+)", remove=FALSE) %>%
  tidyr::extract(value, "hcl", regex="hcl:([^ ]+)", remove=FALSE) %>%
  tidyr::extract(value, "ecl", regex="ecl:([^ ]+)", remove=FALSE) %>%
  tidyr::extract(value, "pid", regex="pid:([^ ]+)", remove=FALSE)

################################################################

# Part 1
missing_values <- apply(df, 1, function(row) {
  sum(is.na(row))
})

sum(missing_values == 0)
 
################################################################

# Part 2
df %>%
  tidyr::extract(pid, "pid", regex="([0-9]+)", remove=FALSE) %>%
  tidyr::extract(hgt, c("hgt_val", "hgt_measure"), regex="(\\d+)(cm|in)", remove=FALSE) %>%
  tidyr::extract(hcl, "hcl_val", regex="#([a-f0-9]+)", remove=FALSE) %>%
  dplyr::mutate(
    has_hgt_valid = ifelse(hgt_measure == "cm", 
                           hgt_val >= 150 & hgt_val <= 193, 
                           hgt_val >= 59 & hgt_val <= 76),
  ) %>%
  dplyr::filter(
    byr >= 1920 & byr <= 2002,
    iyr >= 2010 & iyr <= 2020,
    eyr >= 2020 & eyr <= 2030,
    has_hgt_valid,
    nchar(hcl_val) == 6,
    nchar(hcl) == 7,
    ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
    nchar(pid) == 9,
  ) %>%
  nrow()

################################################################
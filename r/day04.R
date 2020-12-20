library(tidyverse)
library(data.table)

input.prod <- read_file("data/day04/prod.txt")
input.test <- read_file("data/day04/dev.txt")

input <- input.prod
input <- input.test

# Part 1
df <- strsplit(input, "\n\n")[[1]] %>%
  as_tibble() %>%
  dplyr::mutate(value = gsub("\\n", " ", value))

df %>%
  dplyr::mutate(
    has_byr = grepl("byr:", value),
    has_iyr = grepl("iyr:", value),
    has_eyr = grepl("eyr:", value),
    has_hgt = grepl("hgt:", value),
    has_hcl = grepl("hcl:", value),
    has_ecl = grepl("ecl:", value),
    has_pid = grepl("pid:", value),
    all = has_byr & has_iyr & has_eyr & has_hgt & has_hcl & has_ecl & has_pid
  ) %>%
  dplyr::filter(
    all
  )

# Part 2
df %>%
  tidyr::extract(value, "byr", regex="byr:([^ ]+)", remove=FALSE) %>%
  tidyr::extract(value, "iyr", regex="iyr:([^ ]+)", remove=FALSE) %>%
  tidyr::extract(value, "eyr", regex="eyr:([^ ]+)", remove=FALSE) %>%
  tidyr::extract(value, "hgt", regex="hgt:([^ ]+)", remove=FALSE) %>%
  tidyr::extract(value, "hcl", regex="hcl:([^ ]+)", remove=FALSE) %>%
  tidyr::extract(value, "ecl", regex="ecl:([^ ]+)", remove=FALSE) %>%
  tidyr::extract(value, "pid", regex="pid:([^ ]+)", remove=FALSE) %>%
  tidyr::extract(pid, "pid", regex="([0-9]+)", remove=FALSE) %>%
  tidyr::extract(hgt, "hgt_val", regex="(\\d+)", remove=FALSE) %>%
  tidyr::extract(hgt, "hgt_measure", regex="([a-z]+)", remove=FALSE) %>%
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
    hgt_measure %in% c("cm", "in"),
    has_hgt_valid,
    nchar(hcl_val) == 6,
    nchar(hcl) == 7,
    ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
    nchar(pid) == 9,
  )

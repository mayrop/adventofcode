library(tidyverse)

input.prod <- read_file("data/day03/prod.txt")
input.test <- read_file("data/day03/dev.txt")

input <- input.test
input <- input.prod

# Part 1

##------------------------------

find_trees <- function(rows, n_row, jump_x, jump_y) {
  cont <- 1
  chars <- c()
  
  for (i in seq(1 + jump_y, n_row, jump_y)) {
    y <- ((cont * jump_x) + 1) %% ncol(rows)
    y <- ifelse(y == 0, ncol(rows), y)
    chars <- c(chars, rows[i, y])
    cont <- cont + 1
  }
  
  chars
}

##------------------------------

df <- strsplit(input, "\n")[[1]] %>%
  as_tibble()

rows <- strsplit(df$value, "") %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  as.matrix() %>% 
  t()

colnames(rows) <- paste("c_", 1:ncol(rows), sep="")
rownames(rows) <- paste("c_", 1:nrow(rows), sep="")
rows

trees <- find_trees(rows, nrow(df), 3, 1)
trees
sum(trees == "#")

################################################

# Part 2
jumps <- rbind(c(1, 1), c(3, 1), c(5, 1), c(7, 1), c(1, 2))
trees <- c()

for (i in 1:nrow(jumps)) {
  chars <- find_trees(rows, nrow(df), jumps[i, 1], jumps[i, 2])
  trees <- c(trees, sum(chars=="#"))
}

trees
prod(trees)

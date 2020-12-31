library(tidyverse)

################################################################

# Functions

convert_to_matrix <- function(input) {
  mat <- strsplit(input, "") %>% 
    as.data.frame() %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    as.matrix() %>% 
    t()
  
  colnames(mat) <- paste("c_", 1:ncol(mat), sep="")
  rownames(mat) <- paste("c_", 1:nrow(mat), sep="")
  
  mat
}


find_trees <- function(mat, jump_x, jump_y) {
  cont <- 1
  chars <- c()
  
  for (i in seq(1 + jump_y, nrow(mat), jump_y)) {
    j <- ((cont * jump_x) + 1) %% ncol(mat)
    
    if (j == 0) {
      j = ncol(mat)
    }
    
    chars <- c(chars, mat[i, j])
    cont <- cont + 1
  }
  
  chars
}

################################################################

# Read Data

input.prod <- read_file("../data/day03/prod.txt")
input.test <- read_file("../data/day03/dev.txt")

input <- input.test
input <- input.prod

# Transform data
input <- strsplit(input, "\n")[[1]]

################################################################

# Part 1

mat <- convert_to_matrix(input)
trees <- find_trees(mat, 3, 1)
  
sum(trees == "#")

################################################################

# Part 2
jumps <- rbind(c(1, 1), c(3, 1), c(5, 1), c(7, 1), c(1, 2))

sapply(1:nrow(jumps), function(i) {
  temp <- find_trees(mat, jumps[i, 1], jumps[i, 2])
  sum(temp == "#")
}) %>% prod()

################################################################

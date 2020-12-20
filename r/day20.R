library(tidyverse)

################################################################
# Functions
find_matching_edges <- function(edge, current_edge, ind) {
  matching_edges <- list()
  current_edge <- current_edge %>% as.character()
  
  for (i in names(edges)) {
    if (i == current_edge) {
      next
    }
    
    # loop through the 4 edges
    for (j in 1:4) {
      if (sum(edge == edges[[i]][j,]) == 10
          | sum(rev(edge) == edges[[i]][j,]) == 10
          | sum(edge == rev(edges[[i]][j,])) == 10
          | sum(rev(edge) == rev(edges[[i]][j,])) == 10) {
        matching_edges[[as.character(i)]] = list(j=j, i=ind)
      }
    } 
  }
  
  matching_edges
}

# This function will take an array of strings and will return 
# a matrix with each character in separate cols & rows
convert_to_matrix <- function(strings) {
  matrix <- strsplit(strings, "") %>% 
    as.data.frame() %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    as.matrix() %>% 
    t()
  
  colnames(matrix) <- paste("c_", 1:ncol(matrix), sep="")
  rownames(matrix) <- paste("c_", 1:nrow(matrix), sep="")
  
  matrix
}
################################################################

# Read Data
input.prod <- read_file("../data/day20/prod.txt")
input.test <- read_file("../data/day20/dev.txt")

input <- input.test
input <- input.prod

# Transform Data
tiles_input <- strsplit(input, "\n\n")[[1]] 

# Getting properties by each tile in a list
tiles <- map(tiles_input, function(.x) {
  tile <- strsplit(.x, "\n")[[1]]
  m <- convert_to_matrix(tile[-1])
  
  tile_edges <- rbind(
    m[1, ], # first row (top)
    m[nrow(m), ], # last row (bottom)
    m[, 1], # left col
    m[, ncol(m)] # right col
  )
  
  tile_name <- tile[1] %>%
    gsub("[A-Za-z ]+|:", "", .) %>%
    as.numeric()
  
  list(
    matrix=m,
    name=tile_name,
    edges=tile_edges
  )
})

# Applyting nice names
names(tiles) <- map_dbl(tiles, ~.$name)

# Getting the edges by each tile inside a list
edges <- map(tiles, ~.$edges)

# Adding the matching edges to each tile
tiles <- map(tiles, function(.tile) {
  # Need to see which of the 4 edges have matches
  matching_edges <- lapply(1:4, function(i) {
    find_matching_edges(.tile$edges[i, ], .tile$name, i)
  })

  # Change data structure to a nice tibble for all the edges
  .tile$matching_edges <- map_df(matching_edges, function(.edges) {
    if (length(.edges) == 0) {
      return(tibble())     
    }
    nname <- names(.edges)
    tibble(
      name=nname, 
      i=.edges[[nname]]$i, 
      j=.edges[[nname]]$j
    ) 
  })
  
  .tile
})

################################################################

# Part 1

# We can assume that corners will have only 2 edges :) (thank God!)
which(map(tiles, ~.$matching_edges %>% nrow()) == 2) %>% 
  names() %>% 
  as.numeric() %>%
  prod()

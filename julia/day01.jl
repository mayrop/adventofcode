#Pkg.add("StatsBase")
#Pkg.add("DataFrames")
using DataFrames
using StatsBase

f = open("data/day01/dev.txt", "r")
lines = readlines(f)
# few file operations
close(f)

lines = parse.(Int64, lines)

#-------------------------------------------------------

# Part 1

# Option 1
# https://stackoverflow.com/questions/44393145/best-way-to-subtract-vector-from-matrix-in-julia/44393263
prod(intersect(2020 .- lines, lines))

# Option 2
# for finding the indexes
indexes = findall(in(2020 .- lines), lines)
lines[indexes]

# Option 3
# concatenate 2 vectors
# https://stackoverflow.com/questions/39586830/concatenating-arrays-in-julia
# chain filter: https://discourse.julialang.org/t/function-chaining-with-and-filter-function/17060
vcat(2020 .- lines, lines) |>
    countmap |>
    x -> filter(p -> last(p) == 2, x) |>
    keys |>
    prod

#-------------------------------------------------------

# Part 2
df = vec(collect(Base.product(lines, lines, lines))) |>
    DataFrame

# Changing colnames
# https://stackoverflow.com/questions/21559683/how-do-you-change-multiple-column-names-in-a-julia-version-0-3-dataframe
rename!(df, [:x, :y, :z], makeunique=true)

df.sum = df.x + df.y + df.z

filter(p -> p.sum == 2020, df)

# Get first row, cols 1 to 3 and run product on them
filter(p -> p.sum == 2020, df)[1, 1:3] |> prod

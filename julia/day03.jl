using DataFrames
using StatsBase
using Printf

#-------------------------------------------------------
# Functions

function find_trees(df, jump_x, jump_y)
    cont = 1
    chars = Vector{Char}()
    n_col = size(df, 2)
    n_row = size(df, 1)

    foreach([(1 + jump_y):jump_y:n_row;]) do (i)
        j = mod(((cont * jump_x) + 1), n_col)

        if j == 0
            j = n_col
        end

        append!(chars, df[i,j])
        cont += 1
    end

    chars
end

#-------------------------------------------------------

# Read Data
f = open("data/day03/prod.txt", "r")
lines = readlines(f)
# few file operations
close(f)

#-------------------------------------------------------

# Transform Data

df = DataFrame(raw=lines)
cols = split.(df.raw, "")
n_col = length(cols[1])

foreach(1:n_col) do (i)
    col = "col$(i)"
    df[!, Symbol(col)] .= ""
end

# Looping rows
foreach(1:size(df, 1)) do (i)
    row = cols[i]
    # Looping columns
    foreach(1:n_col) do (j)
        col = "col$(j)"
        df[i, Symbol(col)] = row[j]
    end
end

# Remove raw column
select!(df, Not(:raw))

#-------------------------------------------------------

# Part 1
chars = find_trees(df, 3, 1)
filter(c -> c == '#', chars) |> length

#-------------------------------------------------------

# Part 2
jumps = [[1,1], [3, 1], [5, 1], [7, 1], [1, 2]]
map(jump -> (
    find_trees(df, jump[1], jump[2]) |>
    chars -> filter(c -> c == '#', chars) |>
    length
), jumps) |> prod

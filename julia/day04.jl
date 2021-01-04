using DataFrames
using DataFramesMeta

#-------------------------------------------------------

# Read Data
f = open("data/day04/prod.txt", "r")
file = read(f, String)
# few file operations
close(f)

# https://datascience-enthusiast.com/R/R_Julia_cheat_sheet.html
#-------------------------------------------------------
lines = split(file, "\n\n")
lines = map(line -> replace(line, "\n" => " "), lines)

df = DataFrame(raw=lines)

# Regex: https://gist.github.com/dataPulverizer/23c8d992d351d7faf0ed1c1966605b10
foreach([:pid, :hgt, :byr, :iyr, :eyr, :hcl, :ecl]) do (col)
   df[!, col] .= missing
   # https://discourse.julialang.org/t/replacing-missing-values-in-dataframe-convert-type-union-float64-is-ambiguous/52119
   df[!, col] = convert(Vector{Union{String, Missing}}, df[!, col])

   foreach(1:size(df, 1)) do (i)
      col_match = match(Regex("$(col):([^ ]+)"), df[i, :raw])
      if col_match !== nothing
         df[i, col] = String(col_match[1])
      end
   end
end

#-------------------------------------------------------

# Part 1
dims = dropmissing(df) |> size
dims[1]


#-------------------------------------------------------

# For later
# https://stackoverflow.com/questions/58672335/julia-quickly-make-a-column-wise-sum-over-an-array
# https://stackoverflow.com/questions/58672335/julia-quickly-make-a-column-wise-sum-over-an-arraymatr
# https://www.juliabloggers.com/data-wrangling-in-julia-based-on-dplyr-flights-tutorials/

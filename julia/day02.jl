using DataFrames
using StatsBase

# Read Data
f = open("data/day02/prod.txt", "r")
lines = readlines(f)
# few file operations
close(f)

#-------------------------------------------------------

df = DataFrame(raw=lines)
cols = split.(df.raw)

# Separate columns
# https://stackoverflow.com/questions/57599906/how-to-separate-a-dataframe-column-in-two-given-a-delimiter
foreach(enumerate([:min_max, :letter, :password])) do (i, col)
   df[!, col] = getindex.(cols, i)
end

# Insert min/max
min_max = split.(df.min_max, "-")
foreach(enumerate([:min, :max])) do (i, col)
   df[!, col] = getindex.(min_max, i)
end

# Casting
df.min = parse.(Int64, df.min)
df.max = parse.(Int64, df.max)

# Clean rest of columns
df.letter = replace.(df.letter, ":" => "")

# How to insert columns
# https://stackoverflow.com/questions/58002144/how-to-assign-value-to-a-column-while-iterating-over-a-dataframe-using-for-eachr
# insertcols!(df, 3, :occurences => 0)
df[!, :substrings] .= 0

foreach(1:size(df)[1]) do (i)
   dict1 = countmap([c for c in df[i, :password]])
   # get(dict1, "s", 0) different than get(dict1, 's', 0)
   # https://stackoverflow.com/questions/59946081/julia-convert-string-to-char-or-convert-arraysubstringstring-1-to-char
   df[i, :substrings] = get(dict1, only(df[i, :letter]), 0)
end

#-------------------------------------------------------

# Part 1
valid_df = filter(p -> p.substrings >= p.min && p.substrings <= p.max, df)
size(valid_df, 1)

#-------------------------------------------------------

df[!, :min_char] .= false
df[!, :max_char] .= false
df[!, :is_valid] .= false

# Part 2

foreach(1:size(df)[1]) do (i)
   # get(dict1, "s", 0) different than get(dict1, 's', 0)
   # https://stackoverflow.com/questions/59946081/julia-convert-string-to-char-or-convert-arraysubstringstring-1-to-char
   df[i, :min_char] = SubString(df[i, :password], df[i, :min], df[i, :min]) == df[i, :letter]
   df[i, :max_char] = SubString(df[i, :password], df[i, :max], df[i, :max]) == df[i, :letter]
   df[i, :is_valid] = df[i, :min_char] ⊻ df[i, :max_char]
end

valid_df = filter(p -> p.is_valid, df)
size(valid_df, 1)

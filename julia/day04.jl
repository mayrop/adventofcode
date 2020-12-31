using DataFrames

#-------------------------------------------------------

# Read Data
f = open("data/day04/prod.txt", "r")
file = read(f, String)
# few file operations
close(f)

#-------------------------------------------------------
lines = split(file, "\n\n")
lines = map(line -> replace(line, "\n" => " "), lines)

df = DataFrame(raw=lines)
df[!, :is_valid] .= false

# Regex: https://gist.github.com/dataPulverizer/23c8d992d351d7faf0ed1c1966605b10
foreach([:pid, :hgt, :byr, :iyr, :eyr, :hcl, :ecl]) do (col)
   df[!, col] .= ""

   foreach(1:size(df, 1)) do (i)
      col_match = match(Regex("$(col):([^ ]+)"), df[i, :raw])
      if col_match !== nothing
         df[i, col] = String(col_match[1])
      end
   end
end

foreach(1:size(df, 1)) do (i)
   line = df[i, :]
   # How to clean this?
   is_valid = (line.pid != "" && line.hgt != "" && line.byr != "" && line.iyr != ""
      && line.eyr != "" && line.hcl != "" && line.ecl != "")
   df[i, :is_valid] = is_valid
end

#-------------------------------------------------------

# Part 1
dims = filter(line -> line.is_valid, df) |> size
dims[1]

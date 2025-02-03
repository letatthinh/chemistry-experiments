library(stringr)

# Column index in excel start from 1
# "A" = 1, B = "2", ...
get_column_index <- function(column_name) {
  column_index <- 0
  
  # In R, index start from 1
  for (index in 1:nchar(column_name)) {
    # Extract character at index from column_name
    character <- str_sub(column_name, index, index)
    
    # Convert character to number
    # Since the column index in excel start from 1, we plus 1
    # ('A' = 1, 'B' = 2, ..., 'Z' = 26)
    character_number <- utf8ToInt(character) - utf8ToInt("A") + 1
    
    # Calculate the index
    column_index <- (column_index * 26) + character_number
  }
  
  return(column_index)
}

print(get_column_index("A"))  # 1
print(get_column_index("Z"))  # 26
print(get_column_index("AA")) # (1 * 26) + 1 = 27
print(get_column_index("ZZ")) # (26 * 26) + 26 = 702

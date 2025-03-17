# Methods support working with data in the excel file
Excel_Utility <- R6Class(
  # Class name
  "Excel_Utility",
  
  # Public properties and methods
  public = list(
    # Get value from a cell (e.g. A12)
    get_cell_value = function(df, cell_name) {
      # Extract column name (e.g. A)
      column_name <- str_extract(cell_name, "[A-Z]+")
      
      # Extract row index (e.g. 12)
      row_index <- as.numeric(str_extract(cell_name, "[0-9]+"))
      
      if (is.na(column_name)) {
        stop("Wrong cell format! Missing column name")
      }
      
      if (is.na(row_index)) {
        stop("Wrong cell format! Missing row index")
      }
      
      # Convert column letters to numeric index
      column_index <- self$get_column_index(column_name)
      
      # Return the simplified cell value (a string or a number) with drop = TRUE
      return(df[row_index, column_index, drop = TRUE])
    },
    
    # Convert columns from character type to numeric type
    # column_indices is a vector of column names, e.g. c(1, 3)
    convert_column_type_to_numeric = function(df, column_indices = NULL) {
      # If column_indices is NULL: covert all columns to numeric
      if (is.null(column_indices)) {
        df[] <- lapply(df, as.numeric)
      } else {
        # Else, only convert specified columns by indices
        # NAs will be introduced if the data are not numeric.
        df[, column_indices] <- lapply(df[column_indices], as.numeric)
      }
      
      return(df)
    },
    
    # Get the main df by row indices
    get_column_index = function(column_name) {
      # Set default column index = 0
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
  )
)

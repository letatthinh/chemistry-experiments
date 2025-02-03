# Notes:
#   - Set the working directory to the main.R file



# LOAD LIBRARIES ---------------------------------------------------------------
library(R6)      # Create classes in R
library(readxl)  # Read excel files
library(stringr) # Work with strings



# SETTINGS ---------------------------------------------------------------------
# Note: Only general settings go here
INFILE <- "expt06.xlsx"
EXPERIMENT_NAME <- "Experiment 6:  Limiting Reactants"



# ENUMS ------------------------------------------------------------------------
# Note: Using list in R creates an object like in Javascript, or Dictionary in 
# Python
File_Extension <- list(xlsx="xlsx", csv="csv")



# UTILITIES --------------------------------------------------------------------
# Note: Only functions that support you in doing something in multiple places go
# here
# Get column index of a column in the excel file by name
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

# Get value from a cell (e.g. A12)
get_cell_value <- function(df, cell_name) {
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
  column_index <- get_column_index(column_name)
  
  # Return the simplified cell value with drop = TRUE
  return(df[row_index, column_index, drop = TRUE])
}



# CLASSES ----------------------------------------------------------------------
# Define a base experiment class (parent class)
# Other experiment classes should inherit from this one
Base_Experiment <- R6Class(
  # Class name
  "Base_Experiment",
  
  # Public properties and methods
  public = list(
    # Store the data from file
    df = NULL,
    
    # Constructor - Read excel file as default
    initialize = function(
      # input file path
      file_path,
      # TRUE to use the first row as column names
      col_names = FALSE
    ) {
      # self keyword helps reference to class properties [and methods ?]
      self$df <- read_excel(file_path, col_names=col_names)
    },
    
    # Check if the experiment name is provided in cell A2
    has_experiment_name = function(experiment_name) {
      cell_value <- get_cell_value(self$df, "A2")
      return(cell_value == experiment_name)
    }
  )
)

# Define Experiment_6 class that inherits from Base_Experiment
Experiment_6 <- R6Class(
  "Child",
  # Make it inherit from Base_Experiment class using the inherit property
  inherit = Base_Experiment,
  
  # Public properties and methods
  public = list(
    # Constructor - Read excel file
    initialize = function(file_path) {
      # Call parent's constructor
      super$initialize(file_path)
    }
  )
)



# MAIN -------------------------------------------------------------------------
# This is where the code run
# Create an object of the Experiment_6 class using the new() function
experiment_6 <- Experiment_6$new(INFILE)

# Requirement 1.a: Check if the experiment has a matching name in cell A2; 
# exit if no title is found or not matching the name.
if (!experiment_6$has_experiment_name(EXPERIMENT_NAME)) {
  stop("The experiment name in cell A2 is not correct or not provided.")
}


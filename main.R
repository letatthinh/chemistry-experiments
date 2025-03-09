# Notes:
#   - Set the working directory to the main.R file



# LOAD LIBRARIES ---------------------------------------------------------------
# Function to check and install a package if missing
load_or_install_package <- function(package_name) {
  # requireNamespace will check if the package is exist only, it doesn't load
  # the package into memory
  if (!requireNamespace(package_name)) {
    install.packages(package_name)
  }
  
  # Load the package after installation 
  # character.only: package will be passed in by a string
  library(package_name, character.only = TRUE)
}

# For working with OOP in R
load_or_install_package("R6")
# # For working with strings
load_or_install_package("stringr")
# # For working with data
load_or_install_package("dplyr")
# # For working with graphs
load_or_install_package("ggplot2")
# # For reading or writing data to excel file
load_or_install_package("openxlsx2")



# SETTINGS ---------------------------------------------------------------------
# Note: Only general settings go here
# File name
INFILE <- "expt06.xlsx"
# Should match value in cell A2
EXPERIMENT_NAME <- "Experiment 6:  Limiting Reactants"
# Start row index of main experiment data in the excel sheet
MAIN_DF_START_ROW_INDEX <- 9
# End row index of main experiment data in the excel sheet
MAIN_DF_END_ROW_INDEX <- 32



# ENUMS ------------------------------------------------------------------------
# Note: Using list in R creates an object like in Javascript, or Dictionary in 
# Python
File_Extension <- list(xlsx="xlsx", csv="csv")



# UTILITY CLASSSES -------------------------------------------------------------
# Note: Only functions that support you in doing something in multiple places go
# here
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
      column_index <- private$get_column_index(column_name)
      
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
        df[column_indices] <- lapply(df[column_indices], as.numeric)
      }
      
      return(df)
    }
  ),
  private = list(
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



# EXPERIMENT CLASSES -----------------------------------------------------------
# Define a base experiment class (parent class)
# Other experiment classes should inherit from this one
Base_Experiment <- R6Class(
  # Class name
  "Base_Experiment",
  
  # Public properties and methods
  public = list(
    # Sheet data
    sheet_df = NULL,
    # Main data
    main_df = NULL,
    # Excel utility
    excel_utility = Excel_Utility$new(),

    # Constructor - Read excel file as default
    initialize = function(
      # Input file path
      file_path,
      # TRUE to use the first row as column names
      has_column_names = FALSE
    ) {
      # 'self' keyword helps reference to class properties and methods
      self$sheet_df <- read_excel(file_path, col_names=has_column_names)

      # Requirement 1.a: Check if the experiment has a matching name in cell A2;
      # exit if no title is found or not matching the name
      self$check_experiment_name()
    },

    # Check if the experiment name is provided in cell A2
    # Must always check the experiment name in all experiments
    check_experiment_name = function() {
      cell_value <- self$excel_utility$get_cell_value(self$sheet_df, "A2")

      # Stop if experiment name in cell A2 is NA or contain only space
      # characters
      if (is.na(cell_value) || trimws(cell_value) == "") {
        stop("The experiment name in cell A2 is not provided.")
      }

      # Stop if experiment name in cell A2 is not correct
      if (cell_value != EXPERIMENT_NAME) {
        stop(paste0(
          "The experiment name in cell A2 is not correct. ",
          "The value should be '", EXPERIMENT_NAME, "'."))
      }
    },

    # Get the main df by row indices
    get_main_df = function(start_row_index, end_row_index) {
      # Select main data from sheet data and convert it to data frame
      self$main_df <- as.data.frame(
        self$sheet_df[start_row_index:end_row_index, ]
      )
      # Set the first column as row names
      rownames(self$main_df) <- self$main_df[[1]]
      # Remove the first column from the main df
      self$main_df <- self$main_df[, -1]
    }
  )
)

# Define Experiment_6 class that inherits from Base_Experiment
Experiment_6 <- R6Class(
  "Experiment_6",
  # Make it inherit from Base_Experiment class using the inherit property
  inherit = Base_Experiment,

  # Public properties and methods
  public = list(
    # Boolean vector for checking missing values in column B
    chkB = NULL,
    # Boolean vector for checking chemical compound mass values in
    # column B
    chk_mass = NULL,
    # Boolean vector for checking missing values in column C
    chkC = NULL,
    # Boolean vector for checking crucible mass values in column C
    chk_crucible = NULL,
    # Boolean vector for checking missing values in column D
    chkD = NULL,
    # Boolean vector for checking ppt (precipitate ?) values in column D
    chk_ppt = NULL,
    # Molar mass of KOx?
    MM_KOx = 184.23,
    # Molar mass of CaOx?
    MM_CaOx = 146.11,
    # Default value of the expected KOx moles
    default_expected_KOx_moles = 1.125e-3,
    # A vector of expected KOx moles by picking the smaller one between F and 
    # default value of the expected KOx moles
    expected_KOx_moles = NULL,

    # Constructor - Read excel file
    initialize = function(file_path) {
      # Call parent's constructor
      super$initialize(file_path)
    },

    # Override the get main df in parent
    get_main_df = function(start_row, end_row) {
      # Call the get_main_df() function from parent
      super$get_main_df(start_row, end_row)
      # Rename columns
      names(self$main_df) <- c("B", "C", "D", "E", "F", "G")
      # Convert string data to numeric data
      self$main_df <- self$excel_utility$convert_column_type_to_numeric(
        self$main_df
      )
    },

    # Check for missing data
    # Consition False if data is missing and True otherwise
    check_missing_data = function() {
      self$chkB <- !is.na(self$main_df$B)
      self$chkC <- !is.na(self$main_df$C)
      self$chkD <- !is.na(self$main_df$D)
    },

    # Check chemical compound mass data
    # False if not NA and (negative or not in range (0.080, 0.340))
    check_chemical_compound_mass = function() {
      self$chk_mass <- !(self$main_df$B < 0 |
         self$main_df$B < 0.080 | self$main_df$B > 0.340) & self$chkB
    },

    # Check crucible mass data
    # False if not NA and negative
    check_crucible_mass = function() {
      self$chk_crucible <- !(self$main_df$C < 0) & self$chkC
    },

    # Check ppt (precipitate ?) mass data
    # False if not NA and (negative or mass in column D < mass in column C)
    # column C must also pass the validation
    check_precipitate_mass = function() {
      self$chk_ppt <- !(self$main_df$D < 0 |
        self$main_df$D < self$main_df$C) & self$chkD & self$chkC
    },

    # E = D - C
    calculate_produced_chemical_compound_mass = function() {
      self$main_df$E <- ifelse(
        # If columns C and D passed the validation
        self$chk_crucible & self$chk_ppt,
        # Do the substractio
        self$main_df$D - self$main_df$C,
        # Else, assign NA
        NA
      )
    },

    # F = B / MM_KOx
    calculate_KOx_moles = function() {
      self$main_df$F <- ifelse(
        # If column B passed the validation
        self$chk_mass,
        # Do the division
        self$main_df$B / self$MM_KOx,
        # Else, assign NA
        NA
      )
    },
    
    # Set expected KOx moles? (the smaller one between F and 1.125e-3)
    # Note: not sure if this is the expected F or G
    set_expected_KOx_moles = function() {
      self$expected_KOx_moles <- ifelse(
        # If column F is not NA
        !is.na(self$main_df$F),
        # Pick the smaller value between F and 1.125e-3
        pmin(self$main_df$F, self$default_expected_KOx_moles),
        # Else, assign NA
        NA
      )
    },

    # G = E / MM_CaOx
    calculate_CaOx_moles = function() {
      self$main_df$G <- ifelse(
        # If column E is not NA
        !is.na(self$main_df$E),
        # Do the division
        self$main_df$E / self$MM_CaOx,
        # Else, assign NA
        NA
      )
    },

    # Create scatter plot
    create_KOx_and_CaOx_scatter_plot = function() {
      # Extract column G and F into a new data frame 
      F_and_G_df <- self$main_df[, c("F", "G")]
      # Add red color for outliers, and green for valid data
      F_and_G_df$color <- ifelse(
        F_and_G_df$G < 0.8 * self$expected_KOx_moles | 
        F_and_G_df$G > 1.2 * self$expected_KOx_moles,
        "red", 
        "green"
      )
      # Convert row names into labels in a new column
      F_and_G_df$label <- rownames(F_and_G_df)
      # Remove NAs
      cleaned_F_and_G_df <- na.omit(F_and_G_df)
      # Multiply each of F and G columns by 1000
      cleaned_F_and_G_df[, c("F", "G")] <- 
        cleaned_F_and_G_df[, c("F", "G")] * 1000
      
      # Create scatter plot
      ggplot(cleaned_F_and_G_df,
             aes(x = F,
                 y = G,
                 color = color)
      ) +
        geom_point(size = 5, alpha = 0.8) +
        # Add labels
        geom_text(aes(label = label), 
                  color = "black",
                  vjust = -1, 
                  hjust = 0.5) +
        # Use colors directly from the data
        scale_color_identity() +
        labs(
          x = expression(K[2]*C[2]*O[4] ~ "\u00b7" ~ H[2]*O/mmol),
          y = expression(CaC[2]*O[4] ~ "\u00b7" ~ H[2]*O/mmol),
        ) +
        theme_classic() +
        theme(
          panel.grid.major = element_line(
            color = "#e2e8f0",
            linewidth = 0.5,
            linetype = 1
          )
        )
    }
  )
)



# MAIN -------------------------------------------------------------------------
# This is where the code run
# Create an object of the Experiment_6 class using the new() function
# Requirement 1.a
experiment <- Experiment_6$new(INFILE)
# Requirement 1.b:
experiment$get_main_df(MAIN_DF_START_ROW_INDEX, MAIN_DF_END_ROW_INDEX)
experiment$check_missing_data()
# Requirement 1.c:
experiment$check_chemical_compound_mass()
# Requirement 1.d:
experiment$check_crucible_mass()
# Requirement 1.e:
experiment$check_precipitate_mass()
# Requirement 2.a:
experiment$calculate_produced_chemical_compound_mass()
# Requirement 2.b:
experiment$calculate_KOx_moles()
# Requirement 3.a:
experiment$set_expected_KOx_moles()
# Requirement 2.c:
experiment$calculate_CaOx_moles()
# Requirement 2.d + 3.a:
experiment$create_KOx_and_CaOx_scatter_plot()

# Requirement 4.a:
# Load the excel file
# wb <- loadWorkbook(INFILE)

print(experiment$main_df)

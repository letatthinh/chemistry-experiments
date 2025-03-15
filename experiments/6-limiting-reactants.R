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
    initialize = function() {
      # Set infile
      self$infile <- "expt06.xlsx"
      # Set main_sheet_name
      self$main_sheet_name <- "class data"
      # Set experiment_name
      self$experiment_name <- "Experiment 6:  Limiting Reactants"
      # Set main_df_start_row_index
      self$main_df_start_row_index <- 9
      # Set main_df_end_row_index
      self$main_df_end_row_index <- 32
      # Call parent's constructor
      super$initialize()
    },
    
    # Override the get main df in parent
    get_main_df = function() {
      # Call the get_main_df() function from parent
      super$get_main_df()
      
      # Rename columns (from A to G)
      names(self$main_df) <- LETTERS[
        which(LETTERS == "A"):which(LETTERS == "G")
      ]
      
      # Convert string data to numeric data from column B to column G
      self$main_df <- self$excel_utility$convert_column_type_to_numeric(
        self$main_df,
        # Define column index range
        self$excel_utility$get_column_index("B"):
          self$excel_utility$get_column_index("G")
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
      # Extract column A, F and G into a new data frame 
      extracted_df <- self$main_df[, c("A", "F", "G")]
      # Add red color for outliers, and green for valid data
      extracted_df$color <- ifelse(
        extracted_df$G < 0.8 * self$expected_KOx_moles | 
          extracted_df$G > 1.2 * self$expected_KOx_moles,
        "red", 
        "green"
      )
      # Remove NAs
      cleaned_extracted_df <- na.omit(extracted_df)
      # Multiply each of F and G columns by 1000
      cleaned_extracted_df[, c("F", "G")] <- 
        cleaned_extracted_df[, c("F", "G")] * 1000
      
      # Create scatter plot
      self$plot <- ggplot(cleaned_extracted_df,
                          aes(x = F,
                              y = G,
                              color = color)
      ) +
        geom_point(size = 5, alpha = 0.8) +
        # Add labels
        geom_text(aes(label = A), 
                  color = "black",
                  vjust = -1, 
                  hjust = 0.5) +
        # Use colors directly from the data
        scale_color_identity() +
        labs(
          x = expression(K[2]*C[2]*O[4] ~ "\u00b7" ~ H[2]*O/mmol),
          y = expression(CaC[2]*O[4] ~ "\u00b7" ~ H[2]*O/mmol),
          title = "Result"
        ) +
        theme_classic() +
        theme(
          panel.grid.major = element_line(
            color = "#e2e8f0",
            linewidth = 0.5,
            linetype = 1
          )
        )
      
      self$plot
    },
    
    # Write plot to pdf
    write_plot_result = function() {
      ggsave("limiting_reactants.pdf", 
             plot = self$plot, 
             width = 12, 
             height = 8)
    },
    
    # Clean maindf - create data_result_df
    create_data_result_df = function() {
      # Create a copy of main_df and 
      new_main_df <- data.frame(self$main_df)
      
      # Round float values to 9 decimal numbers from column E to G
      start_column <- self$excel_utility$get_column_index("E")
      end_column <- self$excel_utility$get_column_index("G")
      new_main_df[, start_column:end_column] <- round(
        new_main_df[, start_column:end_column], 
        9
      )
      
      # Replace NA values in new_main_df with blanks
      new_main_df[is.na(new_main_df)] <- ""
      
      return(new_main_df)
    }
  )
)
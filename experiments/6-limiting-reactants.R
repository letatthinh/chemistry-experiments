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
    
    # Boolean vector for checking total mass of the crucible and the precipitate
    # mass in column D
    chk_ppt = NULL,
    
    # Boolean vector for checking valid amount of the compound created in 
    # column G
    chk_amount_created = NULL,
    
    # Molar mass of KOx
    MM_KOx = 184.23,
    
    # Molar mass of CaOx
    MM_CaOx = 146.11,
    
    # Default value of the expected KOx moles
    default_expected_KOx_moles = 1.125e-3,
    
    # A vector of expected KOx moles by picking the smaller one between F and 
    # default value of the expected KOx moles
    expected_KOx_moles = NULL,
    
    # Constructor - Read excel file
    initialize = function(infile, main_sheet_name, experiment_name) {
      # Set infile
      self$infile <- infile
      # Set main_sheet_name
      self$main_sheet_name <- main_sheet_name
      # Set experiment_name
      self$experiment_name <- experiment_name
      # Call parent's constructor
      super$initialize()
    },
    
    # Override the get main df in parent
    extract_main_df = function(start_row_index, end_row_index) {
      # Call the get_main_df() function in parent
      super$extract_main_df(start_row_index, end_row_index)
      
      # Rename columns (from A to G)
      names(self$main_df) <- LETTERS[
        which(LETTERS == "A"):which(LETTERS == "G")
      ]
      
      # Convert string data to numeric data from column B to column G
      # Note: Column index in main_df is similar to column index in Excel
      start_column_index <- self$excel_utility$get_column_index("B")
      end_column_index <- self$excel_utility$get_column_index("G")
      
      self$main_df[, start_column_index:end_column_index] <- lapply(
        self$main_df[start_column_index:end_column_index], 
        as.numeric)
    },
    
    # Check chemical compound mass data (column B)
    # Notes:
    #   - NA when chkB is FALSE.
    #   - FALSE when chkB is TRUE and (chemical compound mass data (B) is 
    #     negative or not in range [0.080, 0.340])
    check_chemical_compound_mass = function() {
      self$chk_mass <- ifelse(
        self$chkB,
        !(self$check_negative(self$main_df$B) | 
          self$main_df$B < 0.080 | 
          self$main_df$B > 0.340),
        # Note: NA indicates missing chemical compound mass data
        NA
      )
    },
    
    # Check crucible mass data (column C)
    # Notes
    #   - NA when chkC is FALSE.
    #   - FALSE when chkC is TRUE and crucible mass data (C) is negative.
    check_crucible_mass = function() {
      self$chk_crucible <- ifelse(
        self$chkC,
        !(self$check_negative(self$main_df$C)),
        NA
      )
    },
    
    # Check total mass of the crucible and the chemical compound created mass 
    # data (column D)
    # Notes:
    #   - NA when chkD is FALSE.
    #   - FALSE when chkD is TRUE and precipitate mass data (D) is negative or 
    #     smaller than crucible mass data (C)
    check_total_crucible_and_precipitate_mass = function() {
      # If chkD is TRUE and chkC is TRUE
      self$chk_ppt <- ifelse(self$chkD & self$chkC,
        # FALSE when D is negative or D < C
        ifelse(self$check_negative(self$main_df$D) | 
               self$main_df$D < self$main_df$C, 
          FALSE, 
          TRUE),
        # If chkD is TRUE and chkC is FALSE
        ifelse(self$chkD & !self$chkC,
          # FALSE when D is negative
          ifelse(self$check_negative(self$main_df$D), 
            FALSE, 
            TRUE),
          # If chkD is FALSE, assign NA
          NA)
      )
    },
    
    # Calculate produced chemical compound (precipitate) mass (column E)
    # E = D - C
    calculate_precipitate_mass = function() {
      self$main_df$E <- ifelse(
        # If mass data in columns C and D are valid
        self$chk_crucible & self$chk_ppt,
        # Do the substraction
        self$main_df$D - self$main_df$C,
        # Else, assign NA
        NA
      )
    },
    
    # Calculate KOx moles (column F)
    # F = B / MM_KOx
    calculate_KOx_moles = function() {
      self$main_df$F <- ifelse(
        # If mass data in column B is valid
        self$chk_mass,
        # Do the division
        self$main_df$B / self$MM_KOx,
        # Else, assign NA
        NA
      )
    },
    
    # Set expected KOx moles (the smaller one between F and 1.125e-3)
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
    
    # Calculate CaOx moles (column G)
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
    
    # Check amount of the compound created in the experiment (column G)
    # Notes:
    #   - NA when G is NA.
    #   - FALSE when G < 0.8 * expected_KOx_moles 
    #     or G > 1.2 * expected_KOx_moles
    check_valid_amount_created = function() {
      self$chk_amount_created <- ifelse(
        self$main_df$G >= 0.8 * self$expected_KOx_moles &
          self$main_df$G <= 1.2 * self$expected_KOx_moles,
        TRUE, 
        FALSE
      )
    },
    
    # Create scatter plot
    create_KOx_and_CaOx_scatter_plot = function() {
      # Extract column A, F and G into a new data frame 
      extracted_df <- self$main_df[, c("A", "F", "G")]
      
      # Add red color for outliers, and green for valid data
      extracted_df$color <- ifelse(
        self$chk_amount_created,
        "black",
        "red"
      )
      
      # Remove NAs
      cleaned_extracted_df <- na.omit(extracted_df)
      
      # Multiply each of F and G columns by 1000
      cleaned_extracted_df[, c("F", "G")] <- 
        cleaned_extracted_df[, c("F", "G")] * 1000
      
      # Create scatter plot
      self$plot <- ggplot(cleaned_extracted_df,
                          aes(x = F, y = G, color = color)) +
        geom_point(size = 5, alpha = 0.8) +
        # Add labels
        geom_text_repel(aes(label = A),
                  size = self$default_plot_font_size,
                  color = "black", 
                  vjust = -1.5, 
                  hjust = 0.5) +
        # Allow labels to overflow
        coord_cartesian(clip = "off") +
        # Use colors directly from the data
        scale_color_identity() +
        # Increase y axis to add some space near the top
        scale_y_continuous(limits = c(min(cleaned_extracted_df$G), 
                                      max(cleaned_extracted_df$G) + 0.05)) + 
        labs(x = expression(K[2]*C[2]*O[4] ~ "\u00b7" ~ H[2]*O/mmol),
             y = expression(CaC[2]*O[4] ~ "\u00b7" ~ H[2]*O/mmol)
        ) +
        theme_classic() +
        theme(axis.text = element_text(size = self$conversion_utility$mm_to_pt(
                  self$default_plot_font_size)),
              axis.title = element_text(size = self$conversion_utility$mm_to_pt(
                self$default_plot_font_size + 1.5)),
              plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
              axis.title.x = element_text(margin = margin(t = 10)),
              axis.title.y = element_text(margin = margin(r = 10)))
      
      self$plot
    },
    
    # Write plot to pdf
    write_plot_result = function() {
      ggsave("limiting_reactants.pdf", plot = self$plot, width = 12, height = 8)
    },
    
    # Create result df
    create_result_df = function() {
      # Create a copy of main_df and 
      result_df <- data.frame(self$main_df)
      
      # Round float values to 9 decimal numbers from column E to G
      start_column <- self$excel_utility$get_column_index("E")
      end_column <- self$excel_utility$get_column_index("G")
      number_of_decimal_numbers <- 9
      
      result_df[, start_column:end_column] <- round(
        result_df[, start_column:end_column], number_of_decimal_numbers
      )
      
      # Replace NA values in result_df with blanks
      result_df[is.na(result_df)] <- ""
      
      return(result_df)
    },
    
    # Write validity report
    write_validity_report = function() {
      # Specify the file path
      file_path <- "limiting_reactants.md"
      
      # Create if the file doesn't exist, overwrite otherwise
      file.create(file_path)
      
      # Open a connection to the file with writing mode
      # Ref: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/connections
      file_connection <- file(file_path, open = "a")
      
      # Close the file connection on exit
      on.exit(close(file_connection))
      
      # Report stations with missing data in columns B, C, and D
      private$write_missing_data_stations("B", self$chkB, file_connection)
      cat("\n\n", file = file_connection)
      private$write_missing_data_stations("C", self$chkC, file_connection)
      cat("\n\n", file = file_connection)
      private$write_missing_data_stations("D", self$chkD, file_connection)
      cat("\n\n", file = file_connection)
      
      # Report stations with invalid mass in columns B, C, and D
      private$write_invalid_mass_stations("B", self$chk_mass, file_connection)
      cat("\n\n", file = file_connection)
      private$write_invalid_mass_stations("C", self$chk_crucible, file_connection)
      cat("\n\n", file = file_connection)
      private$write_invalid_mass_stations("D", self$chk_ppt, file_connection)
      cat("\n\n", file = file_connection)
      
      # Report stations with mass of precipitate could not be calculated in 
      # column E
      # Check calculable ppt
      # Notes:
      #   - NA when chk_crucible and chk_ppt are both NA
      #   - FALSE when either chk_crucible or chk_ppt is FALSE,
      #   - TRUE when chk_crucible and chk_ppt are both TRUE
      chk_calculable_ppt <- ifelse(
        (self$chk_crucible == FALSE | self$chk_ppt == FALSE) | 
          (xor(is.na(self$chk_crucible), is.na(self$chk_ppt))),
        FALSE, 
        TRUE
      )
      
      private$write_incalculable_precipitate_mass_stations(
        chk_calculable_ppt, file_connection
      )
      
      cat("\n\n", file = file_connection)
      
      # Report stations that are outliers in column G
      private$write_invalid_amount_created(
        self$chk_amount_created, file_connection
      )
      
      # Remove empty lines at the bottom
      private$remove_empty_lines_in_report(file_path)
    }
  ),
  private = list(
    # Write to report the stations from a list provided
    write_stations = function(stations, file_connection) {
      for (index in 1:length(stations)) {
        # Add a new line if it is not the last row
        if (index < length(stations)) {
          cat("-", stations[index], "\n", 
              file = file_connection)
        } else {
          cat("-", stations[index], 
              file = file_connection)
        }
      }
    },
    
    # Write to the report the stations that are missing data in a column
    write_missing_data_stations = function(
      column_name,
      check_vector,
      file_connection
    ) {
      # Collect stations with missing data
      # Note: Station names are in column A
      missing_data_stations <- self$main_df$A[check_vector == FALSE]
      
      if (length(missing_data_stations) > 0) {
        cat("# Stations with missing data in column ", column_name, ":\n\n", 
            sep = "",
            file = file_connection)
        
        private$write_stations(missing_data_stations, file_connection)
      }
    },
    
    # Write to the report the stations that have invalid data in a column.
    write_invalid_mass_stations = function(
      column_name,
      check_vector,
      file_connection
    ) {
      # Collect stations with invalid mass data
      # Note: Station names are in column A
      invalid_mass_stations <- self$main_df$A[
        check_vector == FALSE & !is.na(check_vector)
      ]
      
      if (length(invalid_mass_stations) > 0) {
        cat("# Stations with invalid mass in column ", column_name, ":\n\n", 
            sep = "",
            file = file_connection)
        
        private$write_stations(invalid_mass_stations, file_connection)
      }
    },
    
    # Write to the report the stations having mass of precipitate could not be 
    # calculated
    write_incalculable_precipitate_mass_stations = function(
      check_vector,
      file_connection
    ) {
      # Collect stations with incalculable precipitate
      # Note: Station names are in column A
      incalculable_precipitate_stations <- self$main_df$A[
        check_vector == FALSE & !is.na(check_vector)
      ]
      
      if (length(incalculable_precipitate_stations) > 0) {
        cat("# Stations for which a mass of precipitate could not be",
            "calculated:\n\n",
            file = file_connection)
        
        private$write_stations(incalculable_precipitate_stations, 
                               file_connection)
      }
    },
    
    # Write to the report the stations having invalid amount of the compound 
    # created
    write_invalid_amount_created = function(check_vector, file_connection) {
      # Collect stations with incalculable precipitate
      # Note: Station names are in column A
      invalid_amount_created_stations <- self$main_df$A[
        check_vector == FALSE & !is.na(check_vector)
      ]
      
      if (length(invalid_amount_created_stations) > 0) {
        cat("# Stations for which the amount of precipitate is identified as an",
            "outlier:\n\n",
            file = file_connection)
        
        private$write_stations(invalid_amount_created_stations, 
                               file_connection)
      }
    },
    
    # Write to the report the stations having invalid amount of the compound 
    # created
    remove_empty_lines_in_report = function(file_path) {
      lines <- readLines(file_path, warn = FALSE)
      
      if (length(lines) > 0) {
        # Get from the first line till the last line with text
        trimmed_lines <- lines[1:max(which(lines != ""))]
        
        # Write back to the file
        writeLines(trimmed_lines, file_path)
      }
    }
  )
)
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
    initialize = function(infile = NULL,
                          experiment_name = NULL) {
      # Call parent's constructor
      super$initialize(infile = infile,
                       experiment_name = experiment_name)
    },
    
    # Override the get main df in parent
    set_main_df = function(start_row_index, end_row_index) {
      # Call the get_main_df() function in parent
      super$set_main_df(start_row_index, end_row_index)
      
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
    #   - TRUE otherwise
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
    #   - TRUE otherwise
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
      self$chk_ppt <- ifelse(
        self$chkD & self$chkC,
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
    
    # Create scatter plot
    create_KOx_and_CaOx_scatter_plot = function() {
      # Extract column A, F and G into a new data frame 
      extracted_df <- self$main_df[, c("A", "F", "G")]
      
      # Add red color for outliers, and black for valid data
      extracted_df$color <- ifelse(
        private$check_valid_amount_created(),
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
    write_validity_report = function(file_path) {
      # Create if the file doesn't exist, overwrite otherwise
      file.create(file_path)
      
      # Open a connection to the file with writing mode
      # Ref: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/connections
      file_connection <- file(file_path, open = "a")
      
      # Close the file connection on exit
      on.exit(close(file_connection))
      
      # Report stations that have missing data
      # Case 1: Stations with missing data in all 3 columns B, C, and D
      missing_data_in_all_BCD_stations <- 
        private$write_missing_data_in_BCD_stations(file_connection)
      # Case 2: At least 1 column has data among 3 columns B, C and D, report
      #         columns have missing data separately. Make sure these values are
      #         not in missing_data_in_all_BCD_stations
      private$write_missing_data_stations_by_column(
        "B",
        self$chkB, 
        missing_data_in_all_BCD_stations, 
        file_connection)
      private$write_missing_data_stations_by_column(
        "C",
        self$chkC, 
        missing_data_in_all_BCD_stations, 
        file_connection)
      private$write_missing_data_stations_by_column(
        "D", 
        self$chkD, 
        missing_data_in_all_BCD_stations, 
        file_connection)
      
      # Report stations with invalid mass in columns B, C, and D
      private$write_invalid_mass_stations_by_column("B", 
                                                    self$chk_mass, 
                                                    file_connection)
      private$write_invalid_mass_stations_by_column("C", 
                                                    self$chk_crucible, 
                                                    file_connection)
      private$write_invalid_mass_stations_by_column("D", 
                                                    self$chk_ppt, 
                                                    file_connection)
      
      # Report stations with mass of precipitate could not be calculated in 
      # column E
      
      private$write_incalculable_precipitate_mass_stations(
        private$check_calculable_ppt(), file_connection
      )
      
      # Report stations that are outliers in column G
      private$write_invalid_amount_created_stations(
        private$check_valid_amount_created(), file_connection
      )
      
      # Remove empty lines at the bottom
      private$remove_empty_lines_in_report(file_path)
    }
  ),
  private = list(
    # Check calculable ppt
    # Notes:
    #   - NA when chk_crucible and chk_ppt are both NA
    #   - FALSE when either chk_crucible or chk_ppt is FALSE,
    #   - TRUE when chk_crucible and chk_ppt are both TRUE
    check_calculable_ppt = function() {
      return(ifelse((self$chk_crucible == FALSE | self$chk_ppt == FALSE) |
                    xor(is.na(self$chk_crucible), is.na(self$chk_ppt)),
                    FALSE, 
                    TRUE))
    },
    
    # Check amount of the compound created in the experiment (column G)
    # Notes:
    #   - NA when G is NA.
    #   - FALSE when G < 0.8 * expected_KOx_moles 
    #     or G > 1.2 * expected_KOx_moles
    check_valid_amount_created = function() {
      return(ifelse(self$main_df$G >= 0.8 * self$expected_KOx_moles &
                    self$main_df$G <= 1.2 * self$expected_KOx_moles,
                    TRUE, 
                    FALSE))
    },
    
    # Write to report the stations from a list provided
    write_stations = function(stations, file_connection) {
      for (index in 1:length(stations)) {
        # Add a new line if it is not the last row
        if (index < length(stations)) {
          cat("-", stations[index], "\n", file = file_connection)
        } else {
          cat("-", stations[index], file = file_connection)
        }
      }
    },
    
    # Write to the report the stations that are missing data in all 3 columns B, 
    # C, and D
    write_missing_data_in_BCD_stations = function(
      file_connection
    ) {
      missing_data_in_all_BCD_stations <- self$main_df$A[
        which(!self$chkB & !self$chkC & !self$chkD)
      ]
      
      if (length(missing_data_in_all_BCD_stations) > 0) {
        cat("### Stations with missing data in all 3 columns B, C, and D:\n\n", 
            sep = "",
            file = file_connection)
        
        private$write_stations(missing_data_in_all_BCD_stations, 
                               file_connection)
        
        return(missing_data_in_all_BCD_stations)
      }
    },
    
    # Write to the report the stations that are missing data by column
    write_missing_data_stations_by_column = function(
      column_name,
      check_vector,
      exclude_stations,
      file_connection
    ) {
      # Collect stations in column A with missing data and not in the exclude
      # station list
      missing_data_stations <- self$main_df$A[which(
        !check_vector & !(self$main_df$A %in% exclude_stations)
      )]
      
      if (length(missing_data_stations) > 0) {
        cat("\n\n### Stations with missing data in column ", 
            column_name, 
            ":\n\n",
            sep = "",
            file = file_connection)
        
        private$write_stations(missing_data_stations, file_connection)
        
        return(missing_data_stations)
      }
    },
    
    # Write to the report the stations that have invalid data in a column.
    write_invalid_mass_stations_by_column = function(
      column_name,
      check_vector,
      file_connection
    ) {
      # Collect stations in column A with invalid mass data
      invalid_mass_stations <- self$main_df$A[
        check_vector == FALSE & !is.na(check_vector)
      ]
      
      if (length(invalid_mass_stations) > 0) {
        cat("\n\n### Stations with invalid mass data in column ", 
            column_name, 
            ":\n\n", 
            sep = "",
            file = file_connection)
        
        private$write_stations(invalid_mass_stations, file_connection)
        
        return(invalid_mass_stations)
      }
    },
    
    # Write to the report the stations having mass of precipitate could not be 
    # calculated (column E)
    write_incalculable_precipitate_mass_stations = function(
      check_vector,
      file_connection
    ) {
      # Collect stations in column A with incalculable precipitate
      incalculable_precipitate_stations <- self$main_df$A[
        check_vector == FALSE & !is.na(check_vector)
      ]
      
      if (length(incalculable_precipitate_stations) > 0) {
        cat("\n\n### Stations for which a mass of precipitate could not be",
            "calculated (column E):\n\n",
            file = file_connection)
        
        private$write_stations(incalculable_precipitate_stations, 
                               file_connection)
        
        return(incalculable_precipitate_stations)
      }
    },
    
    # Write to the report the stations having invalid amount of the compound 
    # created (column G)
    write_invalid_amount_created_stations = function(
      check_vector, 
      file_connection
    ) {
      # Collect stations in column A with incalculable precipitate
      invalid_amount_created_stations <- self$main_df$A[
        check_vector == FALSE & !is.na(check_vector)
      ]
      
      if (length(invalid_amount_created_stations) > 0) {
        cat("\n\n### Stations for which the amount of precipitate is",
            "identified as an outlier (column G):\n\n",
            file = file_connection)
        
        private$write_stations(invalid_amount_created_stations, 
                               file_connection)
        
        return(invalid_amount_created_stations)
      }
    },
    
    # Remove empty lines at the end from the report
    remove_empty_lines_in_report = function(file_path) {
      # Read the file
      lines <- readLines(file_path, warn = FALSE)
      
      if (length(lines) > 0) {
        # Trim leading empty lines at the top
        while (length(lines) > 0 && !grepl("\\S", lines[1])) {
          lines <- tail(lines, -1)
        }
        
        # Write the cleaned lines back to the file
        writeLines(lines, file_path)
        
        # Return the cleaned content
        return(lines)
      }
    }
  )
)
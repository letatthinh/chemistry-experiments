# Define a base experiment class (parent class)
# Other experiment classes should inherit from this one
Base_Experiment <- R6Class(
  # Class name
  "Base_Experiment",
  
  # Public properties and methods
  public = list(
    # Input file name
    infile = NULL,
    # Name of the experiment - should match value in cell A2
    experiment_name = NULL,
    # Sheet data
    sheet_df = NULL,
    # Main experiment data
    main_df = NULL,
    # Excel utility
    excel_utility = Excel_Utility$new(),
    # Conversion utility
    conversion_utility = Conversion_Utility$new(),
    # Plot variable
    plot = NULL,
    # Plot default font size (in mm)
    default_plot_font_size = 5,
    
    # Constructor - Read excel file as default
    initialize = function(
      infile = NULL,
      experiment_name = NULL,
    # has_column_names: indicate if the first row has column names
      has_column_names = FALSE
    ) {
      # Set infile
      self$infile <- infile
      
      # Set experiment_name
      self$experiment_name <- experiment_name
      
      # Stop if experiment name is not defined
      if (is.null(self$infile)) {
        stop(paste("The infile variable is not defined in child experiment", 
                   "class."))
      }
      
      # 'self' keyword helps reference to class properties and methods
      self$sheet_df <- read_xlsx(self$infile, col_names=has_column_names)
    },
    
    # Check if the experiment name is provided in cell A2
    check_experiment_name = function() {
      cell_a2_value <- self$excel_utility$get_cell_value(self$sheet_df, "A2")
      
      # Stop if experiment name is not defined
      if (is.null(self$experiment_name)) {
        stop("The experiment_name parameter is not defined")
      }
      
      # Stop if experiment name in cell A2 is NA or contain only space
      # characters
      if (is.na(cell_a2_value) || trimws(cell_a2_value) == "") {
        stop("The experiment name in cell A2 in the excel file is missing.")
      }
      
      # Stop if experiment name in cell A2 doesn't match the provided value
      if (cell_a2_value != self$experiment_name) {
        stop(paste0("The experiment name in cell A2 is incorrect. The value ",
                    "should be '", self$experiment_name, "'."))
      }
      
      return(TRUE)
    },
    
    # Get the main df by row indices
    set_main_df = function(start_row_index, end_row_index) {
      # Get main data from start_row_index to end_row_index
      self$main_df <- as.data.frame(self$sheet_df[
        start_row_index:end_row_index, 
      ])
    },
    
    # Check for missing mass data in a column
    # Condition is FALSE if data is missing and TRUE otherwise
    check_missing = function(mass_vector) {
      return(!is.na(mass_vector))
    },
    
    # Check for negative mass data in a column
    # Condition is TRUE if data is negative and FALSE otherwise
    check_negative = function(mass_vector) {
      return(mass_vector < 0)
    },
    
    # Write plot to pdf
    write_plot_result = function(file_path, width = 12, height = 8) {
      ggsave(file_path, plot = self$plot, width = width, height = height)
    },
    
    # Write result to a new sheet
    write_result = function(
      df,
      from_sheet_name,
      to_sheet_name,
      df_start_row_index,
      df_start_column_index = 1,
      file_path = self$infile
    ) {
      # Create workbook object
      # Ref: https://janmarvin.github.io/openxlsx2/reference/wb_load.html?q=wb_load#null
      workbook <- wb_load(self$infile)
      
      # Remove if the to_sheet_name exists
      if (to_sheet_name %in% workbook$get_sheet_names()) {
        workbook <- wb_remove_worksheet(workbook, to_sheet_name)
      }
      
      # Clone the main_sheet_name to to_sheet_name
      # Note: References to sheet names in formulas, charts, pivot tables, etc. 
      # may not be updated. Some elements like named ranges and slicers cannot 
      # be cloned yet.
      # Ref: https://janmarvin.github.io/openxlsx2/reference/wb_clone_worksheet.html
      workbook$clone_worksheet(from_sheet_name, new = to_sheet_name)
      
      # Write new_main_df
      # Ref: https://janmarvin.github.io/openxlsx2/reference/wb_add_data.html
      workbook$add_data(to_sheet_name,
                        df,
                        start_row = df_start_row_index,
                        start_col = df_start_column_index,
                        # Avoid writing column names of df
                        col_names = FALSE)
      
      workbook$save(file_path, overwrite=TRUE)
      
      return(TRUE)
    }
  )
)
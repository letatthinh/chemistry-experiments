# Define a base experiment class (parent class)
# Other experiment classes should inherit from this one
Base_Experiment <- R6Class(
  # Class name
  "Base_Experiment",
  
  # Public properties and methods
  public = list(
    # Input file name
    infile = NULL,
    # Name of the main sheet
    main_sheet_name = NULL,
    # Name of the experiment - should match value in cell A2
    experiment_name = NULL,
    # Sheet data
    sheet_df = NULL,
    # Main experiment data
    main_df = NULL,
    # Start row index of main experiment data in the excel sheet
    main_df_start_row_index = NULL,
    # End row index of main experiment data in the excel sheet
    main_df_end_row_index = NULL,
    # Excel utility
    excel_utility = Excel_Utility$new(),
    # Plot variable
    plot = NULL,
    
    # Constructor - Read excel file as default
    initialize = function(
    # has_column_names: indicate if the first row has column names
      has_column_names = FALSE
    ) {
      # 'self' keyword helps reference to class properties and methods
      self$sheet_df <- read_xlsx(self$infile, col_names=has_column_names)
      
      # Requirement 1.a: Check if the experiment has a matching name in cell A2;
      # exit if no title is found or not matching the name
      self$check_experiment_name()
    },
    
    # Check if the experiment name is provided in cell A2
    check_experiment_name = function() {
      cell_value <- self$excel_utility$get_cell_value(self$sheet_df, "A2")
      
      # Stop if experiment name is not defined
      if (is.null(self$experiment_name)) {
        stop(paste("The experiment_name variable is not defined", 
                   "in child experiment class."))
      }
      
      # Stop if experiment name in cell A2 is NA or contain only space
      # characters
      if (is.na(cell_value) || trimws(cell_value) == "") {
        stop("The experiment name in cell A2 is not provided.")
      }
      
      # Stop if experiment name in cell A2 is not correct
      if (cell_value != self$experiment_name) {
        stop(paste(
          "The experiment name in cell A2 is not correct. ",
          "The value should be '", self$experiment_name, "'."))
      }
    },
    
    # Get the main df by row indices
    get_main_df = function() {
      # Stop if row indices are not defined
      if (is.null(self$main_df_start_row_index) || 
          is.null(self$main_df_end_row_index)) {
        stop(paste("The main_df_start_row_index or main_df_end_row_index",
                   "variable is not defined in child experiment class."))
      }
      
      # Select main data from sheet data and convert it to data frame
      self$main_df <- as.data.frame(
        self$sheet_df[
          self$main_df_start_row_index:self$main_df_end_row_index, 
        ]
      )
    },
    
    # Write result to a new sheet
    write_result = function(
    new_sheet_name,
    df,
    df_start_row_index,
    df_start_column_index = 1
    ) {
      # Stop main sheet name is not defined
      if (is.null(self$main_sheet_name)) {
        stop(paste("The main_sheet_name variable is not defined in child",
                   "experiment class."))
      }
      
      # Create workbook object
      # Ref: https://janmarvin.github.io/openxlsx2/reference/wb_load.html?q=wb_load#null
      workbook <- wb_load(self$infile)
      
      # Remove if the new_sheet_name exists
      if (new_sheet_name %in% workbook$get_sheet_names()) {
        workbook <- wb_remove_worksheet(workbook, new_sheet_name)
      }
      
      # Clone the main_sheet_name to new_sheet_name
      # Note: References to sheet names in formulas, charts, pivot tables, etc. 
      # may not be updated. Some elements like named ranges and slicers cannot 
      # be cloned yet.
      # Ref: https://janmarvin.github.io/openxlsx2/reference/wb_clone_worksheet.html
      workbook$clone_worksheet(self$main_sheet_name, new = new_sheet_name)
      
      # Write new_main_df
      # Ref: https://janmarvin.github.io/openxlsx2/reference/wb_add_data.html
      workbook$add_data(
        new_sheet_name, 
        df,
        start_row = df_start_row_index,
        start_col = df_start_column_index,
        # Avoid writing column names of new_main_df
        col_names = FALSE)
      
      # Save the workbook (overwrite the existing file)
      workbook$save(self$infile, overwrite=TRUE)
    }
  )
)
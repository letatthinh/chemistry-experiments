test_that("Base experiment - Class initialization and properties", {
  infile_test <- "data/expt06.xlsx"
  experiment_name_test <- "Experiment 6:  Limiting Reactants"
  
  # Initialize class object
  experiment <- Base_Experiment$new(
    infile = infile_test,
    experiment_name = experiment_name_test
  )
  
  # Expect infile is set
  expect_equal(experiment$infile, infile_test)
  
  # Expect experiment_name is set
  expect_equal(experiment$experiment_name, experiment_name_test)
})



test_that("Base experiment - Data file path (infile) is not defined", {
  # Expect an error message
  expect_error(Base_Experiment$new(),
               paste("The infile variable is not defined in child experiment",
                     "class."))
})



test_that("Base experiment - Experiment name is not defined", {
  infile_test <- "data/expt06.xlsx"
  
  # Initialize class object
  experiment <- Base_Experiment$new(
    infile = infile_test
  )
  
  # Expect an error message
  expect_error(experiment$check_experiment_name(),
               "The experiment_name parameter is not defined")
})



test_that(paste("Base experiment - Experiment name in cell A2",
                "in the excel file is missing"), {
                  infile_test <- "data/experiment-name-not-provided-in-cell-A2.xlsx"
                  experiment_name_test <- "Experiment 6:  Limiting Reactants"
                  
                  # Initialize class object
                  experiment <- Base_Experiment$new(
                    infile = infile_test,
                    experiment_name = experiment_name_test
                  )
                  
                  # Expect an error message
                  expect_error(experiment$check_experiment_name(),
                               "The experiment name in cell A2 in the excel file is missing.")
                })



test_that("Base experiment - Experiment name in cell A2 is incorrect", {
  infile_test <- "data/incorrect-experiment-name-in-cell-A2.xlsx"
  experiment_name_test <- "Experiment 6:  Limiting Reactants"
  
  # Initialize class object
  experiment <- Base_Experiment$new(
    infile = infile_test,
    experiment_name = experiment_name_test
  )
  
  # Expect an error message
  expect_error(experiment$check_experiment_name(),
               paste0("The experiment name in cell A2 is incorrect. The value ",
                      "should be '", experiment$experiment_name, "'."))
})



test_that("Base experiment - Experiment name in cell A2 is correct", {
  infile_test <- "data/expt06.xlsx"
  experiment_name_test <- "Experiment 6:  Limiting Reactants"
  
  # Initialize class object
  experiment <- Base_Experiment$new(
    infile = infile_test,
    experiment_name = experiment_name_test
  )
  
  # Expect returning TRUE
  expect_true(experiment$check_experiment_name())
})



test_that("Base experiment - Set main df", {
  infile_test <- "data/expt06.xlsx"
  
  # Initialize class object
  experiment <- Base_Experiment$new(
    infile = infile_test
  )
  
  experiment$set_main_df(start_row_index = 9, end_row_index = 32)
  
  # Expect main_df has data
  expect_gt(nrow(experiment$main_df), 0)
})



test_that("Base experiment - Check missing mass data", {
  infile_test <- "data/expt06.xlsx"
  vector_test <- c(2, NA, 3)
  expected_vector_result <- c(TRUE, FALSE, TRUE)
  
  # Initialize class object
  experiment <- Base_Experiment$new(
    infile = infile_test
  )
  
  vector_result <- experiment$check_missing(vector_test)
  expect_setequal(vector_result, expected_vector_result)
})



test_that("Base experiment - Check negative mass data", {
  infile_test <- "data/expt06.xlsx"
  vector_test <- c(-2, NA, 3)
  expected_vector_result <- c(TRUE, NA, FALSE)
  
  # Initialize class object
  experiment <- Base_Experiment$new(
    infile = infile_test
  )
  
  vector_result <- experiment$check_negative(vector_test)
  expect_setequal(vector_result, expected_vector_result)
})



test_that("Base experiment - Write result", {
  infile_test <- "data/expt06.xlsx"
  # Create vectors for the 3 columns
  col1 <- c(0.231, 0.432, 0.112, 0.754, 0.765)
  col2 <- c(0.783, 0.567, 0.891, 0.634, 0.284)
  col3 <- c(0.874, 0.912, 0.451, 0.333, 0.567)
  # Combine the vectors into a DataFrame
  df <- data.frame(col1, col2, col3)
  
  # Initialize class object
  experiment <- Base_Experiment$new(
    infile = infile_test
  )
  
  # Specify the file path
  output_file_path <- "data/write-result-output.xlsx"
  
  if (file.exists(output_file_path)) {
    # Remove the write-result file
    file.remove(output_file_path)
  }
  
  expect_true(experiment$write_result(
    df = df,
    from_sheet_name = "class data",
    to_sheet_name = "complete",
    df_start_row_index = 9,
    file_path = output_file_path))
  
  # Get new sheetname data from the out file
  new_sheet_df <- read_xlsx(output_file_path, 
                            sheet = "complete", 
                            col_names=FALSE)
  
  # Check if cell B9 is euqal "0.783"
  cell_b9_value <- experiment$excel_utility$get_cell_value(new_sheet_df, 
                                                           "B9")
  expect_equal(cell_b9_value, "0.783")
})
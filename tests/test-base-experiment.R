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
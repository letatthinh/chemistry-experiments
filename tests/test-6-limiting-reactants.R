test_that("Base experiment - Class initialization and properties", {
  infile_test <- "data/expt06.xlsx"
  main_sheet_name_test <- "class data"
  experiment_name_test <- "Experiment 6:  Limiting Reactants"
  
  # Initialize class object
  experiment_6 <- Experiment_6$new(
    infile = infile_test,
    main_sheet_name = main_sheet_name_test,
    experiment_name = experiment_name_test
  )
  
  # Test infile
  expect_equal(experiment_6$infile, infile_test)
  
  # Test main_sheet_name
  expect_equal(experiment_6$main_sheet_name, main_sheet_name_test)
  
  # Test experiment_name
  expect_equal(experiment_6$experiment_name, experiment_name_test)
})



test_that("Base experiment - Data file path (infile) is not defined", {
  # Expect an error message
  expect_error(Experiment_6$new(),
               paste("The infile variable is not defined in child experiment",
                     "class."))
})



test_that("Base experiment - Experiment name is not defined", {
  infile_test <- "data/expt06.xlsx"

  # Initialize class object
  experiment_6 <- Experiment_6$new(
    infile = infile_test
  )

  # Expect an error message
  expect_error(experiment_6$check_experiment_name(),
               "The experiment_name parameter is not defined")
})



test_that(paste("Base experiment - Experiment name in cell A2",
                "in the excel file is missing"), {
  infile_test <- "data/experiment-name-not-provided-in-cell-A2.xlsx"
  experiment_name_test <- "Experiment 6:  Limiting Reactants"

  # Initialize class object
  experiment_6 <- Experiment_6$new(
    infile = infile_test,
    experiment_name = experiment_name_test
  )

  # Expect an error message
  expect_error(experiment_6$check_experiment_name(),
               "The experiment name in cell A2 in the excel file is missing.")
})



test_that("Base experiment - Experiment name in cell A2 is incorrect", {
  infile_test <- "data/incorrect-experiment-name-in-cell-A2.xlsx"
  experiment_name_test <- "Experiment 6:  Limiting Reactants"

  # Initialize class object
  experiment_6 <- Experiment_6$new(
    infile = infile_test,
    experiment_name = experiment_name_test
  )

  # Expect an error message
  expect_error(experiment_6$check_experiment_name(),
               paste0("The experiment name in cell A2 is incorrect. The value ",
                      "should be '", experiment_6$experiment_name, "'."))
})



test_that("Base experiment - Experiment name in cell A2 is correct", {
  infile_test <- "data/expt06.xlsx"
  experiment_name_test <- "Experiment 6:  Limiting Reactants"
  
  # Initialize class object
  experiment_6 <- Experiment_6$new(
    infile = infile_test,
    experiment_name = experiment_name_test
  )
  
  # Expect an error message
  expect_equal(experiment_6$check_experiment_name(), TRUE)
})
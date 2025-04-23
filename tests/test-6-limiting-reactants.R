test_that("Experiment 6 - Class initialization and properties", {
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

test_that("Experiment 6 - File path (infile) is not defined", {
  # Expect an error message
  expect_error(Experiment_6$new(),
               paste("The infile variable is not defined in child experiment",
                     "class."))
})

test_that("Experiment 6 - Experiment name (experiment_name) is not defined", {
  infile_test <- "data/expt06.xlsx"

  # Initialize class object
  experiment_6 <- Experiment_6$new(
    infile = infile_test
  )

  # Expect an error message
  expect_error(experiment_6$check_experiment_name(),
               paste("The experiment_name variable is not defined",
                     "in child experiment class."))
})

test_that(paste("Experiment 6 - Experiment name in cell A2 in the excel file",
                "is not provided"), {
  infile_test <- "data/test-experiment-name-not-provided-in-cell-A2.xlsx"
  experiment_name_test <- "Experiment 6:  Limiting Reactants"

  # Initialize class object
  experiment_6 <- Experiment_6$new(
    infile = infile_test,
    experiment_name = experiment_name_test
  )

  # Expect an error message
  expect_error(experiment_6$check_experiment_name(),
               paste("The experiment name in cell A2 in the excel file",
                     "is not provided."))
})
test_that("Experiment 6 - Class initialization and properties", {
  infile_test <- "data/expt06.xlsx"
  experiment_name_test <- "Experiment 6:  Limiting Reactants"
  
  # Initialize class object
  experiment <- Experiment_6$new(
    infile = infile_test,
    experiment_name = experiment_name_test
  )
  
  # Expect infile is set
  expect_equal(experiment$infile, infile_test)
  
  # Expect experiment_name is set
  expect_equal(experiment$experiment_name, experiment_name_test)
})



test_that("Experiment 6 - Set main df", {
  infile_test <- "data/expt06.xlsx"
  experiment_name_test <- "Experiment 6:  Limiting Reactants"
  expected_column_names <- c("A", "B", "C", "D", "E", "F", "G")
  expected_numeric_column_names <- c("B", "C", "D", "E", "F", "G")
  
  # Initialize class object
  experiment <- Experiment_6$new(
    infile = infile_test,
    experiment_name = experiment_name_test
  )
  
  experiment$set_main_df(9, 32)
  
  # Expect main_df has correct column names
  expect_true(all(expected_column_names %in% colnames(experiment$main_df)))
  
  # Expect main_df has expected columns converted from string to numeric
  expect_true(all(sapply(experiment$main_df[expected_numeric_column_names], 
                         is.numeric)))
})



test_that("Experiment 6 - Check chemical compound mass data (column B)", {
  infile_test <- "data/chemical-compound-mass-check.xlsx"
  expected_vector_result <- c(NA, TRUE, FALSE, FALSE)
  
  # Initialize class object
  experiment <- Experiment_6$new(
    infile = infile_test
  )
  
  # Check chemical compound mass data (column B)
  experiment$set_main_df(9, 12)
  experiment$chkB <- experiment$check_missing(experiment$main_df$B)
  experiment$check_chemical_compound_mass()
  
  
  # Compare elements in each vector
  expect_equal(experiment$chk_mass, expected_vector_result)
})



test_that("Experiment 6 - Check crucible mass data (column C)", {
  infile_test <- "data/crucible-mass-check.xlsx"
  expected_vector_result <- c(NA, TRUE, FALSE)
  
  # Initialize class object
  experiment <- Experiment_6$new(
    infile = infile_test
  )
  
  # Check crucible mass data (column C)
  experiment$set_main_df(9, 11)
  experiment$chkC <- experiment$check_missing(experiment$main_df$C)
  experiment$check_crucible_mass()
  
  
  # Compare elements in each vector
  expect_equal(experiment$chk_crucible, expected_vector_result)
})



test_that("Experiment 6 - Check total mass (column D)", {
  infile_test <- "data/total-mass-check.xlsx"
  expected_vector_result <- c(NA, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  
  # Initialize class object
  experiment <- Experiment_6$new(
    infile = infile_test
  )
  
  # Check total mass (column D)
  experiment$set_main_df(9, 15)
  experiment$chkC <- experiment$check_missing(experiment$main_df$C)
  experiment$chkD <- experiment$check_missing(experiment$main_df$D)
  experiment$check_total_crucible_and_precipitate_mass()
  
  
  # Compare elements in each vector
  expect_equal(experiment$chk_ppt, expected_vector_result)
})



test_that("Experiment 6 - Calculate produced precipitate mass (column E)", {
  infile_test <- "data/precipitate-mass-check.xlsx"
  expected_vector_result <- c(NA, 1, NA, NA, NA, NA, NA, NA)
  
  # Initialize class object
  experiment <- Experiment_6$new(
    infile = infile_test
  )
  
  # Check total mass (column D)
  experiment$set_main_df(9, 16)
  experiment$chkC <- experiment$check_missing(experiment$main_df$C)
  experiment$chkD <- experiment$check_missing(experiment$main_df$D)
  experiment$check_crucible_mass()
  experiment$check_total_crucible_and_precipitate_mass()
  experiment$calculate_precipitate_mass()
  
  
  # Compare elements in each vector
  expect_equal(experiment$main_df$E, expected_vector_result)
})



test_that("Experiment 6 - Calculate produced precipitate mass (column E)", {
  infile_test <- "data/precipitate-mass-check.xlsx"
  expected_vector_result <- c(NA, 1, NA, NA, NA, NA, NA, NA)
  
  # Initialize class object
  experiment <- Experiment_6$new(
    infile = infile_test
  )
  
  # Check total mass (column D)
  experiment$set_main_df(9, 16)
  experiment$chkC <- experiment$check_missing(experiment$main_df$C)
  experiment$chkD <- experiment$check_missing(experiment$main_df$D)
  experiment$check_crucible_mass()
  experiment$check_total_crucible_and_precipitate_mass()
  experiment$calculate_precipitate_mass()
  
  
  # Compare elements in each vector
  expect_equal(experiment$main_df$E, expected_vector_result)
})
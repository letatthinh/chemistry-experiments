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
  
  # Calculate produced precipitate mass (column E)
  experiment$set_main_df(9, 16)
  experiment$chkC <- experiment$check_missing(experiment$main_df$C)
  experiment$chkD <- experiment$check_missing(experiment$main_df$D)
  experiment$check_crucible_mass()
  experiment$check_total_crucible_and_precipitate_mass()
  experiment$calculate_precipitate_mass()
  
  
  # Compare elements in each vector
  expect_equal(experiment$main_df$E, expected_vector_result)
})



test_that("Experiment 6 - Calculate KOx moles (column F)", {
  infile_test <- "data/kox-moles-calculation.xlsx"
  expected_vector_result <- c(0.2/184.23, NA, NA)
  
  # Initialize class object
  experiment <- Experiment_6$new(
    infile = infile_test
  )
  
  # Calculate KOx moles (column F)
  experiment$set_main_df(9, 11)
  experiment$chkB <- experiment$check_missing(experiment$main_df$B)
  experiment$check_chemical_compound_mass()
  experiment$calculate_KOx_moles()
  
  
  # Compare elements in each vector
  expect_equal(experiment$main_df$F, expected_vector_result)
})



test_that(paste("Experiment 6 - Set expected KOx moles",
                "(the smaller one between F and 1.125e-3)"), {
  infile_test <- "data/expected-kox-moles-check.xlsx"
  expected_vector_result <- c(0.2/184.23, 1.125e-3, NA)
  
  # Initialize class object
  experiment <- Experiment_6$new(
    infile = infile_test
  )
  
  # Set expected KOx moles
  experiment$set_main_df(9, 11)
  experiment$chkB <- experiment$check_missing(experiment$main_df$B)
  experiment$check_chemical_compound_mass()
  experiment$calculate_KOx_moles()
  experiment$set_expected_KOx_moles()
  
  # Compare elements in each vector
  expect_equal(experiment$expected_KOx_moles, expected_vector_result)
})



test_that("Experiment 6 - Calculate CaOx moles (column G)", {
  infile_test <- "data/caox-moles-calculation.xlsx"
  expected_vector_result <- c(
    NA, NA, NA, NA, NA, NA,
    (26.807 - 26.718) / 146.11,
    (29.920 - 29.810) / 146.11
  )
  
  # Initialize class object
  experiment <- Experiment_6$new(
    infile = infile_test
  )
  
  # Calculate CaOx moles (column G)
  experiment$set_main_df(9, 16)
  experiment$chkC <- experiment$check_missing(experiment$main_df$C)
  experiment$chkD <- experiment$check_missing(experiment$main_df$D)
  experiment$check_crucible_mass()
  experiment$check_total_crucible_and_precipitate_mass()
  experiment$calculate_precipitate_mass()
  experiment$calculate_CaOx_moles()
  
  # Compare elements in each vector
  expect_equal(experiment$main_df$G, expected_vector_result)
})



test_that("Experiment 6 - Create scatter plot graph and result", {
  infile_test <- "data/expt06.xlsx"
  file_path <- "limiting_reactants.pdf"
  
  # Try to remove the pdf file first
  file.remove(file_path)
  
  # Initialize class object
  experiment <- Experiment_6$new(
    infile = infile_test
  )
  
  # Create scatter plot graph and file
  experiment$set_main_df(9, 32)
  experiment$chkB <- experiment$check_missing(experiment$main_df$B)
  experiment$chkC <- experiment$check_missing(experiment$main_df$C)
  experiment$chkD <- experiment$check_missing(experiment$main_df$D)
  experiment$check_chemical_compound_mass()
  experiment$check_crucible_mass()
  experiment$check_total_crucible_and_precipitate_mass()
  experiment$calculate_precipitate_mass()
  experiment$calculate_KOx_moles()
  experiment$set_expected_KOx_moles()
  experiment$calculate_CaOx_moles()
  experiment$create_KOx_and_CaOx_scatter_plot()
  experiment$write_plot_result(file_path=file_path)
  
  # Check if the plot variable is not NULL
  expect_false(is.null(experiment$plot))
  
  # Check if the file is generated
  expect_true(file.exists(file_path))
})



test_that("Experiment 6 - Create result df and result sheet", {
  infile_test <- "data/expt06.xlsx"
  to_sheet_name <- "complete"
  expected_cell_E32_value_in_new_sheet <- "0.172"
  
  # Initialize class object
  experiment <- Experiment_6$new(
    infile = infile_test
  )
  
  # Create result df and result sheet
  experiment$set_main_df(9, 32)
  experiment$chkB <- experiment$check_missing(experiment$main_df$B)
  experiment$chkC <- experiment$check_missing(experiment$main_df$C)
  experiment$chkD <- experiment$check_missing(experiment$main_df$D)
  experiment$check_chemical_compound_mass()
  experiment$check_crucible_mass()
  experiment$check_total_crucible_and_precipitate_mass()
  experiment$calculate_precipitate_mass()
  experiment$calculate_KOx_moles()
  experiment$set_expected_KOx_moles()
  experiment$calculate_CaOx_moles()
  result_df <- experiment$create_result_df()
  experiment$write_result(
    df = result_df,
    from_sheet_name = "class data",
    to_sheet_name = to_sheet_name,
    df_start_row_index = 9)
  
  # Check if new sheet name exist
  workbook <- wb_load(infile_test)
  expect_true(to_sheet_name %in% workbook$get_sheet_names())
  
  # Check a random value, e.g E32 = 0.172
  new_sheet_df <- read_xlsx(infile_test, sheet = to_sheet_name, col_names=FALSE)
  cell_E32_value <- experiment$excel_utility$get_cell_value(new_sheet_df, "E32")
  expect_equal(cell_E32_value, expected_cell_E32_value_in_new_sheet)
})



test_that("Experiment 6 - Create validity report", {
  infile_test <- "data/validity-report-check-bcd.xlsx"
  file_path <- "limiting_reactants.md"
  expected_texts <- c(
    "### Stations with missing data in all 3 columns B, C, and D:",
    "A-1"
  )
  
  # Initialize class object
  experiment <- Experiment_6$new(
    infile = infile_test
  )
  
  # Create scatter plot graph and file
  experiment$set_main_df(9, 10)
  experiment$chkB <- experiment$check_missing(experiment$main_df$B)
  experiment$chkC <- experiment$check_missing(experiment$main_df$C)
  experiment$chkD <- experiment$check_missing(experiment$main_df$D)
  experiment$check_chemical_compound_mass()
  experiment$check_crucible_mass()
  experiment$check_total_crucible_and_precipitate_mass()
  experiment$calculate_precipitate_mass()
  experiment$calculate_KOx_moles()
  experiment$set_expected_KOx_moles()
  experiment$calculate_CaOx_moles()
  experiment$write_validity_report(file_path = "limiting_reactants.md")
  
  # Read the file
  lines <- readLines(file_path, warn = FALSE)
  
  index <- 1
  
  for (line in lines) {
    # Check if any line contain the expected text, with case-sensitive match
    if (grepl(expected_texts[index], line, fixed = TRUE)) {
      index <- index + 1
    }
    
    if (index > length(expected_texts)) break
  }
  
  expect_equal(index - 1, length(expected_texts))
})



test_that("Experiment 6 - Report remaining cases", {
  infile_test <- "data/validity-report-check.xlsx"
  file_path <- "limiting_reactants.md"
  expected_texts <- c(
    "### Stations with missing data in column B:",
    "A-1",
    "### Stations with missing data in column C:",
    "A-2",
    "### Stations with missing data in column D:",
    "A-3",
    "### Stations with invalid mass data in column B:",
    "A-4",
    "A-5",
    "B-4",
    "### Stations with invalid mass data in column C:",
    "A-6",
    "B-5",
    "### Stations with invalid mass data in column D:",
    "B-1",
    "B-2",
    "B-5",
    paste("### Stations for which a mass of precipitate",
          "could not be calculated (column E):"),
    "A-2",
    "A-3",
    "A-6",
    "B-1",
    "B-2",
    "B-5",
    paste("### Stations for which the amount of precipitate",
          "is identified as an outlier (column G):"),
    "B-3"
  )
  
  # Initialize class object
  experiment <- Experiment_6$new(
    infile = infile_test
  )
  
  # Create scatter plot graph and file
  experiment$set_main_df(9, 19)
  experiment$chkB <- experiment$check_missing(experiment$main_df$B)
  experiment$chkC <- experiment$check_missing(experiment$main_df$C)
  experiment$chkD <- experiment$check_missing(experiment$main_df$D)
  experiment$check_chemical_compound_mass()
  experiment$check_crucible_mass()
  experiment$check_total_crucible_and_precipitate_mass()
  experiment$calculate_precipitate_mass()
  experiment$calculate_KOx_moles()
  experiment$set_expected_KOx_moles()
  experiment$calculate_CaOx_moles()
  experiment$write_validity_report(file_path = "limiting_reactants.md")
  
  # Read the file
  lines <- readLines(file_path, warn = FALSE)
  
  index <- 1
  
  for (line in lines) {
    # Check if any line contain the expected text, with case-sensitive match
    if (grepl(expected_texts[index], line, fixed = TRUE)) {
      index <- index + 1
    }
    
    if (index > length(expected_texts)) break
  }
  
  expect_equal(index - 1, length(expected_texts))
})






















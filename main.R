# General notes:
#   - Set the working directory to the main.R file


# LOAD SOURCES -----------------------------------------------------------------
# Must always load library.R first and be careful with the order of other source
# files.
source("utilities/library.R")
source("utilities/conversion.R")
source("utilities/excel.R")
source("experiments/base.R")
source("experiments/6-limiting-reactants.R")


# MAIN -------------------------------------------------------------------------
main_df_start_row_index = 9
main_df_end_row_index = 32

# Requirement 1.a: Create an object of the Experiment_6 class
experiment <- Experiment_6$new(
  infile = "expt06.xlsx",
  main_sheet_name = "class data",
  experiment_name = "Experiment 6:  Limiting Reactants"
)
# Check if the experiment name has a matching name in cell A2
experiment$check_experiment_name()
# Requirement 1.b: Collect main experiment data from row 9 to row 32 and check 
# missing data in columns B, C, and D
experiment$extract_main_df(main_df_start_row_index, main_df_end_row_index)
experiment$chkB <- experiment$check_missing(experiment$main_df$B)
experiment$chkC <- experiment$check_missing(experiment$main_df$C)
experiment$chkD <- experiment$check_missing(experiment$main_df$D)
# Requirement 1.c:
experiment$check_chemical_compound_mass()
# Requirement 1.d:
experiment$check_crucible_mass()
# Requirement 1.e:
experiment$check_total_crucible_and_precipitate_mass()
# Requirement 2.a:
experiment$calculate_precipitate_mass()
# Requirement 2.b:
experiment$calculate_KOx_moles()
# Requirement 3.a:
experiment$set_expected_KOx_moles()
# Requirement 2.c:
experiment$calculate_CaOx_moles()
# Requirement 2.d + 3.a:
experiment$create_KOx_and_CaOx_scatter_plot()
# Requirement 4.a:
result_df <- experiment$create_result_df()
experiment$write_result(
  new_sheet_name = "complete",
  result_df,
  main_df_start_row_index)
# Requirement 4.b:
experiment$write_plot_result()
# Requirement 4.c:
experiment$write_validity_report()

print(experiment$main_df)
print(experiment$chkB)
print(experiment$chk_mass)
print(experiment$chkC)
print(experiment$chk_crucible)
print(experiment$chkD)
print(experiment$check_negative(experiment$main_df$D))

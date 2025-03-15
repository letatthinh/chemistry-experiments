# General notes:
#   - Set the working directory to the main.R file


# LOAD SOURCES -----------------------------------------------------------------
# Must always load library.R first and be careful with the order of other source
# files
source("utilities/library.R")
source("utilities/excel.R")
source("experiments/base.R")
source("experiments/6-limiting-reactants.R")


# MAIN -------------------------------------------------------------------------
# This is where the code run
# Create an object of the Experiment_6 class using the new() function
# Requirement 1.a
experiment <- Experiment_6$new()
# Requirement 1.b:
experiment$get_main_df()
experiment$check_missing_data()
# Requirement 1.c:
experiment$check_chemical_compound_mass()
# Requirement 1.d:
experiment$check_crucible_mass()
# Requirement 1.e:
experiment$check_precipitate_mass()
# Requirement 2.a:
experiment$calculate_produced_chemical_compound_mass()
# Requirement 2.b:
experiment$calculate_KOx_moles()
# Requirement 3.a:
experiment$set_expected_KOx_moles()
# Requirement 2.c:
experiment$calculate_CaOx_moles()
# Requirement 2.d + 3.a:
experiment$create_KOx_and_CaOx_scatter_plot()
# Requirement 4.a:
data_result_df <- experiment$create_data_result_df()
experiment$write_result(
  new_sheet_name = "complete",
  data_result_df,
  experiment$main_df_start_row_index)
# Requirement 4.b:
experiment$write_plot_result()

print(experiment$main_df)

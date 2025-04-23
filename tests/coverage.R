# General notes:
#   - Set the working directory to the this file

coverage <- file_coverage(
  source_files = c(
    "../utilities/conversion.R",
    "../utilities/excel.R",
    "../experiments/base.R",
    "../experiments/6-limiting-reactants.R"
  ),
  test_files = c(
    "test-6-limiting-reactants.R"
  )
)

print(coverage)
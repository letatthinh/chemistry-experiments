
# LOAD SOURCES -----------------------------------------------------------------
# Must always load library.R first and be careful with the order of other source
# files.
source("utilities/library.R")
source("utilities/conversion.R")
source("utilities/excel.R")
source("experiments/base.R")
source("experiments/6-limiting-reactants.R")

# Run the unit tests
# Note: this will change the working directory to root/tests
testthat::test_dir("tests")

# Calculate test coverage
# Note: the working directory is still at root!!!
coverage <- file_coverage(
  source_files = c(
    "utilities/library.R",
    "utilities/conversion.R",
    "utilities/excel.R",
    "experiments/base.R",
    "experiments/6-limiting-reactants.R"
  ),
  test_files = c(
    "tests/test-6-limiting-reactants.R"
  )
)

print(coverage)
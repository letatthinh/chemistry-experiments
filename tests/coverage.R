# General notes:
#   - Set the working directory to the this file

coverage <- file_coverage(
  source_files = c(
    "../experiments/base.R",
    "../experiments/6-limiting-reactants.R"
  ),
  test_files = c(
    "test-base-experiment.R",
    "test-6-limiting-reactants.R"
  )
)

# Print uncovered lines by source file and line number
print(coverage)

# View coverage report
report(coverage)
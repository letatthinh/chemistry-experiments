# Methods support working with packages in R
# Note: All packages must be both defined and loaded in this file.


# Load a package, or install it if it is missing.
load_or_install_package <- function(package_name) {
  # requireNamespace(): simply check if the package exists
  if (!requireNamespace(package_name)) {
    # If it doesn't exist, install it
    install.packages(package_name)
  }
  
  # Load the package
  # character.only: indicate package will be passed in as a string
  library(package_name, character.only = TRUE)
}


# LOAD PACKAGES ---------------------------------------------------------------

# For working with class-like structures in R
load_or_install_package("R6")

# For working with strings
load_or_install_package("stringr")

# For working with data
load_or_install_package("dplyr")

# For working with graphs
load_or_install_package("ggplot2")

# For reading or writing data to excel
load_or_install_package("openxlsx2")

# For reading or writing data to excel
load_or_install_package("openxlsx2")

# For handling overlapping text labels
load_or_install_package("ggrepel")
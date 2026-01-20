#!/usr/bin/env Rscript
# Script to regenerate roxygen documentation
# Run this script from the package root directory:
# Rscript dev/regenerate_docs.R

# Check if we're in the right directory
if (!file.exists("DESCRIPTION")) {
  stop("Please run this script from the package root directory")
}

# Load roxygen2
if (!requireNamespace("roxygen2", quietly = TRUE)) {
  message("Installing roxygen2...")
  install.packages("roxygen2")
}

# Regenerate documentation
message("Regenerating roxygen documentation...")
roxygen2::roxygenise()

message("Documentation regenerated successfully!")
message("Please review the changes in man/ and NAMESPACE before committing.")

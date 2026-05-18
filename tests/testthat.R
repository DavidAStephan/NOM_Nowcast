library(testthat)

# Load every R/ source file before running tests so that helper functions
# in subdirectories are visible. Mirrors what `targets::tar_source("R")`
# does at pipeline time.
r_files <- list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
for (f in r_files) sys.source(f, envir = globalenv())

test_check("nomnowcast")

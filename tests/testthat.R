## Test entry point. The project is sourced, not installed as a package
## (targets uses tar_source("R")), so `test_check()` cannot be used.
## Run from the project root with:  Rscript tests/testthat.R
suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
})

if (!file.exists("DESCRIPTION") && file.exists("../DESCRIPTION")) setwd("..")
if (!file.exists("DESCRIPTION")) stop("Run tests from the project root")

r_files <- list.files("R", pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
for (f in r_files) sys.source(f, envir = globalenv())

res <- testthat::test_dir("tests/testthat", reporter = "summary",
                          stop_on_failure = FALSE)
df <- as.data.frame(res)
tot <- colSums(df[, c("nb", "failed", "skipped", "error", "warning")])
cat("\n== totals ==\n"); print(tot)
if (tot[["failed"]] > 0 || tot[["error"]] > 0) {
  stop("testthat: ", tot[["failed"]], " failed, ", tot[["error"]], " errored")
}

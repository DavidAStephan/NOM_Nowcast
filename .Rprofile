source("renv/activate.R")

# Project-wide options. Keep this file tiny; everything substantive lives in
# config.yml or in the package code under R/.
options(
  repos          = c(CRAN = "https://cloud.r-project.org"),
  readr.show_col_types = FALSE,
  tibble.print_max     = 30,
  tibble.width         = 120,
  dplyr.summarise.inform = FALSE,
  cli.num_colors       = 256
)

# Always run from project root.
if (interactive()) {
  if (!file.exists("DESCRIPTION")) {
    warning("nomnowcast: working directory does not contain DESCRIPTION. ",
            "Open the project at its root.")
  }
}


library(rix)

# Choose the path to your project
path_default_nix <- "."

rix(
  ## use debian current version
  r_ver  = "4.2.3",
  r_pkgs = c(
             "data.table",
             "dplyr",
             NULL), # List all the packages you need
  ide          = "rstudio", # List whatever editor you need
  project_path = path_default_nix,
  overwrite    = TRUE,
  print        = FALSE
)

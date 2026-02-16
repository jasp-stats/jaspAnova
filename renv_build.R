### Title:    Build package with renv
### Author:   Kyle M. Lang
### Created:  2024-11-05
### Modified: 2025-05-01

## Let renv do things without asking:
renv::consent(provided = TRUE)

if (is.null(renv::project())) {
  message("This isn't an active renv project. So, I'm activating the project.")
  renv::activate()
} else {
  message("We're in an active renv project. Let's move on.")
}

message("Restoring/synchronizing the project library.")
renv::restore(clean = FALSE)

message("Installing the package.")
renv::install('.')
#renv::install('gitcreds')
#renv::clean()
#utils::install.packages('gitcreds')

message("R libPath for developer mode:\n", .libPaths()[1])

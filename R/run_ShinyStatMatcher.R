# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

run_ShinyStatMatcher <- function(){
  appDir <- system.file("app_folder", package = "ShinyStatMatcher")
  rmarkdown::run(paste0(appDir, "/0-master-app-matching.Rmd"))
  #print(dir(appDir))
}





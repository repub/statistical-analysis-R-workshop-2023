get_libs <- function(libs) {
  new.packages <- libs[!(libs %in% installed.packages()[, "Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  # Load all packages into the R environment
  sapply(libs, require, character.only = TRUE)
}
# Give a list of libraries (libs) to install and load them
get_libs <- function(libs) {
  # Get a list of libraries that are not currently installed
  new.packages <- libs[!(libs %in% installed.packages()[, "Package"])]
  
  # If all libraries are installed then continue, else install missing libraries
  if(length(new.packages)) install.packages(new.packages)
  
  # Load all packages into the R environment
  sapply(libs, require, character.only = TRUE)
}
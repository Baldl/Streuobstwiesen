#' Setup project environment
#'
#' @description This script configures the project environment.
#'
#' @author [name], [email@com]
#'

# Use this file for general project settings.

# This script is sourced when you run the main control script. Use variable envrmt to access project directories.


require(envimaR)

# Define libraries
libs <- c("terra", 
          "sf", 
          "tidyverse", 
          "caret", 
          "foreach", 
          "doParallel", 
          "CAST", 
          "RSDB",
          "plyr",
          "mapview",
          "telegram.bot", 
          "randomForest", 
          "uavRst", 
          "RStoolbox",
          "ggplot2",
          "ggnewscale",
          "RColorBrewer",
          "gt",
          "yaml",
          "webshot",
          "reticulate",
          "rgee"
)

# Set project specific subfolders
projectDirList   = c("data/",
                     "data/001_raw_data/sentinel/",
                     "data/001_raw_data/study_area/",
                     "data/001_raw_data/lidar/",
                     "data/001_raw_data/hlnug/",
                     "data/002_modelling/model_training_data/" ,
                     "data/002_modelling/models/",
                     "data/002_modelling/prediction/",
                     "data/002_modelling/aoa/",
                     "data/002_modelling/selected_variables/",
                     "data/003_validation/",
                     "data/003_validation/confusionmatrix/",
                     "data/003_validation/varImp/",
                     "data/003_validation/figures/",
                     "data/003_validation/figures/maps/",
                     "data/003_validation/figures/illustrations/",
                     "docs/",
                     "mapping/",
                     "notes/",
                     "src/",
                     "src/functions/")

# Load libraries and create environment object to be used in other scripts for path navigation
project_folders <- list.dirs(path = root_folder, full.names = FALSE, recursive = TRUE)
project_folders <- project_folders[!grepl("\\..", project_folders)]
envrmt <- createEnvi(
  root_folder = root_folder, 
  fcts_folder = file.path(root_folder, "src/functions/"),  
  folders = projectDirList,
  libs = libs, create_folders = FALSE)
meta <- createMeta(root_folder)

# Define more variables

# Load more data
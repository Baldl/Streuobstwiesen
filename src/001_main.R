#' sub control script for data preparation
#'
#'
#' @description Use this script for controlling the processing.
#'
#' @author [name], [email@com]
#'


# 0 - set up ####
#---------------#

library(envimaR)
library(rprojroot)
root_folder = find_rstudio_root_file()

source(file.path(root_folder, "src/functions/000_setup.R"))


# 1 -
#-------------------

extr = readRDS(file.path(envrmt$model_training_data, "extraction.RDS"))
train_ids = sample(unique(extr$OBJ_ID),  length(unique(extr$OBJ_ID))*0.8)
extr_train = extr%>%filter(OBJ_ID %in% train_ids)

nrow(filter(extr_train, class == "other"))
nrow(filter(extr_train, class == "SO"))

extr_train = extr_train %>% 
 group_by(class)%>%
  sample_n(nrow(filter(extr_train, class == "SO")))

n_distinct(filter(extr_train, class == "other"))
n_distinct(filter(extr_train, class == "SO"))





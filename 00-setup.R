# Setup access to dropbox in AWS EC2
library(RStudioAMI)
excludeSyncDropbox("*")
includeSyncDropbox("to aws")


# Short setup
install.packages(c("lavaan", "semTools", "simsem", "purrr"))
library(lavaan)
library(simsem)                 
library(semTools)
library(purrr)

install.packages("dplyr")
library(dplyr)


# # Full setup
# install.packages(c("lavaan", "semTools", "simsem", "boot", "tidyverse", "psych", "SparseM", "car"))
# 
# 
# library(purrr)
# library(dplyr)
# library(lavaan)
# library(semTools)
# library(psych)
# library(boot)
# library(simsem)   
# library(ggplot2)
# library(car)

library(RStudioAMI)
passwd()


# Setup access to dropbox in AWS EC2
linkDropbox()
excludeSyncDropbox("*")
includeSyncDropbox("to aws")


# Short setup
install.packages(c("lavaan", "semTools", "simsem", "tidyverse", "parallel"))
library(lavaan)
library(simsem)                 
library(semTools)
library(tidyverse)
library(parallel)


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

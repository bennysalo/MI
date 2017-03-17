# Setup access to dropbox in AWS EC2
install.packages(c("lavaan", "semTools", "tidyverse", "psych"))

library(RStudioAMI)
linkDropbox()
excludeSyncDropbox("*")
includeSyncDropbox("to aws")

library(purrr)
library(dplyr)
library(lavaan)
library(semTools)
library(psych)
library(boot)
library(simsem)
library(ggplot2)

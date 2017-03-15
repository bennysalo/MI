# Setup access to dropbox in AWS EC2
library(RStudioAMI)
linkDropbox()
excludeSyncDropbox("*")
includeSyncDropbox("to aws")

library(purrr)
library(lavaan)
library(semTools)
library(psych)

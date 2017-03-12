library(lavaan)
library(boot)
library(simsem)
library(semTools)
library(ggplot2)




# Function for calculating standardized loading and threshold differences from a configural model
# To be used with 'bootstrapLavaan' to extract these parameters from every bootstrap,
# and to extract them from the original model
# Takes argument 'fitted.model' which is a fitted cfa model (using lavaan)

coef_diff <- function(fitted.model) {

  # Pick number of obervations per group, used for calculating pooled factor variance
  n.g1 <- lavInspect(fitted.model, what = "nobs")[1]
  n.g2 <- lavInspect(fitted.model, what = "nobs")[2]

  # Pick parameter table, will be used multiple times
  pt <- parameterTable(fitted.model)

  # Identify factors (the names on the left hand side in parameters that includes the operator "=~")
 factors <- subset(pt, op == "=~")
 factors <- unique(factors$lhs)

  # Pick the rows in pt that has the operator "~~" (variances) AND has one of the factors in "lhs"
 factor.vars <- subset(pt, op == "~~" & lhs %in% factors)
 factor.vars <- factor.vars[c("lhs", "op",  "rhs", "group", "est")]
  # Split into two groups 
 factor.vars1 <- subset(factor.vars, group == 1)
 factor.vars2 <- subset(factor.vars, group == 2)
  # Merge and match by parameter 
 factor.vars <- merge(factor.vars1, factor.vars2, by = c("lhs", "op", "rhs"), 
                      suffixes = c(".g1",".g2"))
 
  # Calculate pooled sd: square root of the mean variance weighted by degrees of freedom
 factor.vars$pool.f.sd <- sqrt((factor.vars$est.g1 * (n.g1 - 1) + 
                                factor.vars$est.g2 * (n.g2 - 1))   / (n.g1 + n.g2 - 2))

  # Pick loadings and merge groups to same row for same parameter
 loadings <- subset(parameterTable(fitted.model), op == "=~")
 loadings <- loadings[c("lhs", "op", "rhs", "group", "est")]
 loadings1 <- subset(loadings, group == 1)
 loadings2 <- subset(loadings, group == 2)
 loadings <- merge(loadings1, loadings2, by = c("lhs", "op", "rhs"), 
                   suffixes = c(".g1",".g2"))

  # Merge pooled factor sd for the right factor to the loadings
 loadings <- merge(loadings, factor.vars[,c("lhs","pool.f.sd")], by = "lhs")

  # Calculate differences between loadings in the two groups and standardize the differences
  # Differences in loading can be standardized by multiplying the difference by sd(factor)/sd(item)
  # In delta parameterization (the default in lavaan::cfa) sd(item) is by definition 1...
  # i.e. differences can be standardized through multiplication by sd(factor)

 loadings$s.diff <- (loadings$est.g1 - loadings$est.g2) * loadings$pool.f.sd

  # Pick thresholds (operator is "|") and merge to same row for same threshold
  thresholds  <- subset(parameterTable(fitted.model), op == "|")
  thresholds  <- thresholds[c("lhs", "op",  "rhs", "group", "est")]
  thresholds1 <- subset(thresholds, group == 1)
  thresholds2 <- subset(thresholds, group == 2)
  thresholds  <- merge(thresholds1, thresholds2, by = c("lhs", "op", "rhs"), 
                       suffixes = c(".g1",".g2"))
  # Create a column pool.f.sd (NA's) for thresholds matching the one for loadings
  # (simply to be able to combine rows with rbind later)
  thresholds$pool.f.sd <- NA
  
  # Calculate threshold difference
  thresholds$s.diff <- thresholds$est.g1 - thresholds$est.g2

  # Combine loadings and thresholds
  loadings_and_thresholds <- rbind(loadings, thresholds)

  # Create output vector and give it names according to the lhs, op and rhs columns
  # and gives them the suffix ".std_diff"
  out <- loadings_and_thresholds$s.diff
  names(out) <- paste(loadings_and_thresholds$lhs, loadings_and_thresholds$op, 
                      loadings_and_thresholds$rhs,".std_diff")
  # Add original coefficients to allow double checking
  c(out, coef(fitted.model, type = "user"), n1 = n.g1, n2 = n.g2)
}

# Function for placing standardised differences and the 95% CIs in matrix

create_sdiff_CI_df <- function(original_model, bootstrapped_values) {
  # Grab the standardized differences from the original fit using 'coeff_diff'
  # Only grab the coefficients with the suffix ".std_diff"
  standardized_differences <- coef_diff(original_model)
  standardized_differences <- standardized_differences[grep(names(standardized_differences), 
                                                            pattern = ".std_diff")]
  
  # Using apply, grab the 2.5 and 97.5 percentile in the *columns* ('MARGIN = 2') in bootstrapped_values
  #  ('bootstrapLavaan' places bootstraps on seperate rows 
  #  with seperate columns for the different coeffictions)
  confidence_intervals <- apply(bootstrapped_values, MARGIN = 2, quantile, probs = c(0.025, 0.975))
  # Transpose to place lower and upper limit in different columns instad of rows
  confidence_intervals <- t(confidence_intervals)
  # Grab only the parameters with the suffix ".std_diff"
  confidence_intervals <- confidence_intervals[grep(rownames(confidence_intervals), 
                                                    pattern = ".std_diff"), ]
  
  # Create 'out' data.frame combining the standardized differences and their confidence interval 
  out <- data.frame(standardized_differences, confidence_intervals)
  # Give this data frame adequate column names
  colnames(out) <- c("std_diff", "boot_2.5p", "boot_97.5p")
  rownames(out) <- sub(pattern = ".std_diff", replacement = "", x = rownames(out))
  out
}




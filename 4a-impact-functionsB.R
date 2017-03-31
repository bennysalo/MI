


# Function for replacing the 'ustart' values in a parameter table 
# with the fitted coefficents from another parameter table
# Returns a paramter table that can be used with 'lavaan:simulateData' 
# (as in function 'create_data_list' below)

replace_coefs_in_pt <- function(pt.frame, pt.replacement) {
  # Ensure the same order of parameters in both paramter tables
  order_frame       <- order(pt.frame$op,       pt.frame$lhs,       pt.frame$rhs)
  order_replacement <- order(pt.replacement$op, pt.replacement$lhs, pt.replacement$rhs)
  
  frame             <- pt.frame[order_frame, ]
  replacement       <- pt.replacement[order_replacement, ]
  
  # Put in the same data.frame (to avoid differing row numbers claiming differences)
  both           <- cbind(frame[,2:4], replacement[,2:4])
  
  # Continue only if the parameters are identical (otherwise give error message)
  if (!(identical(both[,1:3], both[,4:6]))) {
    stop("Table parameters do not match", call. = FALSE) }
  
  # Replace the 'ustart' values in the frame with the coefficients in the replacing parameter table
    frame$ustart <- replacement$est
    # Return in original order 
    # and only the columns from output of a arbitrary model in lavaan:lavaanify
    return(frame[order(frame$id), names(lavaanify('factor =~ item'))])
}


# Function for replacing a single paramenter. Takes arguments:
# pt.frame = The paramenter table where a parameter should be replaced and then returned
# pt.replacement = The parameter table from where the replacing parameter should be taken
# lhs, op and rhs = column names in parameter tables used to identify parameter

replace_1_coef_in_pt <- function (pt.frame, pt.replacement, lhs, op, rhs = "") {
  # locate row number for parameter (to be changed) in both tables, grab that index i
  i_frame       <- which(pt.frame$lhs == lhs       & pt.frame$op == op       & pt.frame$rhs == rhs)
  i_replacement <- which(pt.replacement$lhs == lhs & pt.replacement$op == op & pt.replacement$rhs == rhs)
  # Check that there is one (and only one) index for both tables
  if (length(i_frame) != 1 | length(i_replacement) != 1) {
    stop("No, or more than one, matching parameter", call. = FALSE)
  } 
  # Make replacement
  pt.frame[i_frame, "ustart"] <- pt.replacement[i_replacement, "est"]
  return(pt.frame)
}


# Create data list for use in simulations. Takes arguments: 
# parameter table 1 and 2, the group sizes, the number of datasets to produce, and
# labels for the groups (defaults to 1 & 2)

create_data_list<- function (pt1, pt2, n1, n2, n_sets, label1 = 1, label2 = 2) {
  data_list <- list()
  for(i in 1:n_sets) {
    simulated_data.group1 <- simulateData(pt1, sample.nobs = n1)
    simulated_data.group2 <- simulateData(pt2, sample.nobs = n2)
    simulated_data        <- rbind(simulated_data.group1, simulated_data.group2)
    
    # Add column with group membership - if labels are defined, use those
    simulated_data        <- data.frame(simulated_data, 
                                        group = rep(c(label1, label2), times = c(n1, n2)))
    # Make all variables ordered
    simulated_data[c(1:length(simulated_data))] <- 
      lapply(simulated_data[c(1:length(simulated_data))], ordered)
    # Add the latest simulated dataset to the list
    data_list[[i]]        <- simulated_data
  }
  return(data_list)
}




# Create parameter tables for simulation of invariant data. Takes aruguments:
# pt_list = List of parameter tables produced by 'grab_parameter_tables'
# n_sets = number of data sets to produce (passed to 'create_data_list')

create_invariant_data <- function(results_list, n_sets) {
  
  
  pt.single     <- parameterTable(cfa(model     = results_list[["base model"]], 
                                      data      = FinPrisonMales2, 
                                      std.lv    = TRUE, 
                                      estimator = "WLSMV",
                                      do.fit    = FALSE))
  pt.strong     <- parameterTable(results_list[["strong fit"]])
  
  # Create parameter tables for the two groups
  # Use single group model as frame and use parameter values from strong model
  pt.invariant1 <- replace_coefs_in_pt(pt.frame = pt.single, 
                                       pt.replacement = pt.strong[pt.strong$group == 1, ])
  pt.invariant2 <- replace_coefs_in_pt(pt.single, pt.strong[pt.strong$group == 2, ])
  
  # Grab group sizes for the two groups
  n1            <- lavInspect(results_list[["strong fit"]], what = "nobs")[1]
  n2            <- lavInspect(results_list[["strong fit"]], what = "nobs")[2]
  
  # Add label for group labels
  label1        <- lavInspect(results_list[["strong fit"]], what = "group.label")[1]
  label2        <- lavInspect(results_list[["strong fit"]], what = "group.label")[2]
  
  # Use 'create_data_list' to simulate datasets
  invariant_data <- create_data_list(pt.invariant1, pt.invariant2, 
                                     n1, n2, label1 = label1, label2 = label2, n_sets)
  return(invariant_data)
}

# Create parameter tables for simulation of invariant data. 
# - replacing means (but not variances) with the same from strong invariance model
# Takes arguments:
# drug.model.list = List of three fitted models produced by 'run.3.drugmodels'
# n.sets = number of data sets to produce (passed to 'create_data_list'
create_biased_data <- function(results_list, n_sets) {
  
  # Grab parameter tables from the three fitted models
  pt.single     <- parameterTable(cfa(model     = results_list[["base model"]], 
                                      data      = FinPrisonMales2, 
                                      std.lv    = TRUE, 
                                      estimator = "WLSMV",
                                      do.fit    = FALSE))
  pt.configural <- parameterTable(results_list[["configural fit"]])
  pt.strong     <- parameterTable(results_list[["strong fit"]])
  
  # Identify factors
  factors          <- subset(pt.single, op == "=~")
  factors          <- unique(factors$lhs)
  
  # Create parameter tables for groups 1 and 2
  # with single group as frame and parameter values from configural model
  pt.biased1    <- replace_coefs_in_pt(pt.single, pt.configural[pt.configural$group == 1, ])
  pt.biased2    <- replace_coefs_in_pt(pt.single, pt.configural[pt.configural$group == 2, ])
  
  # Replace means with those from strong model. This to make the mean difference comparable
  # with the invariant data
  for (i in 1:length(factors)) {
  pt.biased1    <- replace_1_coef_in_pt(pt.frame = pt.biased1, 
                                        pt.replacement = pt.strong[pt.strong$group == 1,], 
                                        lhs = factors[i], op = "~1")
  pt.biased2    <- replace_1_coef_in_pt(pt.frame = pt.biased2, 
                                        pt.replacement = pt.strong[pt.strong$group == 2,], 
                                        lhs = factors[1], op = "~1")
  }
  
  # Grab group sizes for the two groups
  n1            <- lavInspect(results_list[["strong fit"]], what = "nobs")[1]
  n2            <- lavInspect(results_list[["strong fit"]], what = "nobs")[2]
  
  # Add label for group labels
  label1        <- lavInspect(results_list[["strong fit"]], what = "group.label")[1]
  label2        <- lavInspect(results_list[["strong fit"]], what = "group.label")[2]
  
  # Simulate biased data using parameter tables
  biased_data <- create_data_list(pt.biased1, pt.biased2, 
                                n1, n2, label1 = label1, label2 = label2, n_sets)
  return(biased_data)
}

# Function for calculating difference in standardized coeffiecient
# between invariant and biased datasets. Takes arguments:
# sim.invariant = SimResult from runs on invariant data
# sim.biased = SimResult from runs on biased data


get_path_difference <- function (sim.invariant, sim.biased) {
  # pick stanardized coefficients of the parameter "DrugL~group"
  std_coeff.inv  <- inspect(sim.invariant, what = "std")$"DrugL~group"
  std_coeff.bias <- inspect(sim.biased,    what = "std")$"DrugL~group"
  # calculate Fisher's Z
  Fz.inv  <- atanh(std_coeff.inv)
  Fz.bias <- atanh(std_coeff.bias)
  # Calculate mean, difference in means
  n.inv   <- length(std_coeff.inv )
  m.inv   <- mean(Fz.inv)
  sd.inv  <- sd(Fz.inv)
  se.inv  <- sd(Fz.inv)/sqrt(length(Fz.inv))
  
  n.bias  <- length(std_coeff.bias)
  m.bias  <- mean(Fz.bias)
  sd.bias <- sd(Fz.bias)
  se.bias <- sd(Fz.bias)/sqrt(length(Fz.bias))
  
  diff    <- mean(Fz.inv - Fz.bias)
  sd.diff <- sd(Fz.inv - Fz.bias)
  se.diff <- sd(Fz.inv - Fz.bias)/sqrt(length(Fz.bias))
  # Create vector and convert back to standardized coefficient
  out <- c(m.inv, m.bias , diff , 
           sd.inv, sd.bias, sd.diff, 
           se.inv, se.bias, se.diff)
  out <- tanh(out)
  out <- matrix(out, 3,3, byrow = FALSE)
  out <- cbind(out, c(length(Fz.inv), length(Fz.bias), mean(c(length(Fz.inv), length(Fz.bias)))))
  rownames(out) <- c("Invariant datasets", "Biased datasets", "Difference")
  colnames(out) <- c("Mean", "sd", "se", "n of repl")
  return(out)
}

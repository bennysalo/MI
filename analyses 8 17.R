rm(list = ls())

install.packages(c("lavaan", "semTools", "simsem", "tidyverse"))
library(tidyverse)
library(lavaan)
library(simsem)

# LOAD DATA

FinPrisonMales2 <- readRDS("C:/Users/benny_000/Dropbox/to aws//FinPrisonMales2.rds")
FinPrisonMales2 <- readRDS("~/Dropbox/to aws/FinPrisonMales2.rds")



# PREDIFINE USED OBJECTS

predefined_model    <- ' 
  economy    =~ i_financialManagement + i_financialObstacles + i_financialSituation + 
                i_dailyLifeManagement + ic_accomodation +  i_workApplication
  alcohol    =~ i_alcFrequency + i_alcEffectWork + i_alcEffectRelations + i_alcEffectHealth +
                i_alcWithMeds + i_alcMotivationTreat + i_alcViolence
  change     =~ i_motivationChange + i_attitudeStaff + i_attitudeSupervision + i_othersView + i_attitudeHostile +
                i_insight + i_manipulative + i_attitudeProcrime + i_socialSkills + i_alcMotivationTreat +
                i_instrumentalAggression + i_workAttitude
  drugs      =~ i_peersCriminal + i_riskSeeking + i_drugHistory +ifact_drugUse +
                i_alcWithMeds + i _attitudeProcrime
  aggression =~ i_impulsive + i_alcViolence + i_domViolPerp + i_instrumentalAggression + i_attitudeHostile +
                i_othersView + i_attitudeStaff
  employment =~ i_workHistory + i_eduNeed + i_eduAttitude + i_workAttitude +
                i_remedialTeaching + i_workApplication'

items <- subset(lavaanify(predefined_model), op == "=~")$rhs

predefined_grouping <- "ageMedSplit"

column_numbers <- which(names(FinPrisonMales2) %in% c(items, predefined_grouping))
predefined_data     <- select(FinPrisonMales2, column_numbers)

n_samples           <- 2

predefined_factors  <- subset(lavaanify(predefined_model), op == "=~")
predefined_factors  <- unique(predefined_factors$lhs)

predefined_path_model <- paste(predefined_model, '\n', 
                               paste(predefined_factors, '~ group \n', collapse = " "))





# RUN PRELIMINARY ANALYSES, INCLUDING FINDING REFERENT ITEMS

# Pick items that only occur once in the parameter table
# (candidates as referent items)
unidim_items <- names(table(items))[table(items) == 1]


# Run a set of preliminary analyses
# Median split on AGE
print(Sys.time())
start<-Sys.time()
mod1_prel_results <- run_prel_analyses(base_model = predefined_model, 
                                       used_data  = predefined_data, 
                                       grouping   = predefined_grouping, 
                                       item_vector = unidim_items)

Sys.time()-start



print(Sys.time())
start2<-Sys.time()


# BOOTSTRAP SAMPLES  

# Function for creating one stratified bootstrap (maintaining group sizes)
create_one_bootstrap <- function (dataset, grouping_var) {
  require(dplyr)
  dataset %>%
    group_by_(grouping_var) %>%
    # For each group, sample with replacement a number of rows equal the original number of rows
    sample_frac(size = 1, replace = TRUE) %>%
    ungroup() 
}

set.seed(2108)
 
  boots_samples <- 
    # Create 'n_samples' copies of the original data i one big data.frame
    map_df(predefined_data, rep.int, times = n_samples) %>% 
    # Create index numers for each replication of the origianl data: sample_nr
    mutate(sample_nr = rep(1:n_samples, each = nrow(predefined_data))) %>% 
    # Nest based on sample_nr creating a tibble with 'n_samples' rows with original data.frame in each row.
    nest  (-sample_nr) %>%
    # Use the function 'create_one_bootstrap' to create a bootstrap sample for each row
    mutate(boots = map(.x = data, 
                       .f = ~ create_one_bootstrap(.x, grouping_var = predefined_grouping))) %>% 
    # Remove the origial data
    select(-data) 
  
 
# FIT CONFIGURAL MODEL TO BOOTSTRAP SAMPLES AND RECORD PARAMETERS
  # Function for getting the parameter table from the results of fitting a configural invariance model
  # Argument 'orig_pt' is meant to take a parameter table 

  get_parTable <- function(dataset, orig_pt, grouping_var = predefined_grouping) {
    require(lavaan)
    fit <- cfa(model = orig_pt, data = dataset, group = grouping_var, estimator = "WLSMV")
    parTable(fit)
  }  
  
  # Setup the parameter table that will be used as the model for the analyses
  # Grab the parameter table from the configural fit in the preliminary analyses
  orig_configural_pt <- parTable(mod1_prel_results$configural_fit)
  # Replace start values with the estimated parameters in the original sample
  orig_configural_pt$ustart <- orig_configural_pt$est
  
  # Run configural invariance model and record the parameter table for each bootstrap sample

  configural_pts <- lapply(boots_samples$boots, get_parTable, orig_pt = orig_configural_pt)

  # Add parameter tables of configural fits to the tibble
  
  boots_samples <- boots_samples %>% 
    mutate(pt_configural = configural_pts)
  
  # Remove the now duplicate results from the environment
  rm(configural_pts)
  
# CREATE PARAMETER TABLES THAT CAN BE USED FOR SIMULATION
  
  # Function for replacing the 'ustart' values in a parameter table 
  # with the fitted coefficents from another parameter table
  # Returns a paramter table that can be used with 'lavaan:simulateData' 
  # Setup for use in functions 'create_invariant_data' and 'create_biased_data'
  #c Takes arguments:
  # pt.frame = The paramenter table where a parameter should be replaced and then returned
  # pt.replacement = The parameter table from where the replacing parameter should be taken
  replace_coefs_in_pt <- function(pt.frame, pt.replacement) {
    require(lavaan)
    # Ensure the same order of parameters in both paramter tables
    order_frame       <- order(pt.frame$op,       pt.frame$lhs,       pt.frame$rhs)
    order_replacement <- order(pt.replacement$op, pt.replacement$lhs, pt.replacement$rhs)
    
    frame             <- pt.frame[order_frame, ]
    replacement       <- pt.replacement[order_replacement, ]
    
    # Put in the same data.frame, side by side, (to avoid differing row numbers claiming differences)
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
  
  
  

  # Split pt_configural into seperate tables for the two groups: 'pt_boot_1' and 'pt_boot_2'
  boots_samples <- boots_samples %>% 
    mutate(pt_boot_g1 = map(.x = pt_configural, ~filter(.x, group == 1)),
           pt_boot_g2 = map(.x = pt_configural, ~filter(.x, group == 2)))
  
  # Create a frame for the parameter tables in both groups: based on a single group model
  pt_single <- parTable(cfa(model = predefined_model,
                            data      = predefined_data,
                            std.lv    = TRUE,
                            estimator = "WLSMV",
                            do.fit    = FALSE))
  
  
  # Set up the parameter table frame twice for each sample, once for each group
  # Theese will, after modification, be used for simulating datasets
  boots_samples <- boots_samples %>%
    mutate(pt_for_sim_1 = replicate(n = nrow(boots_samples), expr = pt_single, simplify = FALSE),
           pt_for_sim_2 = replicate(n = nrow(boots_samples), expr = pt_single, simplify = FALSE))
  
  # Modify the 'ustart' values in these pts for simulation
  
  # Use the function 'replace_coefs_in_pt()' to replace the start values in pt_for_sim_1 and pt_for_sim_2
  # with the estimated parameter values in pt_configural_1 and pt_configural_2 (respectively)
  boots_samples <- boots_samples %>% 
    mutate(pt_for_sim_1 = map2(.x = pt_for_sim_1, .y = pt_boot_g1 , 
                               .f = ~ replace_coefs_in_pt(pt.frame = .x, pt.replacement = .y)),
           pt_for_sim_2 = map2(.x = pt_for_sim_2, .y = pt_boot_g2 , 
                      .f = ~ replace_coefs_in_pt(pt.frame = .x, pt.replacement = .y)))
  
  # Replace factor MEANS in pt_for_sim_2 with 0
  boots_samples <- boots_samples %>% 
    # Replace the pt_for_sim_2 column with the same list of data.frames where...
    mutate(pt_for_sim_2 = map(.x = pt_for_sim_2,
                              # ... the 'ustart' column in each data.frame is replaced by...
                              .f = ~mutate(.x, ustart = 
                                            # the same column with some values replaced.
                                            replace(ustart,
                                                    # The replaced rows are defined by:
                                                    op == "~1" & lhs %in% predefined_factors,
                                                    # and the replacing value is 0.
                                                    0))))
  
  # Replace factor VARIANCES in pt_for_sim_2 with 1 (same idea)
  boots_samples <- boots_samples %>% 
    # Replace the pt_for_sim_2 column with the same list of data.frames where...
    mutate(pt_for_sim_2 = map(.x = pt_for_sim_2,
                              # ... the 'ustart' column in each data.frame is replaced by...
                              .f = ~mutate(.x, ustart = 
                                            # the same column with some values replaced.
                                            replace(ustart,
                                                    # The replaced rows are defined by:
                                                    op == "~1" & lhs %in% predefined_factors & rhs %in% predefined_factors,
                                                    # and the replacing value is 1.
                                                    1))))        
           
  


# SIMULATE DATA

  create_data<- function (pt1, pt2, n1, n2) {
    require(lavaan)
      simulated_data.group1 <- simulateData(pt1, sample.nobs = n1)
      simulated_data.group2 <- simulateData(pt2, sample.nobs = n2)
      simulated_data        <- rbind(simulated_data.group1, simulated_data.group2)
      # Add column with group membership
      simulated_data        <- data.frame(simulated_data, 
                                          group = rep(c(1, 2), times = c(n1, n2)))}
  
  
  group_sizes <- predefined_data %>% group_by(ageMedSplit) %>% summarise("n" = n())
  n1 <- group_sizes$n[1]
  n2 <- group_sizes$n[2] #make sure lavaan interprets it the same way
  
  test <- create_data(boots_samples$pt_for_sim_1[[1]], boots_samples$pt_for_sim_2[[1]], n1, n2)
  nrow(test)
  
  boots_samples <-
  boots_samples %>% 
    mutate(sim_data = map2(.x = pt_for_sim_1, .y = pt_for_sim_2,
                           .f = ~create_data(.x, .y, n1, n2))) %>% 
    # make all variables ordered
    mutate(sim_data = map(.x = sim_data, .f = ~mutate_all(.x, ordered)))

# CALCULATE SCORE DISTRIBUTIONS
  
  # create one big data frame with indices for each simulted datset
  
  score_distributions <-
  boots_samples %>% 
    select(sample_nr, sim_data) %>% 
    unnest(sim_data) %>% 
  
  # move to long format tidy data, 3 columns: dataset, item, score
  # numer of rows = n_sample * items * participant
    gather(key = item, value = score, -c(sample_nr, group)) %>% 
    # Fix the fact that simulation creates scores [1, 2, 3] instead of [0, 1, 2]
    mutate(score = score - 1) %>% 
  # group_by dataset, item, and score 
    group_by(sample_nr, item, score, group) %>% 
    summarise(n = n()) %>% 
    ungroup 
  
  
  score_distributions %>% group_by(item) %>% 
    
    summarise(chi_test = chisq.test(x = group, y = score))
  
  score_distributions %>% 
    group_by(item, score, group) %>% 
    summarise(mean = mean(n), p2.5 = quantile(n, 0.025), p97.5 = quantile(n, 0.975)) 
  # summarise( mean, quantile 2.5 and quantile 97.5)
  # tibble with 5 columns: dataset, item, mean, q2.5, q97.5
  

  

# FIT PATH MODEL
  simulation_results <-
    simsem::sim(model     = predefined_path_model,
              rawData   = boots_samples$sim_data,
              lavaanfun = "sem",
              std.lv    = TRUE,
              estimator = "WLSMV")

  
  Sys.time()-start2



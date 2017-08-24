rm(list = ls())
set.seed(2108)

library(tidyverse)

# LOAD DATA

FinPrisonMales2 <- readRDS("C:/Users/benny_000/Dropbox/AAAKTUELLT/MI/FinPrisonMales2.rds")

# IDENTIFY REFERENT ITEMS

# create a vector of item names for items that only load on one factor
unidim_items <- subset(lavaanify(Mod6facMI), op == "=~")$rhs
# Pick items that only occur once in the parameter table
unidim_items <- names(table(unidim_items))[table(unidim_items) == 1]

model_for_all <- Mod6facMI
data_for_all <- FinPrisonMales2

# Run a set of preliminary analyses
# Median split on AGE
print(Sys.time())
start<-Sys.time()
mod1_prel_results <- analyses_step_2(base_model = model_for_all, 
                                     used_data  = data_for_all, 
                                     grouping   = "ageMedSplit", 
                                     item_vector = unidim_items)

Sys.time()-start

summary(mod1_prel_results)

# BOOTSTRAP SAMPLES  

# Function for creating onw stratified bootstrap (maintaining group sizes)
create_one_bootstrap <- function (dataset, grouping_var) {
  require(dplyr)
  dataset %>%
    group_by_(grouping_var) %>%
    # For each group, sample with replacement a number of rows equal the original number of rows
    sample_frac(size = 1, replace = TRUE) %>%
    ungroup() 
}

  
  tbl_of_b_samples <- map_df(FinPrisonMales2, rep.int, times = n_samples) %>% 
    mutate(sample_nr = rep(1:n_samples, each = nrow(FinPrisonMales2))) %>% 
    nest(- sample_nr) %>%
    mutate(boots = map(.x = data, .f = ~ create_one_bootstrap(.x, grouping_var = "ageMedSplit"))) %>% 
    select(- data) 
  
 
# FIT CONFIGURAL MODEL TO BOOTSTRAP SAMPLES AND RECORD PARAMETERS
  # Function for getting the parameter table from the results of fitting a configural invariance model
  # Uses the function run_configural_model
  get_parTable <- function(dataset, base_model, grouping_var, referent_items) {
    require(lavaan)
    fit <- run_configural_model(base_model = base_model, used_data = dataset, 
                                grouping = grouping_var, referent_items = referent_items)
    parTable(fit)
  }  
   
  # Run configural invarrinace model and record the parameter table for each bootstrap sample
  configural_pts <- lapply(tbl_of_b_samples$boots, get_parTable,
                           # arguments passed to run_configural_
                           base_model = Mod6facMI, 
                           grouping_var = "ageMedSplit", 
                           referent_items = mod1_prel_results$referent_items)
  
  
  # Add parameter tables of configural fit to the tbl
  
  tbl_of_b_samples <- tbl_of_b_samples %>% 
    mutate(pt_configural = configural_pts)
  
  # Remove the now duplicate results from the environment
  rm(configural_pts)
  
# CREATE PARAMETER TABLES THAT CAN BE USED FOR SIMULATION
  

  # Create a frame for the parameter tables in both groups: based on a single group model
  pt_single <- parTable(cfa(model = Mod6facMI,
                            data      = FinPrisonMales2,
                            std.lv    = TRUE,
                            estimator = "WLSMV",
                            do.fit    = TRUE))

  # # Set up parameter table frame twice for each sample, on for each group
  # tbl_of_b_samples <- tbl_of_b_samples %>% 
  #   mutate(pt_1 = replicate(n = nrow(tbl_of_b_samples), expr = pt_single, simplify = FALSE),
  #          pt_2 = replicate(n = nrow(tbl_of_b_samples), expr = pt_single, simplify = FALSE))
  
  # Split pt_configural into seperate tables for the two groups
  tbl_of_b_samples <- tbl_of_b_samples %>% 
    mutate(pt_configural_1 = map(pt_configural, ~filter(pt_configural, group == 1)),
           pt_configural_2 = map(pt_configural, ~filter(pt_configural, group == 2)))
  
  # For each parameter table, replace values in the 'ustart' column (user defined values that the simulation is based on)
  # with the values from the 'est' column (values estimated from the data)
  
  tbl_of_b_samples <- tbl_of_b_samples %>% 
    mutate(pt_1 = map(pt_configural_1, ~mutate(. , ustart = est)),
           pt_2 = map(pt_configural_2, ~mutate(. , ustart = est)))
  
  # Replace factor means and variance in pt2 with 0 and 1
  
  
  tbl_of_b_samples$pt_2[[1]]$op
  
  



# SIMULATE DATA

  create_data<- function (pt1, pt2, n1, n2) {
      simulated_data.group1 <- simulateData(pt1, sample.nobs = n1)
      simulated_data.group2 <- simulateData(pt2, sample.nobs = n2)
      simulated_data        <- rbind(simulated_data.group1, simulated_data.group2)
      # Add column with group membership
      simulated_data        <- data.frame(simulated_data, 
                                          group = rep(c(1, 2), times = c(n1, n2)))}
  
  
  group_sizes <- FinPrisonMales2 %>% group_by(ageMedSplit) %>% summarise("n" = n())
  n1 <- group_sizes$n[1]
  n2 <- group_sizes$n[2] #make sure lavaan interprets it the same way
  
  test <- create_data(tbl_of_b_samples$pt_1[[1]], tbl_of_b_samples$pt_2[[1]], n1, n2)
  
  
  pt1 <- tbl_of_b_samples$pt_1[[1]]
  pt2 <- tbl_of_b_samples$pt_2[[1]]
  
  pt2 <- select(pt2, -plabel)
  
  pt1$free
  
  pt_single$free
  
# CALCULATE SCORE DISTRIBUTIONS

# FIT PATH MODEL







# Functions that later functions use

# Function to modify a basic model to a model for strong invariance where 
# the variance is fixed to 1 in the first group and free in the second
# the mean is fixed to 0 in the first group and free in the second
# Argument: the basic model
modify_means_and_var_to_strong <- function(base_model) {
  require(lavaan)
  pt    <- lavaanify(base_model)
  # Extract a vector of the factors (the rows in the paramteter table that has the operator "=~",
  #   take the unique names in the 'left hand side' column from that subset of rows)
  factors          <- subset(pt, op == "=~")
  factors          <- unique(factors$lhs)
  
  # Create strings to be used in defining the configural model where
  #  the factor variances are set to 1 in first and free in second group
  #  the factor means are set to 0 in first and free in second group
  factor_variances <- paste(factors, '~~ c(1, NA) *', factors, '\n', collapse = " ")
  factor_means     <- paste(factors, '~ c(0, NA) * 1 \n', collapse = " ")
  
  # Define a model that works as base for a strong invariance model
  # factor variance and mean free in second group
  model_used      <- paste(base_model,'\n',
                           factor_variances,
                           factor_means)
  return(model_used)
  
}


# Function for getting the loading and threshold for a given item
# Argument: the name of the item
require(lavaan)
get_item_parameters <- function(base_model, items) {
  
  pt           <- lavaanify(base_model)
  
  free_loadings <- subset(pt, op == "=~" & rhs %in% items)
  free_loadings <- paste(free_loadings$lhs, free_loadings$op, free_loadings$rhs)
  
  free_parameters <- c(free_loadings,
                       paste(items, '|t1'),
                       paste(items, '|t2'))
  
  return(free_parameters)
}



# Function for running a single partial invariance model
# Loadings and thresholds constrained to be equal except for the tested item
# Will be compared to a strong invariance model
# arguments: (item       = the item in the model to be tested,
#             base_model = the model in simple form of factors and their indicators
#             grouping   = which variable should be used to form groups
#             used_data  = the data that will be used)


run_partial_fit <-  function(base_model = base_model, used_data = used_data, grouping = grouping, item){
  require(lavaan)
  # create a parameter table that can be used to identify pertinent parameters
  
  modified_model  <- modify_means_and_var_to_strong(base_model)
  free_parameters <- get_item_parameters(base_model, item)
  
  # fit model - constrain loadings and thresholds to be equal ('group.equal') and 
  # override this for 'free_parameters' ('group.partial')
  fit_partial     <- cfa(model         = modified_model, 
                         data          = used_data, 
                         std.lv        = TRUE, 
                         estimator     = "WLSMV", 
                         group         = grouping, 
                         group.equal   = c("loadings","thresholds", "residuals"), 
                         group.partial = free_parameters,
                         parameterization = "theta")
  return(fit_partial)
}


# Function for running all partial models
# Arguments:grouping    = which variable should be used to form groups - 
#                         passed to function 'run_partial_fit'
#           base_model  = the model in simple form of factors and their indicators
#                         passed to function 'run_partial_fit'
#           item_vector = atomic vector of the names of the items to test
#           used_data   = data to use
#                         passed to function 'run_partial_fit'

run_all_partial_models <- function(base_model = base_model, used_data = used_data, 
                                   grouping = grouping, item_vector = item_vector) {
  require(purrr)
  partial_models <- map(.x = item_vector, 
                        .f = run_partial_fit, 
                        base_model = base_model, 
                        grouping = grouping, 
                        used_data = used_data)
  # name elements in list
  names(partial_models) <- item_vector
  return(partial_models)
}  

# Function for fitting strong invariance model

run_strong_model <- function(base_model, used_data, grouping)  {
  require(lavaan)
  modified_model  <- modify_means_and_var_to_strong(base_model)
  strong_invariance_fit <- cfa(model     = modified_model,
                               data      = used_data,
                               std.lv    = TRUE,
                               estimator = "WLSMV",
                               group     = grouping,
                               group.equal = c("loadings", "thresholds", "residuals"),
                               parameterization = "theta")
  
}







compare_partials_to_strong <- function(partial_models_list, strong_model) {
  require(lavaan)
  require(purrr)
  require(dplyr)
  # Function for getting pertinenet statistics from ONE likelihood ratio test
  # Arguments(Fitted partial invarianc emodel, Fitted strong partial model)
  get_LRT_stat<- function(partial, strong) {
    LRTstats <- lavTestLRT(partial,strong, method = "satorra.bentler.2010")
    
    out <- data.frame("Chisq"   = LRTstats$Chisq[1],
                      "Df"               = LRTstats$Df[1],
                      "Chisq_diff"       = LRTstats$`Chisq diff`[2],
                      "Df_diff"          = LRTstats$`Df diff`[2],
                      "p-value"          = LRTstats$`Pr(>Chisq)`[2],
                      "unscaled.chidiff" = lavInspect(strong, "fit")[3]-lavInspect(partial, "fit")[3])
    out$LR_by_f                          <- out$unscaled.chidiff/ out$Df_diff           
    return(out)
  }
  
  # Using purrr:map_df, do this for all items
  test_results      <- map_df(.x = partial_models_list, .f = get_LRT_stat, strong = strong_model)
  
  # Add a column with item names
  test_results$item <- names(partial_models_list)
  
  # Add column with factor names
  pt <- dplyr::filter(.data = parTable(partial_models_list[[1]]), 
                      op == "=~" & rhs %in% test_results$item) %>%
    select(rhs, lhs)
  
  pv        <- pt$lhs
  names(pv) <- pt$rhs
  
  test_results$factor <- pv[test_results$item]
  
  # Sort according to unscaled chi difference
  return(test_results[order(test_results$LR_by_f), ])
}

# Function for creating a vector of referent items from a table of the partial invariance test results
get_referent_items <- function(fit_table) {
  require(dplyr)
  
  table <- group_by(fit_table, factor) %>%
    filter(LR_by_f == min(LR_by_f))
  
  return(table$item)
}


run_configural_model <- function(base_model, used_data, grouping, referent_items)  {
  require(lavaan)
  #  'std.lv = TRUE' will make sure that the first item in a factor is not chosen as a marker.
  #     The factor variances as defined in 'model' will however dictate the factor variances
  #  'group.equal = c("loadings", "thresholds")'
  #     combined with 'group.partial' overriding this for all parameters that should stay free
  #     will set the referent items (and only the referent items) to be equal across groups
  
  modified_model  <- modify_means_and_var_to_strong(base_model)
  
  referent_parameters <- get_item_parameters(base_model, referent_items)
  
  pt           <- lavaanify(base_model)
  
  loadings     <- subset(pt, op == "=~")
  items        <- loadings$rhs
  
  loadings     <- paste(loadings$lhs, loadings$op, loadings$rhs)
  
  all_parameters <- c(loadings,
                      paste(items, '|t1'),
                      paste(items, '|t2'))  
  
  # select the parameters not among the referent paramters, they will be freed through 'group.partial'
  free_parameters <- all_parameters[!(all_parameters %in% referent_parameters)]
  
  
  
  configural_invariance_fit <- cfa(model     = modified_model,
                                   data      = used_data,
                                   std.lv    = TRUE,
                                   estimator = "WLSMV",
                                   group     = grouping,
                                   group.equal = c("loadings", "thresholds", "residuals"),
                                   group.partial = free_parameters,
                                   parameterization = "theta")
  return(configural_invariance_fit)
}






# Function for doing all tests relating to finding referent items
run_prel_analyses <- function(base_model, used_data, grouping, item_vector) {
  results <- list()
  # 1. Run partial models
  results[["partial_fits"]] <- run_all_partial_models(base_model, used_data, grouping, item_vector)
  # 2. Run strong invariance model
  results[["strong_fit"]]   <- run_strong_model(base_model, used_data, grouping)
  # 3. Order according to unscaled chi-square difference
  # Order partial fits according to how bad the fit is (higher chi-square)
  # (unscaled chis-square is used)
  # worse fits means that constraining all other items leads to worse fit 
  # - model would benefit from keeping them free
  # i.e.  keeping the tested item free does not improve fit as much
  # - parameters are close enough in the two groups that 
  #   ... fit is not reduced very much if they are constrained to be equal
  #   ... would also show as low chisq.diff when compared to strong model
  # hence that is the best referent item
  # This in turn also means small difference in chisq between partial and strong invariance models
  results[["partial_invariance_table"]]         <- 
    compare_partials_to_strong(partial_models_list = results[["partial_fits"]], 
                               strong_model = results[["strong_fit"]])
  # 4. Assign referent items
  results[["referent_items"]]                   <- 
    get_referent_items(fit_table = results[["partial_invariance_table"]])
  # 5. Pick out the referent items to see if they meet LRT test for invariance
  results[["significance_test_referent_items"]] <- 
    subset(results[["partial_invariance_table"]],item %in% results[["referent_items"]])
  # 6. Fit configural model
  results[["configural_fit"]]                   <- 
    run_configural_model(base_model, used_data, grouping, results[["referent_items"]])
  # 7. Test invariance by comparing configural and strong invariance models
  results[["test_strong_invariance"]]           <-
    lavTestLRT(results[["configural_fit"]], results[["strong_fit"]], method = "satorra.bentler.2010")
  # 8. Record warnings
  results[["warnings"]]                         <- warnings()
  return(results)
}


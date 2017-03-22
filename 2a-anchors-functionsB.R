# Functions that later functions use

# Function to modify a basic model to a model for strong invariance where 
# the variance is fixed to 1 in the first group and free in the second
# the mean is fixed to 0 in the first group and free in the second
# Argument: the basic model
  modify_means_and_var_to_strong <- function(base_model) {
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
                         group.equal   = c("loadings","thresholds"), 
                         group.partial = free_parameters)
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
  
  modified_model  <- modify_means_and_var_to_strong(base_model)
  strong_invariance_fit <- cfa(model     = modified_model,
                                data      = used_data,
                                std.lv    = TRUE,
                                estimator = "WLSMV",
                                group     = grouping,
                                group.equal = c("loadings", "thresholds"))

}







compare_partials_to_strong <- function(partial_models_list, strong_model) {
  # Function for getting pertinenet statistics from ONE likelihood ratio test
  # Arguments(Fitted partial invarianc emodel, Fitted strong partial model)
  get_LRT_stat<- function(partial, strong) {
    LRTstats <- lavTestLRT(partial,strong, method = "satorra.bentler.2010")
    
    out <- data.frame("Chisq"   = LRTstats$Chisq[1],
                      "Df"               = LRTstats$Df[1],
                      "Chisq diff"       = LRTstats$`Chisq diff`[2],
                      "Df diff"          = LRTstats$`Df diff`[2],
                      "p-value"          = LRTstats$`Pr(>Chisq)`[2],
                      "unscaled.chidiff" = lavInspect(strong, "fit")[3]-lavInspect(partial, "fit")[3])
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
  return(test_results[order(test_results$unscaled.chidiff), ])
}

# Function for creating a vector of referent items from a table of the partial invariance test results
get_referent_items <- function(fit_table) {
  require(dplyr)
  
  table <- group_by(fit_table, factor) %>%
    filter(Chisq == max(Chisq))
  
  return(table$item)
}


run_configural_model <- function(base_model, used_data, grouping, referent_items)  {
  
    #  'std.lv = TRUE' will make sure that the first item in a factor is not chosen as a marker.
    #     The factor variances as defined in 'model' will however dictate the factor variances
    #  'group.equal = c("loadings", "thresholds")'
    #     combined with 'group.partial' overriding this for all parameters that should stay free
    #     will set the referent items (and only the referent items) to be equal across groups
  
  modified_model  <- modify_means_and_var_to_strong(base_model)
  
  referent_parameters <- get_item_parameters(base_model, referent_items)
  
  pt           <- lavaanify(base_model)
  loadings     <- subset(pt, op == "=~")$rhs
    
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
                               group.equal = c("loadings", "thresholds"),
                               group.partial = free_parameters)
  return(configural_invariance_fit)
}




# Function for doing all tests relating to finding referent items
analyses_step_2 <- function(base_model, used_data, grouping, item_vector) {
  results <- list()
  # 1. Run partial models
  start<-Sys.time()
  paste("Fitting partial invariance models. Starting at", start)
  results[["partial_fits"]] <- run_all_partial_models(base_model, used_data, grouping, item_vector)
  paste("Fitting partial invariance models completed. Time difference of", round(Sys.time()-start, digits = 2))
  # 2. Run strong invariance model
  start<-Sys.time()
  paste("Fitting strong invariance model. Starting at", start)
  results[["strong fit"]]   <- run_strong_model(base_model, used_data, grouping)
  paste("Fitting strong invariance model completed. Time difference of", round(Sys.time()-start, digits = 2))
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
  results[["partial invariance table"]] <- compare_partials_to_strong(partial_models_list = results[["partial_fits"]], 
                                                                      strong_model = results[["strong fit"]])
  # 4. Assign referent items
  results[["referent items"]] <- get_referent_items(fit_table = results[["partial invariance table"]])
  return(results)
}



# 
# 
# 
# # Function for getting pertinenet statistics from MULTIPLE likelihood ratio test
# # and sorting them accoring to unscaled chi difference
# # Arguments(List of fitted partial invariance models, Fitted strong partial model to compare against)
# find_worst_fit <- function (partial_fits_list) {
#   grab_values <- function (x) {
#     data.frame(
#       "chisq" = lavInspect(x, what = "fit")[3],
#       "df" = lavInspect(x, what = "fit")[4]
#     )}
#   unscaled_chisqs <- map_df(.x = partial_fits_list, .f = grab_values)
#   unscaled_chisqs$item <- names(partial_fits_list)
#   # Add a column for which factor the item belongs to
#   pt <- dplyr::filter(.data = parTable(partial_fits_list[[1]]),
#                       op == "=~" & rhs %in% unscaled_chisqs$item) %>%
#         select(rhs, lhs)
#   pv <- pt$lhs
#   names(pv) <- pt$rhs
# 
#   unscaled_chisqs$factor <- pv[unscaled_chisqs$item]
#   # Order according to fit
#   unscaled_chisqs[order(unscaled_chisqs$chisq, decreasing = TRUE), ]
# }
# 
# 
# 
# 
# 
# # Function for running 3 models: 1. single group, 2. configural invariance, and 3. strong invariance
# # Later functions will take this list as an argument
# # Arguments for this function:
# # A basic model defining factors and items
# # The data used
# # The variable used to form and compare groups
# # A vector of referent items in the configural invariance model
# 
# run_3_models <- function (grouping, referent_items, base_model, used_data) {
#   # Create a list for the three model fits
#   the_3_fits <- list()
#   # Single group model
#   the_3_fits[["single_group"]] <- cfa(base_model,
#                                       data = used_data,
#                                       std.lv = TRUE,
#                                       estimator = "WLSMV")
# 
#   # Grab the parameter table for this single group model
#   pt <- parameterTable(the_3_fits[["single_group"]])
# 
#   # When fitting the model:
#   #  'std.lv = TRUE' will make sure that the first item in a factor is not chosen as a marker.
#   #     The factor variances as defined in 'model' will however dictate the factor variances
#   #  'group.equal = c("loadings", "thresholds")'
#   #     combined with 'group.partial' overriding this for all parameters that should stay free
#   #     will set the referent items (and only the referent items) to be equal across groups
# 
# 
#   # Extract a vector of the factors (the rows in the paramteter table that has the operator "=~",
#   #   take the unique names in the 'left hand side' column from that subset of rows)
#   factors          <- subset(pt, op == "=~")
#   factors          <- unique(factors$lhs)
# 
#   # Create strings to be used in defining the configural and strong models where
#   #  the factor variances are set to 1 in first and free in second group
#   #  the factor means are set to 0 in first and free in second group
#   factor_variances <- paste(factors, '~~ c(1, NA) *', factors, '\n', collapse = " ")
#   factor_means     <- paste(factors, '~ c(0, NA) * 1 \n', collapse = " ")
# 
#   # Stop with a warning if referent_item is not in the model
#   if (!all(referent_items %in% subset(pt, op == "=~")$rhs)) {
#     stop ("Referent item is not an item in the model", call. = FALSE)
#   }
# 
#   # Create vector of free loadings
#   #  (rows in the parameter table with the operator "=~")
#   #     (exclude rows with the referent items on the 'rhs' columns)
#   #  (create of vector of character strings where the three columns are combined)
# 
#   free_loadings    <- subset(pt, op == "=~" & !(rhs %in% referent_items))
#   free_loadings    <- paste(free_loadings$lhs, free_loadings$op, free_loadings$rhs)
# 
#   # Create vector of free thresholds
#   free_thresholds  <- subset(pt, op == "|" & !(lhs %in% referent_items))
#   free_thresholds  <- paste(free_thresholds$lhs, free_thresholds$op, free_thresholds$rhs)
# 
#   # Define a modified model for configural and strong invariance
#   # (with factor variances and means free in the second group)
#   modified_model <- paste(base_model,'\n',
#                             factor_variances,
#                             factor_means)
# 
# 
#   # Configural invariance model
#   # Referent item constrained to be equal by constraining all loadings to be equal
#   # Non-referent items free via 'group.partial' argument
#   the_3_fits[["configural"]]   <- cfa(model = modified_model,
#                                         data = used_data,
#                                         std.lv = TRUE,
#                                         group = grouping,
#                                         estimator = "WLSMV",
#                                         group.equal = c("loadings", "thresholds"),
#                                         group.partial = c(free_loadings, free_thresholds))
#   the_3_fits[["strong"]]       <- cfa(model = modified_model,
#                                         data = used_data,
#                                         std.lv = TRUE,
#                                         estimator = "WLSMV",
#                                         group = grouping,
#                                         group.equal = c("loadings", "thresholds"))
# 
#   return(the_3_fits)
# }
# 
# 
# 
# 
# 
# 
# # # TESTING parallel processing
# # print(Sys.time())
# # start<-Sys.time()
# # # run_all_partial_models_parallel <- function(grouping, base_model, item_vector, used_data) {
# # require(parallel)
# #   # Calculate the number of cores
# #   no_cores <- detectCores() - 1
# #
# #   # Initiate cluster
# #   cl <- makeCluster(no_cores)
# #
# #   clusterExport(cl, c("model_for_all", "unidim_items", "data_for_all"))
# #   clusterEvalQ(cl, library(lavaan))
# #
# #   partial_models <- parLapply(cl  = cl,
# #                               X   = unidim_items,
# #                               fun = run_partial_fit,
# #                               grouping   = "violentCrime",
# #                               base_model = model_for_all,
# #                               used_data  = data_for_all)
# #
# #   stopCluster(cl)
# #
# #   # name elements in list
# #   names(partial_models) <- item_vector
# #   partial_models
# # Sys.time()-start
# #
# #
# #
# #
# 
# 


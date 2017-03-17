# Function for running a single partial invariance model
  # Loadings and thresholds constrained to be equal except for the tested item
  # Will be compared to a strong invariance model
  # arguments: (item       = the item in the model to be tested,
  #             base_model = the model in simple form of factors and their indicators
  #             grouping   = which variable should be used to form groups
  #             used_data  = the data that will be used)


run_partial_fit <-  function(item, base_model = base_model, grouping = grouping, used_data){
  # create a parameter table that can be used to identify pertinent parameters
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
  

  
  # grab parameters that will remain free: loading and thresholds for the tested item 
  free_loading <- subset(pt, op == "=~" & rhs == item)
  free_loading <- paste(free_loading$lhs, free_loading$op, free_loading$rhs)
  
  free_parameters <- c(free_loading,
                       paste(item, '|t1'),
                       paste(item, '|t2'))
  
  # fit model - constrain loadings and thresholds to be equal ('group.equal') and 
  # override this for 'free_parameters' ('group.partial')
  fit_partial     <- cfa(model         = model_used, 
                         data          = used_data, 
                         std.lv        = TRUE, 
                         estimator     = "WLSMV", 
                         group         = grouping, 
                         group.equal   = c("loadings","thresholds"), 
                         group.partial = free_parameters)
}


# Function for running all partial models
  # Arguments:grouping    = which variable should be used to form groups - 
  #                         passed to function 'run_partial_fit.d'
  #           base_model  = the model in simple form of factors and their indicators
  #                         passed to function 'run_partial_fit.d'
  #           item_vector = atomic vector of the names of the items to test
  #           used_data   = data to use
  #                         passed to function 'run_partial_fit.d'

run_all_partial_models <- function(grouping, base_model, item_vector, used_data) {
  # using purrr::map
  partial_models <- map(.x = item_vector, 
                        .f = run_partial_fit, 
                        base_model = base_model, 
                        grouping = grouping, 
                        used_data = used_data)
  # name elements in list
  names(partial_models) <- item_vector
  return(partial_models)
}  

# Function for getting pertinenet statistics from MULTIPLE likelihood ratio test
# and sorting them accoring to unscaled chi difference
# Arguments(List of fitted partial invariance models, Fitted strong partial model to compare against)
find_worst_fit <- function (partial_fits_list) {
  grab_values <- function (x) {
    data.frame(
      "chisq" = lavInspect(x, what = "fit")[3],
      "df" = lavInspect(x, what = "fit")[4]
    )}
  unscaled_chisqs <- map_df(.x = partial_fits_list, .f = grab_values)
  unscaled_chisqs$item <- names(partial_fits_list)
  # Add a column with which factor the item belongs to
  pt <- dplyr::filter(.data = parTable(partial_fits_list[[1]]), 
                      op == "=~" & rhs %in% unscaled_chisqs$item) %>%
        select(rhs, lhs)
  pv <- pt$lhs
  names(pv) <- pt$rhs
  
  unscaled_chisqs$factor <- pv[unscaled_chisqs$item]
  # Order according to fit
  unscaled_chisqs[order(unscaled_chisqs$chisq, decreasing = TRUE), ]
}


get_referent_items <- function(fit_table) {
  require(dplyr)
  table <- group_by(fit_table, factor) %>%
    filter(chisq == max(chisq))
  table$item
}



# Function for running 3 models: 1. single group, 2. configural invariance, and 3. strong invariance
# Later functions will take this list as an argument
# Arguments for this function:
# A basic model defining factors and items
# The data used
# The variable used to form and compare groups
# A vector of referent items in the configural invariance model

run_3_models <- function (grouping, referent_items, base_model, used_data) {
  # Create a list for the three model fits
  the_3_fits <- list()
  # Single group model
  the_3_fits[["single_group"]] <- cfa(base_model, 
                                      data = used_data, 
                                      std.lv = TRUE, 
                                      estimator = "WLSMV")

  # Grab the parameter table for this single group model
  pt <- parameterTable(the_3_fits[["single_group"]])
   
  # When fitting the model:
  #  'std.lv = TRUE' will make sure that the first item in a factor is not chosen as a marker.
  #     The factor variances as defined in 'model' will however dictate the factor variances
  #  'group.equal = c("loadings", "thresholds")'
  #     combined with 'group.partial' overriding this for all parameters that should stay free 
  #     will set the referent items (and only the referent items) to be equal across groups    
  
  
  # Extract a vector of the factors (the rows in the paramteter table that has the operator "=~",
  #   take the unique names in the 'left hand side' column from that subset of rows)
  factors          <- subset(pt, op == "=~")
  factors          <- unique(factors$lhs)
  
  # Create strings to be used in defining the configural and strong models where
  #  the factor variances are set to 1 in first and free in second group
  #  the factor means are set to 0 in first and free in second group
  factor_variances <- paste(factors, '~~ c(1, NA) *', factors, '\n', collapse = " ")
  factor_means     <- paste(factors, '~ c(0, NA) * 1 \n', collapse = " ")
  
  # Stop with a warning if referent_item is not in the model
  if (!all(referent_items %in% subset(pt, op == "=~")$rhs)) {
    stop ("Referent item is not an item in the model", call. = FALSE)
  }
  
  # Create vector of free loadings
  #  (rows in the parameter table with the operator "=~")
  #     (exclude rows with the referent items on the 'rhs' columns)
  #  (create of vector of character strings where the three columns are combined)
  
  free_loadings    <- subset(pt, op == "=~" & !(rhs %in% referent_items))
  free_loadings    <- paste(free_loadings$lhs, free_loadings$op, free_loadings$rhs)
  
  # Create vector of free thresholds
  free_thresholds  <- subset(pt, op == "|" & !(lhs %in% referent_items))
  free_thresholds  <- paste(free_thresholds$lhs, free_thresholds$op, free_thresholds$rhs)
  
  # Define a modified model for configural and strong invariance
  # (with factor variances and means free in the second group)
  modified_model <- paste(base_model,'\n',
                            factor_variances,
                            factor_means) 
  
  
  # Configural invariance model
  # Referent item constrained to be equal by constraining all loadings to be equal
  # Non-referent items free via 'group.partial' argument
  the_3_fits[["configural"]]   <- cfa(model = modified_model,
                                        data = used_data,
                                        std.lv = TRUE,
                                        group = grouping,
                                        estimator = "WLSMV",
                                        group.equal = c("loadings", "thresholds"),
                                        group.partial = c(free_loadings, free_thresholds))
  the_3_fits[["strong"]]       <- cfa(model = modified_model,
                                        data = used_data,
                                        std.lv = TRUE,
                                        estimator = "WLSMV",
                                        group = grouping,
                                        group.equal = c("loadings", "thresholds"))
  
  return(the_3_fits)
}


compare_partials_to_strong <- function(partial_models_list, strong_model = strong_model_for_all) {
  # Function for getting pertinenet statistics from ONE likelihood ratio test
  # Arguments(Fitted partial invariancemodel, Fitted strong partial model)
  get_LRT_stat<- function(partial, strong) {
    LRTstats <- lavTestLRT(partial,strong, method = "satorra.bentler.2010")
    
    out <- data.frame("Chisq"   = LRTstats$Chisq[1],
                      "Df"               = LRTstats$Df[1],
                      "Chisq diff"       = LRTstats$`Chisq diff`[2],
                      "Df diff"          = LRTstats$`Df diff`[2],
                      "p-value"          = LRTstats$`Pr(>Chisq)`[2],
                      "unscaled.chidiff" = inspect(strong, "fit")[3]-inspect(partial, "fit")[3])
    return(out)
  }
  # Using purrr:map_df
  test_results            <- map_df(.x = partial_models_list, .f = get_LRT_stat, strong = strong_model)
  # name rows in data frame
  row.names(test_results) <- names(partial_models_list)
  # sort according to unscaled chi difference
  return(test_results[order(test_results$unscaled.chidiff), ])
}



# # TESTING parallel processing
# print(Sys.time())
# start<-Sys.time()
# # run_all_partial_models_parallel <- function(grouping, base_model, item_vector, used_data) {
# require(parallel)
#   # Calculate the number of cores
#   no_cores <- detectCores() - 1
#   
#   # Initiate cluster
#   cl <- makeCluster(no_cores)
#   
#   clusterExport(cl, c("model_for_all", "unidim_items", "data_for_all"))
#   clusterEvalQ(cl, library(lavaan))
#   
#   partial_models <- parLapply(cl  = cl, 
#                               X   = unidim_items,
#                               fun = run_partial_fit,
#                               grouping   = "violentCrime",
#                               base_model = model_for_all,
#                               used_data  = data_for_all)
#   
#   stopCluster(cl)
#   
#   # name elements in list
#   names(partial_models) <- item_vector
#   partial_models
# Sys.time()-start  
# 
# 
# 
# 




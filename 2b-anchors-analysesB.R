



# Define preset objects that will be used in all five groupings

# create a vector of item names for items that only load on one factor
unidim_items <- subset(lavaanify(Mod6facMI), op == "=~")$rhs
unidim_items <- names(table(unidim_items))[table(unidim_items) == 1]
#   # exclude "i_remedialTeaching" with only 1 threshold
# unidim_items <- unidim_items[unidim_items != "i_remedialTeaching"]

model_for_all <- Mod6facMI
data_for_all <- FinPrisonMales2



# Median split on AGE
print(Sys.time())
start<-Sys.time()
analyses_step_2(base_model = model_for_all, 
                used_data  = data_for_all, 
                grouping   = "ageMedSplit", 
                item_vector = unidim_items)

Sys.time()-start

# 
#       # 1. Run partial models
# 
# print(Sys.time())
# start<-Sys.time()
# results.age <- list()
# results.age[["partial fits"]]        <- run_all_partial_models(grouping = "ageMedSplit",
#                                                  base_model = model_for_all,
#                                                  item_vector = unidim_items,
#                                                  used_data = data_for_all)
# 
# Sys.time()-start
# 
# 
#       # 2. Order according to unsncaled chi-square and assign referent items
# 
# results.age[["partial fits table"]]  <- find_worst_fit(results.age[["partial fits"]])
# results.age[["referent items"]]      <- get_referent_items(results.age[["partial fits table"]])
# 
#       # 3. Fit models - including strong invariance models
# three_fits.age         <- run_3_models(grouping = "ageMedSplit", 
#                                        referent_items = referent_items.age, 
#                                        base_model = Mod6facMI, used_data = FinPrisonMales2)
# 
#       # 4. Likelihood tests partial fits vs. strong invariance
# 
# lrt_tests_partial.age <- compare_partials_to_strong(partial_fits.age,       three_fits.age[["strong"]]) 
# lrt_tests_partial.age[lrt_tests_partial.age$item %in% referent_items.age, ]
#   
#       # 5. Remove the list of partial fits to free up space.
# rm(partial_fits.age)
# 
# 
# ### VIOLENCE
#       # 1. Run partial models
# 
# print(Sys.time())
# start<-Sys.time()
# 
# partial_fits.violence  <- run_all_partial_models(grouping = "violentCrime",
#                                                  base_model = model_for_all,
#                                                  item_vector = unidim_items,
#                                                  used_data = data_for_all)
# Sys.time()-start
# 
# 
#       # 2. Order according to unsncaled chi-square and assign referent items
# 
# 
# partial_fits_table.violence <- find_worst_fit(partial_fits.violence)
# referent_items.violence     <- get_referent_items(partial_fits_table.violence)
# 
#       # 3. Fit models - including strong invariance models
# 
# 
# three_fits.violence    <- run_3_models(grouping = "violentCrime", 
#                                        referent_item = referent_items.violence, 
#                                        base_model = Mod6facMI, used_data = FinPrisonMales2)
# 
# 
#       # 4. Likelihood tests partial fits vs. strong invariance
# 
# lrt_tests_partial.violence <- compare_partials_to_strong(partial_fits.violence,       three_fits.violence[["strong"]]) 
# lrt_tests_partial.violence[lrt_tests_partial.violence$item %in% referent_items.violence, ]
# 
#       # 5. Remove the list of partial fits to free up space.
# 
# rm(partial_fits.violence)
# 
# ### PREVIOUS
# # 1. Run partial models
# 
# print(Sys.time())
# start<-Sys.time()
# 
# 
# partial_fits.previous  <- run_all_partial_models(grouping = "prevReoffence",
#                                                  base_model = model_for_all,
#                                                  item_vector = unidim_items,
#                                                  used_data = data_for_all)
# 
# Sys.time()-start
# 
# 
# # 2. Order according to unsncaled chi-square and assign referent items
# 
# partial_fits_table.previous <- find_worst_fit(partial_fits.previous)
# referent_items.previous     <- get_referent_items(partial_fits_table.previous)
# 
# # 3. Fit models - including strong invariance models
# 
# three_fits.previous    <- run_3_models(grouping = "prevReoffence", 
#                                        referent_item = referent_items.previous, 
#                                        base_model = Mod6facMI, used_data = FinPrisonMales2)
# 
# 
# # 4. Likelihood tests partial fits vs. strong invariance
# 
# lrt_tests_partial.previous <- compare_partials_to_strong(partial_fits.previous,       three_fits.previous[["strong"]]) 
# lrt_tests_partial.previous <- lrt_tests_partial.previous$item %in% referent_items.previous, ]
# 
# 
# # 5. Remove the list of partial fits to free up space.
# 
# rm(partial_fits.previous)
# 
# ### REOFFENCE
# # 1. Run partial models
# 
# print(Sys.time())
# start<-Sys.time()
# 
# partial_fits.reoffence <- run_all_partial_models(grouping = "reoffender",
#                                                  base_model = model_for_all,
#                                                  item_vector = unidim_items,
#                                                  used_data = data_for_all)
# 
# Sys.time()-start
# 
# # 2. Order according to unsncaled chi-square and assign referent items
# 
# ## According to reoffenders (this time) vs. non-reoffenders
# partial_fits_table.reoffence <- find_worst_fit(partial_fits.reoffence)
# referent_items.reoffence     <- get_referent_items(partial_fits_table.reoffence)
# 
# # 3. Fit models - including strong invariance models
# 
# three_fits.reoffence   <- run_3_models(grouping = "reoffender", 
#                                        referent_item = referent_items.reoffence, 
#                                        base_model = Mod6facMI, used_data = FinPrisonMales2)
# 
# # 4. Likelihood tests partial fits vs. strong invariance
# 
# lrt_tests_partial.reoffence <- compare_partials_to_strong(partial_fits.reoffence,       three_fits.reoffence[["strong"]]) 
# lrt_tests_partial.reoffence[lrt_tests_partial.reoffence$item %in% referent_items.reoffence, ]
# 
# 
# # 5. Remove the list of partial fits to free up space.
# 
# rm(partial_fits.reoffence)
# 
# 
# 
# ### CLOSED
# # 1. Run partial models
# 
# print(Sys.time())
# start<-Sys.time()
# 
# partial_fits.closed    <- run_all_partial_models(grouping = "allClosed",
#                                                  base_model = model_for_all,
#                                                  item_vector = drug_items,
#                                                  used_data = data_for_all)
# Sys.time()-start
# 
# # 2. Order according to unsncaled chi-square and assign referent items
# 
# partial_fits_table.closed   <- find_worst_fit(partial_fits.closed)
# referent_items.closed      <- get_referent_items(partial_fits_table.closed)
# 
# # 3. Fit models - including strong invariance models
# 
# three_fits.closed      <- run_3_models(grouping = "allClosed", 
#                                        referent_item = referent_items.closed, 
#                                        base_model = Mod6facMI, used_data = FinPrisonMales2)
# 
# # 4. Likelihood tests partial fits vs. strong invariance
# 
# lrt_tests_partial.closed <- compare_partials_to_strong(partial_fits.closed,       three_fits.closed[["strong"]]) 
# lrt_tests_partial.closed[lrt_tests_partial.age$item %in% referent_items.closed, ]
# 
# 
# # 5. Remove the list of partial fits to free up space.
# 
# rm(partial_fits.closed)
# 
# 
# 
# # Save workspace
# save.image("~/Dropbox/to aws/MI workspace after 2b.RData")
# 
# 

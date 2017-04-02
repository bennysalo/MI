
#### CHECK IMPACT OF MEASURED AMOUT OF BIAS ON SCALE LEVEL


rm(list = ls())
load("~/Dropbox/to aws/MI after 2b - age.RData")
load("~/Dropbox/to aws/impact functions.RData")

print(Sys.time())
start<-Sys.time()
results_step2.age <- add_info(results = results_step2.age, base_model = Mod6facMI, used_data = FinPrisonMales2)
results_step2.age <- all_impact_analyses(results = results_step2.age, 
                                         base_model = Mod6facMI, 
                                         used_data = FinPrisonMales2,
                                         n_sets = 2)

Sys.time()-start
save.image("~/Dropbox/to aws/MI after 2b - age.RData")



rm(list = ls())
load("~/Dropbox/to aws/MI after 2b - previous.RData")
load("~/Dropbox/to aws/impact functions.RData")

print(Sys.time())
start<-Sys.time()
results_step2.previous <- add_info(results = results_step2.previous, base_model = Mod6facMI, used_data = FinPrisonMales2)
results_step2.previous <- all_impact_analyses(results = results_step2.previous, 
                                         base_model = Mod6facMI, 
                                         used_data = FinPrisonMales2,
                                         n_sets = 2)

Sys.time()-start

# save.image("~/Dropbox/to aws/MI after 2b - previous.RData")


rm(list = ls())
load("~/Dropbox/to aws/MI after 2b - violence.RData")
load("~/Dropbox/to aws/impact functions.RData")

print(Sys.time())
start<-Sys.time()
results_step2.violence <- add_info(results = results_step2.violence, base_model = Mod6facMI, used_data = FinPrisonMales2)
results_step2.violence <- all_impact_analyses(results = results_step2.violence, 
                                              base_model = Mod6facMI, 
                                              used_data = FinPrisonMales2,
                                              n_sets = 2)

Sys.time()-start
# save.image("~/Dropbox/to aws/MI after 2b - violence.RData")


rm(list = ls())
load("~/Dropbox/to aws/MI after 2b - reoffence.RData")
load("~/Dropbox/to aws/impact functions.RData")

print(Sys.time())
start<-Sys.time()
results_step2.reoffender <- add_info(results = results_step2.reoffender, base_model = Mod6facMI, used_data = FinPrisonMales2)
results_step2.reoffender <- all_impact_analyses(results = results_step2.reoffender, 
                                              base_model = Mod6facMI, 
                                              used_data = FinPrisonMales2,
                                              n_sets = 2)

Sys.time()-start
# save.image("C:/Users/benny_000/Dropbox/to aws/MI after 2b - reoffence.RData")

rm(list = ls())
load("~/Dropbox/to aws/MI after 2b - closed.RData")
load("~/Dropbox/to aws/impact functions.RData")

print(Sys.time())
start<-Sys.time()
results_step2.closed <- add_info(results = results_step2.closed, base_model = Mod6facMI, used_data = FinPrisonMales2)
results_step2.closed <- all_impact_analyses(results = results_step2.closed,
                                                base_model = Mod6facMI, 
                                                used_data = FinPrisonMales2,
                                                n_sets = 2)

Sys.time()-start
# save.image("~/Dropbox/to aws/MI after 2b - closed.RData")



# 
# 
# 
# invariant_data.age       <- create_invariant_data(results_step2.age, iters)
# biased_data.age          <- create_biased_data(results_step2.age, iters)
# 
# invariant_data.violence  <- create_invariant_data(three_fits.violence, iters)
# biased_data.violence     <- create_biased_data(three_fits.violence, iters)
# 
# invariant_data.previous  <- create_invariant_data(three_fits.previous, iters)
# biased_data.previous     <- create_biased_data(three_fits.previous, iters)
# 
# invariant_data.reoffence <- create_invariant_data(three_fits.reoffence, iters)
# biased_data.reoffence    <- create_biased_data(three_fits.reoffence, iters)
# 
# invariant_data.closed    <- create_invariant_data(three_fits.closed, iters)
# biased_data.closed       <- create_biased_data(three_fits.closed, iters)
# 
# Sys.time()-start
# 
# 
# 
# 
# 
# # Run SEM model in the generated datasets
# 
# # Define the simulation with its arguments as far as it is repeated
#   # previousSim allows previous simulation runs to be added to the current one
#   #  (useful for breaking up the iterations in steps)
#   options('simsem.multicore' = FALSE)
#   
#   
# common_simulation <- function(rawData, previousSim = NULL) {
#   fits <- simsem::sim(model = impact_model,
#               rawData = rawData,
#               lavaanfun = "sem",
#               std.lv = TRUE,
#               estimator = "WLSMV",
#               previousSim = previousSim)
# }
# 
# print(Sys.time())
# start<-Sys.time()
# 
# 
# invariant_fits.age       <- common_simulation(rawData = invariant_data.age)
# biased_fits.age          <- common_simulation(rawData = biased_data.age)
# 
# invariant_fits.violence  <- common_simulation(rawData = invariant_data.violence)
# biased_fits.violence     <- common_simulation(rawData = biased_data.violence)
# 
# invariant_fits.previous  <- common_simulation(rawData = invariant_data.previous)
# biased_fits.previous     <- common_simulation(rawData = biased_data.previous)
# 
# invariant_fits.reoffence <- common_simulation(rawData = invariant_data.reoffence)
# biased_fits.reoffence    <- common_simulation(rawData = biased_data.reoffence)
# 
# invariant_fits.closed    <- common_simulation(rawData = invariant_data.closed)
# biased_fits.closed       <- common_simulation(rawData = biased_data.closed)
# 
# Sys.time()-start
# 
# 
# 
# 
# 
# # pick out the standardized path coefficients 
# # and the difference between invariant and biased datasets
# 
# summary(invariant_data.age[[1]])$group)
# round(get_path_difference(invariant_fits.age, biased_fits.age), digit = 3)
# 
# summary(invariant_data.violence[[1]]$group)
# round(get_path_difference(invariant_fits.violence, biased_fits.violence), digit = 3)
# 
# summary(invariant_data.previous[[1]]$group)
# round(get_path_difference(invariant_fits.previous, biased_fits.previous), digit = 3)
# 
# summary(invariant_data.reoffence[[1]]$group)
# round(get_path_difference(invariant_fits.reoffence, biased_fits.reoffence), digit = 3)
# 
# summary(invariant_data.closed[[1]]$group)
# round(get_path_difference(invariant_fits.closed, biased_fits.closed), digit = 3)
# 


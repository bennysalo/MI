# Load workspace with functions

load("~/Dropbox/to aws/MI-0-all functions and data.R.RData")



# Define preset objects that will be used in all five groupings


model_for_all <- Mod5facMI4

# create a vector of item names for items that only load on one factor
unidim_items <- subset(lavaanify(model_for_all), op == "=~")$rhs
# Pick items that only occur once in the parameter table
unidim_items <- names(table(unidim_items))[table(unidim_items) == 1]

data_for_all <- FinPrisonMales2

n <- 1000


# Median split on AGE
print(Sys.time())
start<-Sys.time()
results.age.2 <- analyses_step_2(base_model = model_for_all, 
                                 used_data  = data_for_all, 
                                 grouping   = "ageMedSplit", 
                                 item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI age 2.RData")


results.age.2 <- run_my_bootstraps(results.age.2, n_samples = n)
save.image("~/Dropbox/to aws/MI age 2.RData")
rm(list = ls())


# ### VIOLENCE
print(Sys.time())
start<-Sys.time()
results.violence.2 <- analyses_step_2(base_model = model_for_all, 
                                      used_data  = data_for_all, 
                                      grouping   = "violentCrime", 
                                      item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI violence 2.RData")

results.violence.2 <- run_my_bootstraps(results.violence.2, n_samples = n)
save.image("~/Dropbox/to aws/MI violence 2.RData")


load.image("~/Dropbox/to aws/MI violence 2.RData")
print(Sys.time())
start<-Sys.time()
results.violence.2.4 <- add_info(results = results.violence.2, base_model = Mod6facMI, used_data = FinPrisonMales2)
results.violence.2.4 <- all_impact_analyses(results = results.violence.2.4, 
                                            base_model = Mod6facMI, 
                                            used_data = FinPrisonMales2,
                                            n_sets = 1000)

Sys.time()-start

save.image("~/Dropbox/to aws/MI violence 2-4.RData")


# ### PREVIOUS

print(Sys.time())
start<-Sys.time()
results.previous.2 <- analyses_step_2(base_model = model_for_all, 
                                      used_data  = data_for_all, 
                                      grouping   = "prevReoffence", 
                                      item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI previous 2.RData")

results.previous.2 <- run_my_bootstraps(results.previous.2, n_samples = n)
save.image("~/Dropbox/to aws/MI previous 2.RData")


# ### REOFFENCE

print(Sys.time())
start<-Sys.time()
results.reoffence.2 <- analyses_step_2(base_model = model_for_all, 
                                       used_data  = data_for_all, 
                                       grouping   = "reoffender", 
                                       item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI reoffence 2.RData")

results.reoffence.2 <- run_my_bootstraps(results.reoffence.2, n_samples = n)
save.image("~/Dropbox/to aws/MI reoffence 2.RData")
rm(list = ls())



# ### CLOSED
print(Sys.time())
start<-Sys.time()
results.closed.2 <- analyses_step_2(base_model = model_for_all, 
                                    used_data  = data_for_all, 
                                    grouping   = "allClosed", 
                                    item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI closed 2.RData")

results.closed.2 <- run_my_bootstraps(results.closed.2, n_samples = n)
save.image("~/Dropbox/to aws/MI closed 2.RData")


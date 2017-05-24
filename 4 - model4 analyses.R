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
results.age.4 <- analyses_step_2(base_model = model_for_all, 
                                 used_data  = data_for_all, 
                                 grouping   = "ageMedSplit", 
                                 item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI age-4.RData")

print(Sys.time())
start<-Sys.time()
results.age.4 <- add_info(results = results.age.4, base_model = model_for_all, used_data = FinPrisonMales2)
results.age.4 <- all_impact_analyses(results = results.violence.4, 
                                          base_model = model_for_all, 
                                          used_data = FinPrisonMales2,
                                          n_sets = 1000)

Sys.time()-start


save.image("~/Dropbox/to aws/MI age-4.RData")


# ### VIOLENCE
print(Sys.time())
start<-Sys.time()
results.violence.4 <- analyses_step_2(base_model = model_for_all, 
                                      used_data  = data_for_all, 
                                      grouping   = "violentCrime", 
                                      item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI violence-4.RData")


print(Sys.time())
start<-Sys.time()
results.violence.4 <- add_info(results = results.violence.4, base_model = model_for_all, used_data = FinPrisonMales2)
results.violence.4 <- all_impact_analyses(results = results.violence.4, 
                                            base_model = model_for_all, 
                                            used_data = FinPrisonMales2,
                                            n_sets = 1000)

Sys.time()-start

save.image("~/Dropbox/to aws/MI violence-4.RData")


# ### PREVIOUS

print(Sys.time())
start<-Sys.time()
results.previous.4 <- analyses_step_2(base_model = model_for_all, 
                                      used_data  = data_for_all, 
                                      grouping   = "prevReoffence", 
                                      item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI previous-4.RData")

print(Sys.time())
start<-Sys.time()
results.previous.4 <- add_info(results = results.previous.4, base_model = model_for_all, used_data = FinPrisonMales2)
results.previous.4 <- all_impact_analyses(results = results.violence.4, 
                                          base_model = model_for_all, 
                                          used_data = FinPrisonMales2,
                                          n_sets = 1000)

Sys.time()-start

save.image("~/Dropbox/to aws/MI previous-4.RData")


# ### REOFFENCE

print(Sys.time())
start<-Sys.time()
results.reoffence.4 <- analyses_step_2(base_model = model_for_all, 
                                       used_data  = data_for_all, 
                                       grouping   = "reoffender", 
                                       item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI reoffence-4.RData")
print(Sys.time())
start<-Sys.time()
results.reoffence.4 <- add_info(results = results.reoffence.4, base_model = model_for_all, used_data = FinPrisonMales2)
results.reoffence.4 <- all_impact_analyses(results = results.violence.4, 
                                          base_model = model_for_all, 
                                          used_data = FinPrisonMales2,
                                          n_sets = 1000)

Sys.time()-start


save.image("~/Dropbox/to aws/MI reoffence-4.RData")



# ### CLOSED
print(Sys.time())
start<-Sys.time()
results.closed.4 <- analyses_step_2(base_model = model_for_all, 
                                    used_data  = data_for_all, 
                                    grouping   = "allClosed", 
                                    item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI closed-4.RData")
print(Sys.time())
start<-Sys.time()
results.closed.4 <- add_info(results = results.closed.4, base_model = model_for_all, used_data = FinPrisonMales2)
results.closed.4 <- all_impact_analyses(results = results.violence.4, 
                                          base_model = model_for_all, 
                                          used_data = FinPrisonMales2,
                                          n_sets = 1000)

Sys.time()-start

save.image("~/Dropbox/to aws/MI closed-4.RData")


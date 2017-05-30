# Load workspace with functions

load("~/Dropbox/to aws/MI-0-all functions and data.R.RData")



# Define preset objects that will be used in all five groupings


model_for_all <- Mod5facMI5

# create a vector of item names for items that only load on one factor
unidim_items <- subset(lavaanify(model_for_all), op == "=~")$rhs
# Pick items that only occur once in the parameter table
unidim_items <- names(table(unidim_items))[table(unidim_items) == 1]

data_for_all <- FinPrisonMales2

n <- 100


# Median split on AGE
print(Sys.time())
start<-Sys.time()
results.age.5 <- analyses_step_2(base_model = model_for_all, 
                                 used_data  = data_for_all, 
                                 grouping   = "ageMedSplit", 
                                 item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI age-5.RData")

print(Sys.time())
start<-Sys.time()
results.age.5 <- add_info(results = results.age.5, base_model = model_for_all, used_data = FinPrisonMales2)
results.age.5 <- all_impact_analyses(results = results.age.5, 
                                     base_model = model_for_all, 
                                     used_data = FinPrisonMales2,
                                     n_sets = n)

Sys.time()-start


save.image("~/Dropbox/to aws/MI age-5.RData")


# ### VIOLENCE
print(Sys.time())
start<-Sys.time()
results.violence.5 <- analyses_step_2(base_model = model_for_all, 
                                      used_data  = data_for_all, 
                                      grouping   = "violentCrime", 
                                      item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI violence-5.RData")


print(Sys.time())
start<-Sys.time()
results.violence.5 <- add_info(results = results.violence.5, base_model = model_for_all, used_data = FinPrisonMales2)
results.violence.5 <- all_impact_analyses(results = results.violence.5, 
                                          base_model = model_for_all, 
                                          used_data = FinPrisonMales2,
                                          n_sets = n)

Sys.time()-start

save.image("~/Dropbox/to aws/MI violence-5.RData")


# ### PREVIOUS

print(Sys.time())
start<-Sys.time()
results.previous.5 <- analyses_step_2(base_model = model_for_all, 
                                      used_data  = data_for_all, 
                                      grouping   = "prevReoffence", 
                                      item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI previous-5.RData")

print(Sys.time())
start<-Sys.time()
results.previous.5 <- add_info(results = results.previous.5, base_model = model_for_all, used_data = FinPrisonMales2)
results.previous.5 <- all_impact_analyses(results = results.previous.5, 
                                          base_model = model_for_all, 
                                          used_data = FinPrisonMales2,
                                          n_sets = n)

Sys.time()-start

save.image("~/Dropbox/to aws/MI previous-5.RData")


# ### REOFFENCE

print(Sys.time())
start<-Sys.time()
results.reoffence.5 <- analyses_step_2(base_model = model_for_all, 
                                       used_data  = data_for_all, 
                                       grouping   = "reoffender", 
                                       item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI reoffence-5.RData")
print(Sys.time())
start<-Sys.time()
results.reoffence.5 <- add_info(results = results.reoffence.5, base_model = model_for_all, used_data = FinPrisonMales2)
results.reoffence.5 <- all_impact_analyses(results = results.reoffence.5, 
                                           base_model = model_for_all, 
                                           used_data = FinPrisonMales2,
                                           n_sets = n)

Sys.time()-start


save.image("~/Dropbox/to aws/MI reoffence-5.RData")



# ### CLOSED
print(Sys.time())
start<-Sys.time()
results.closed.5 <- analyses_step_2(base_model = model_for_all, 
                                    used_data  = data_for_all, 
                                    grouping   = "allClosed", 
                                    item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI closed-5.RData")
print(Sys.time())
start<-Sys.time()
results.closed.5 <- add_info(results = results.closed.5, base_model = model_for_all, used_data = FinPrisonMales2)
results.closed.5 <- all_impact_analyses(results = results.closed.5, 
                                        base_model = model_for_all, 
                                        used_data = FinPrisonMales2,
                                        n_sets = n)

Sys.time()-start

save.image("~/Dropbox/to aws/MI closed-5.RData")


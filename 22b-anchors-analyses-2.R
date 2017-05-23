# Load workspace with functions

load("~/Dropbox/to aws/MI-0-all functions and data.R.RData")



# Define preset objects that will be used in all five groupings


model_for_all <- Mod6facMI2

# create a vector of item names for items that only load on one factor
unidim_items <- subset(lavaanify(model_for_all), op == "=~")$rhs
# Pick items that only occur once in the parameter table
unidim_items <- names(table(unidim_items))[table(unidim_items) == 1]

data_for_all <- FinPrisonMales2

save.image("~/Dropbox/to aws/MI workspace after 1a.RData")



# Median split on AGE
print(Sys.time())
start<-Sys.time()
results.age.2 <- analyses_step_2(base_model = model_for_all, 
                                     used_data  = data_for_all, 
                                     grouping   = "ageMedSplit", 
                                     item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI age 2.RData")


# ### VIOLENCE
print(Sys.time())
start<-Sys.time()
results.violence.2 <- analyses_step_2(base_model = model_for_all, 
                                          used_data  = data_for_all, 
                                          grouping   = "violentCrime", 
                                          item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI violence 2.RData")


# ### PREVIOUS

print(Sys.time())
start<-Sys.time()
results.previous.2 <- analyses_step_2(base_model = model_for_all, 
                                          used_data  = data_for_all, 
                                          grouping   = "prevReoffence", 
                                          item_vector = unidim_items)

Sys.time()-start

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


# ### CLOSED
print(Sys.time())
start<-Sys.time()
results.closed.2 <- analyses_step_2(base_model = model_for_all, 
                                        used_data  = data_for_all, 
                                        grouping   = "allClosed", 
                                        item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI closed 2.RData")



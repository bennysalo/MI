
# Define preset objects that will be used in all five groupings

# create a vector of item names for items that only load on one factor
unidim_items <- subset(lavaanify(Mod6facMI), op == "=~")$rhs
# Pick items that only occur once in the parameter table
unidim_items <- names(table(unidim_items))[table(unidim_items) == 1]

model_for_all <- Mod6facMI
data_for_all <- FinPrisonMales2

save.image("~/Dropbox/to aws/MI workspace after 1a.RData")



# Median split on AGE
print(Sys.time())
start<-Sys.time()
results_step2.age <- analyses_step_2(base_model = model_for_all, 
                     used_data  = data_for_all, 
                     grouping   = "ageMedSplit", 
                     item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI after 2b - age.RData")
# Empty worskpace and load earlier workspace to free up memory
rm(list = ls())
load("~/Dropbox/to aws/MI workspace after 1a.RData")

# ### VIOLENCE
print(Sys.time())
start<-Sys.time()
results_step2.violence <- analyses_step_2(base_model = model_for_all, 
                used_data  = data_for_all, 
                grouping   = "violentCrime", 
                item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI after 2b - violence.RData")
# Empty worskpace and load earlier workspace to free up memory
rm(list = ls())
load("~/Dropbox/to aws/MI workspace after 1a.RData")

# ### PREVIOUS

print(Sys.time())
start<-Sys.time()
results_step2.previous <- analyses_step_2(base_model = model_for_all, 
                used_data  = data_for_all, 
                grouping   = "prevReoffence", 
                item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI after 2b - previous.RData")
# Empty worskpace and load earlier workspace to free up memory
rm(list = ls())
load("~/Dropbox/to aws/MI workspace after 1a.RData")


# ### REOFFENCE

print(Sys.time())
start<-Sys.time()
results_step2.reoffender <- analyses_step_2(base_model = model_for_all, 
                used_data  = data_for_all, 
                grouping   = "reoffender", 
                item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI after 2b - reoffence.RData")
# Empty worskpace and load earlier workspace to free up memory
rm(list = ls())
load("~/Dropbox/to aws/MI workspace after 1a.RData")

# ### CLOSED
print(Sys.time())
start<-Sys.time()
results_step2.closed <- analyses_step_2(base_model = model_for_all, 
                used_data  = data_for_all, 
                grouping   = "allClosed", 
                item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI after 2b - closed.RData")
# Empty worskpace and load earlier workspace to free up memory
rm(list = ls())
load("~/Dropbox/to aws/MI workspace after 1a.RData")


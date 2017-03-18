
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

save.image("~/Dropbox/to aws/MI after 2b - age.RData")
# Load earlier workspace to free up memory
load("~/Dropbox/to aws/MI workspace after 1a.RData")

# ### VIOLENCE
print(Sys.time())
start<-Sys.time()
analyses_step_2(base_model = model_for_all, 
                used_data  = data_for_all, 
                grouping   = "violentCrime", 
                item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI after 2b - violence.RData")
# Load earlier workspace to free up memory
load("~/Dropbox/to aws/MI workspace after 1a.RData")

# ### PREVIOUS

print(Sys.time())
start<-Sys.time()
analyses_step_2(base_model = model_for_all, 
                used_data  = data_for_all, 
                grouping   = "prevReoffence", 
                item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI after 2b - previous.RData")
# Load earlier workspace to free up memory
load("~/Dropbox/to aws/MI workspace after 1a.RData")


# ### REOFFENCE

print(Sys.time())
start<-Sys.time()
analyses_step_2(base_model = model_for_all, 
                used_data  = data_for_all, 
                grouping   = "reoffender", 
                item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI after 2b - reoffence.RData")
# Load earlier workspace to free up memory
load("~/Dropbox/to aws/MI workspace after 1a.RData")

# ### CLOSED
print(Sys.time())
start<-Sys.time()
analyses_step_2(base_model = model_for_all, 
                used_data  = data_for_all, 
                grouping   = "allClosed", 
                item_vector = unidim_items)

Sys.time()-start

save.image("~/Dropbox/to aws/MI after 2b - closed.RData")
# Load earlier workspace to free up memory
load("~/Dropbox/to aws/MI workspace after 1a.RData")


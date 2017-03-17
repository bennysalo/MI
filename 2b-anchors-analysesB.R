
# Define preset objects that will be used in all five groupings

  # create a vector of item names for items that only load on one factor
unidim_items <- subset(lavaanify(Mod6facMI), op == "=~")$rhs
unidim_items <- names(table(unidim_items))[table(unidim_items) == 1]

model_for_all <- Mod6facMI
data_for_all <- FinPrisonMales2

# Fit partial invariance models

print(Sys.time())
start<-Sys.time()
partial_fits.age       <- run_all_partial_models(grouping = "ageMedSplit",
                                                     base_model = model_for_all,
                                                     item_vector = unidim_items,
                                                     used_data = data_for_all)

Sys.time()-start

print(Sys.time())
start<-Sys.time()

clusterExport(cl, c("model_for_all", "unidim_items", "data_for_all"))
partial_fits.violence  <- run_all_partial_models_parallel(grouping = "violentCrime",
                                                     base_model = model_for_all,
                                                     item_vector = unidim_items,
                                                     used_data = data_for_all)
Sys.time()-start

print(Sys.time())
start<-Sys.time()


partial_fits.previous  <- run_all_partial_models(grouping = "prevReoffence",
                                                     base_model = model_for_all,
                                                     item_vector = unidim_items,
                                                     used_data = data_for_all)

Sys.time()-start

print(Sys.time())
start<-Sys.time()

partial_fits.reoffence <- run_all_partial_models(grouping = "reoffender",
                                                     base_model = model_for_all,
                                                     item_vector = unidim_items,
                                                     used_data = data_for_all)

Sys.time()-start

print(Sys.time())
start<-Sys.time()

partial_fits.closed    <- run_all_partial_models(grouping = "allClosed",
                                                     base_model = model_for_all,
                                                     item_vector = drug_items,
                                                     used_data = data_for_all)
Sys.time()-start



# Order partial fits according to how bad the fit is (higher chi-square)
# (unscaled chis-square is used)
# worse fits means that constraining all other items leads to worse fit 
# - model would benefit from keeping them free
# i.e.  keeping the tested item free does not improve fit as much
# - parameters are close enough in the two groups that 
#   ... fit is not reduced very much if they are constrained to be equal
#   ... would also show as low chisq.diff when compared to strong model
# hence that is the best referent item

## According to median split on age
partial_fits_table.age      <- find_worst_fit(partial_fits.age)
referent_items.age          <- get_referent_items(partial_fits_table.age)

## According to violent vs. non-violent crimes 
partial_fits_table.violence <- find_worst_fit(partial_fits.violence)
referent_items.violence     <- get_referent_items(partial_fits_table.violence)

## According to first convictions vs repeat convictions
partial_fits_table.previous <- find_worst_fit(partial_fits.previous)
referent_items.previous     <- get_referent_items(partial_fits_table.previous)

## According to reoffenders (this time) vs. non-reoffenders
partial_fits_table.violence <- find_worst_fit(partial_fits.reoffence)
referent_items.age          <- get_referent_items(partial_fits_table.age)

## According to entire sentence in closed prison vs. open prison or conditional release
partial_fits_table.closed   <- find_worst_fit(partial_fits.closed)
referent_items.closed      <- get_referent_items(partial_fits_table.closed)


# Fit models - including strong invariance models
three_fits.age         <- run_3_models(grouping = "ageMedSplit", 
                                       referent_items = referent_items.age, 
                                       base_model = Mod6facMI, used_data = FinPrisonMales2)

three_fits.violence    <- run_3_models(grouping = "violentCrime", 
                                       referent_item = referent_items.violence, 
                                       base_model = Mod6facMI, used_data = FinPrisonMales2)

three_fits.previous    <- run_3_models(grouping = "prevReoffence", 
                                       referent_item = referent_items.previous, 
                                       base_model = Mod6facMI, used_data = FinPrisonMales2)

three_fits.reoffence   <- run_3_models(grouping = "reoffender", 
                                       referent_item = referent_items.reoffence, 
                                       base_model = Mod6facMI, used_data = FinPrisonMales2)

three_fits.closed      <- run_3_models(grouping = "allClosed", 
                                       referent_item = referent_items.closed, 
                                       base_model = Mod6facMI, used_data = FinPrisonMales2)



# Get the pertinent stats for comparing partial and strong invariance models
# To check if the referent item (and others) can be deemed invariant using
# likelihood ratio tests.

compare_partials_to_strong(partial_fits.age,       three_fits.age[["strong"]]) 

compare_partials_to_strong(partial_fits.violence,  three_fits.violence[["strong"]]) 

compare_partials_to_strong(partial_fits.previous,  three_fits.previous[["strong"]]) 

compare_partials_to_strong(partial_fits.reoffence, three_fits.reoffence[["strong"]]) 

compare_partials_to_strong(partial_fits.closed,    three_fits.closed[["strong"]]) 

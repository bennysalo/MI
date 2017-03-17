
# Define preset objects that will be used in all five groupings

  # create a vector of item names for items that only load on one factor
unidim_items <- subset(lavaanify(Mod6facMI), op == "=~")$rhs
unidim_items <- names(table(unidim_items))[table(unidim_items) == 1]

model_for_all <- Mod6facMI
data_for_all <- FinPrisonMales2

# Fit partial invariance models




# ANALYSES PER GROUPING
# 1. Run partial models
# 2. Order according to unsncaled chi-square and assign referent items
    # Order partial fits according to how bad the fit is (higher chi-square)
    # (unscaled chis-square is used)
    # worse fits means that constraining all other items leads to worse fit 
    # - model would benefit from keeping them free
    # i.e.  keeping the tested item free does not improve fit as much
    # - parameters are close enough in the two groups that 
    #   ... fit is not reduced very much if they are constrained to be equal
    #   ... would also show as low chisq.diff when compared to strong model
    # hence that is the best referent item
# 3. Fit three models: single group, configural invariance, and strong invariance
# 4. # Get the pertinent stats for comparing partial and strong invariance model
  # to check if the referent item (and others) can be deemed invariant using likelihood ratio tests.
# 5. Delete list of partial tests to free up memory


  # Median split on AGE

      # 1. Run partial models

print(Sys.time())
start<-Sys.time()
partial_fits.age       <- run_all_partial_models(grouping = "ageMedSplit",
                                                 base_model = model_for_all,
                                                 item_vector = unidim_items,
                                                 used_data = data_for_all)

Sys.time()-start


      # 2. Order according to unsncaled chi-square and assign referent items

partial_fits_table.age      <- find_worst_fit(partial_fits.age)
referent_items.age          <- get_referent_items(partial_fits_table.age)

      # 3. Fit models - including strong invariance models
three_fits.age         <- run_3_models(grouping = "ageMedSplit", 
                                       referent_items = referent_items.age, 
                                       base_model = Mod6facMI, used_data = FinPrisonMales2)

      # 4. Likelihood tests partial fits vs. strong invariance

lrt_tests_partial.age <- compare_partials_to_strong(partial_fits.age,       three_fits.age[["strong"]]) 
lrt_tests_partial.age[lrt_tests_partial.age$item %in% referent_items.age, ]
  
      # 5. Remove the list of partial fits to free up space.
rm(partial_fits.age)


### VIOLENCE
      # 1. Run partial models

print(Sys.time())
start<-Sys.time()

partial_fits.violence  <- run_all_partial_models(grouping = "violentCrime",
                                                 base_model = model_for_all,
                                                 item_vector = unidim_items,
                                                 used_data = data_for_all)
Sys.time()-start


      # 2. Order according to unsncaled chi-square and assign referent items


partial_fits_table.violence <- find_worst_fit(partial_fits.violence)
referent_items.violence     <- get_referent_items(partial_fits_table.violence)

      # 3. Fit models - including strong invariance models


three_fits.violence    <- run_3_models(grouping = "violentCrime", 
                                       referent_item = referent_items.violence, 
                                       base_model = Mod6facMI, used_data = FinPrisonMales2)


      # 4. Likelihood tests partial fits vs. strong invariance

lrt_tests_partial.violence <- compare_partials_to_strong(partial_fits.violence,       three_fits.violence[["strong"]]) 
lrt_tests_partial.violence[lrt_tests_partial.violence$item %in% referent_items.violence, ]

      # 5. Remove the list of partial fits to free up space.

rm(partial_fits.violence)

### PREVIOUS
# 1. Run partial models

print(Sys.time())
start<-Sys.time()


partial_fits.previous  <- run_all_partial_models(grouping = "prevReoffence",
                                                 base_model = model_for_all,
                                                 item_vector = unidim_items,
                                                 used_data = data_for_all)

Sys.time()-start


# 2. Order according to unsncaled chi-square and assign referent items

partial_fits_table.previous <- find_worst_fit(partial_fits.previous)
referent_items.previous     <- get_referent_items(partial_fits_table.previous)

# 3. Fit models - including strong invariance models

three_fits.previous    <- run_3_models(grouping = "prevReoffence", 
                                       referent_item = referent_items.previous, 
                                       base_model = Mod6facMI, used_data = FinPrisonMales2)


# 4. Likelihood tests partial fits vs. strong invariance

lrt_tests_partial.previous <- compare_partials_to_strong(partial_fits.previous,       three_fits.previous[["strong"]]) 
lrt_tests_partial.previous <- lrt_tests_partial.previous$item %in% referent_items.previous, ]


# 5. Remove the list of partial fits to free up space.

rm(partial_fits.previous)

### REOFFENCE
# 1. Run partial models

print(Sys.time())
start<-Sys.time()

partial_fits.reoffence <- run_all_partial_models(grouping = "reoffender",
                                                 base_model = model_for_all,
                                                 item_vector = unidim_items,
                                                 used_data = data_for_all)

Sys.time()-start

# 2. Order according to unsncaled chi-square and assign referent items

## According to reoffenders (this time) vs. non-reoffenders
partial_fits_table.reoffence <- find_worst_fit(partial_fits.reoffence)
referent_items.reoffence     <- get_referent_items(partial_fits_table.reoffence)

# 3. Fit models - including strong invariance models

three_fits.reoffence   <- run_3_models(grouping = "reoffender", 
                                       referent_item = referent_items.reoffence, 
                                       base_model = Mod6facMI, used_data = FinPrisonMales2)

# 4. Likelihood tests partial fits vs. strong invariance

lrt_tests_partial.reoffence <- compare_partials_to_strong(partial_fits.reoffence,       three_fits.reoffence[["strong"]]) 
lrt_tests_partial.reoffence[lrt_tests_partial.reoffence$item %in% referent_items.reoffence, ]


# 5. Remove the list of partial fits to free up space.

rm(partial_fits.reoffence)



### CLOSED
# 1. Run partial models

print(Sys.time())
start<-Sys.time()

partial_fits.closed    <- run_all_partial_models(grouping = "allClosed",
                                                 base_model = model_for_all,
                                                 item_vector = drug_items,
                                                 used_data = data_for_all)
Sys.time()-start

# 2. Order according to unsncaled chi-square and assign referent items

partial_fits_table.closed   <- find_worst_fit(partial_fits.closed)
referent_items.closed      <- get_referent_items(partial_fits_table.closed)

# 3. Fit models - including strong invariance models

three_fits.closed      <- run_3_models(grouping = "allClosed", 
                                       referent_item = referent_items.closed, 
                                       base_model = Mod6facMI, used_data = FinPrisonMales2)

# 4. Likelihood tests partial fits vs. strong invariance

lrt_tests_partial.closed <- compare_partials_to_strong(partial_fits.closed,       three_fits.closed[["strong"]]) 
lrt_tests_partial.closed[lrt_tests_partial.age$item %in% referent_items.closed, ]


# 5. Remove the list of partial fits to free up space.

rm(partial_fits.closed)



# Save workspace
save.image("~/Dropbox/to aws/MI workspace after 2b.RData")



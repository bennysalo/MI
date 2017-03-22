
# 'Run bootstraps for configural model
saved_seed <- .Random.seed

n_samples <- 10

RNGkind("L'Ecuyer-CMRG")

### Add iseed argument


print(Sys.time())
start<-Sys.time()
boot_drugs.age        <- bootstrapLavaan(results_step2.age[["configural fit"]], 
                                         R = n_samples, type="ordinary", FUN = coef_diff)

#### edited up to here


boot_drugs.violence   <- bootstrapLavaan(three_fits.violence[["configural"]], 
                                         R = n_samples, type="ordinary", FUN = coef_diff)
boot_drugs.previous   <- bootstrapLavaan(three_fits.previous[["configural"]], 
                                         R = n_samples, type="ordinary", FUN = coef_diff)
boot_drugs.reoffence <- bootstrapLavaan(three_fits.reoffence[["configural"]], 
                                         R = n_samples, type="ordinary", FUN = coef_diff)
boot_drugs.closed     <- bootstrapLavaan(three_fits.closed[["configural"]], 
                                         R = n_samples, type="ordinary", FUN = coef_diff)
Sys.time()-start

save.image("C:/Users/benny_000/Dropbox/AAAKTUELLT/MI/MI in drug scale/december2016.RData")



# Grab the standardised differences from the original data
std.diff_and_CIs.age        <- create_sdiff_CI_df(three_fits.age[["configural"]],     
                                                  boot_drugs.age)
std.diff_and_CIs.violence   <- create_sdiff_CI_df(three_fits.violence[["configural"]],
                                                  boot_drugs.violence)
std.diff_and_CIs.previous   <- create_sdiff_CI_df(three_fits.previous[["configural"]],
                                                  boot_drugs.previous)
std.diff_and_CIs.reoffence  <- create_sdiff_CI_df(three_fits.reoffence[["configural"]], 
                                                  boot_drugs.reoffence)
std.diff_and_CIs.closed     <- create_sdiff_CI_df(three_fits.closed[["configural"]],
                                                  boot_drugs.closed)

## Prepare a data.frame for plotting

# Give each row an indication of to which grouping the test pertains 
std.diff_and_CIs.age$grouping       <- "Younger"
std.diff_and_CIs.violence$grouping  <- "Violent crime"
std.diff_and_CIs.previous$grouping  <- "Previous reoffence"
std.diff_and_CIs.reoffence$grouping <- "Reoffence this time"
std.diff_and_CIs.closed$grouping    <- "All of sentence in closed"



# Combine all parameters to one data frame

std.diff_and_CIs.all_5 <- rbind(std.diff_and_CIs.age,
                                std.diff_and_CIs.violence,
                                std.diff_and_CIs.previous,
                                std.diff_and_CIs.reoffence,
                                std.diff_and_CIs.closed)

# Give order for the grouping that I find logical for plotting
std.diff_and_CIs.all_5$grouping <- factor(std.diff_and_CIs.all_5$grouping,
                                          levels = c("Younger",
                                                     "Previous reoffence",
                                                     "Violent crime",
                                                     "All of sentence in closed",
                                                     "Reoffence this time"))

# Create a column indicating which item is tested

std.diff_and_CIs.all_5$item <- rownames(std.diff_and_CIs.all_5)
std.diff_and_CIs.all_5$item <- sub(pattern = "^.*=~", replacement = "", x = std.diff_and_CIs.all_5$item)
std.diff_and_CIs.all_5$item <- sub(pattern = " \\| t1.*$", replacement = "", 
                                   x = std.diff_and_CIs.all_5$item)
std.diff_and_CIs.all_5$item <- sub(pattern = " \\| t2.*$", replacement = "", 
                                   x = std.diff_and_CIs.all_5$item)
std.diff_and_CIs.all_5$item <- sub(pattern = " ", replacement = "", 
                                   x = std.diff_and_CIs.all_5$item)
std.diff_and_CIs.all_5$item <- sub(pattern = " .*$", replacement = "", 
                                   x = std.diff_and_CIs.all_5$item)

# Create a column indicating which paramameter type is tested

std.diff_and_CIs.all_5$parameter_type[grepl(pattern = "=~",
                                            rownames(std.diff_and_CIs.all_5))] <- "Loading"
std.diff_and_CIs.all_5$parameter_type[grepl(pattern = "t1",
                                            rownames(std.diff_and_CIs.all_5))] <- "Threshold 1"
std.diff_and_CIs.all_5$parameter_type[grepl(pattern = "t2",
                                            rownames(std.diff_and_CIs.all_5))] <- "Threshold 2"

# Create plot
#   The coordinates will be flipped

p <- ggplot(data = std.diff_and_CIs.all_5, 
            # set aestetics
                # Seperate rows for each parameter (at this stage x-axis)
            aes(x = item, 
                # Plot the standardized difference (at this stage on the y-axis)
                y = std_diff, 
                # Used the confidence inteval as ymin and ymax
                ymin = boot_2.5p, ymax = boot_97.5p, 
                # Group and color accordning to grouping
                group = grouping, col = grouping)) +
            # Plot point estimate and confidence interval
            geom_pointrange(
              # Avoid overlapping
              position = position_dodge(width = 0.5)) + 
            # Flip coordinates (x-axis to y-axis)
            coord_flip() +
            # Add a horizontal (in fact vertical line) to mark 0 = no difference between groups
            geom_hline(yintercept = 0) +
            # Label y-axis and x-axis
            ylab("Standardised difference") +
            xlab("Item") +
            # Divide the plot into three columns according to parameter type
            facet_grid(grouping ~ parameter_type)
  
p + theme_minimal()


names(std.diff_and_CIs.all_5)
std.diff_and_CIs.all_5[,1:4]

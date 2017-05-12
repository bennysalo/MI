
# 'Run bootstraps for configural model

# RNGkind("L'Ecuyer-CMRG")
# saved_seed <- .Random.seed

rm(list = ls())


load("~/Dropbox/to aws/MI after 2b - age.RData")
load("~/Dropbox/to aws/bootstrap functions.RData")

results_step2.age <- run_my_bootstraps(results_step2.age, n_samples = n)
save.image("~/Dropbox/to aws/MI after 2b - age.RData")
rm(list = ls())

load("~/Dropbox/to aws/MI after 2b - violence.RData")
load("~/Dropbox/to aws/bootstrap functions.RData")

results_step2.violence <- run_my_bootstraps(results_step2.violence, n_samples = n)
save.image("~/Dropbox/to aws/MI after 2b - violence.RData")
rm(list = ls())

load("~/Dropbox/to aws/MI after 2b - previous.RData")
load("~/Dropbox/to aws/bootstrap functions.RData")

results_step2.previous <- run_my_bootstraps(results_step2.previous, n_samples = n)
save.image("~/Dropbox/to aws/MI after 2b - previous.RData")
rm(list = ls())

load("~/Dropbox/to aws/MI after 2b - reoffence.RData")
load("~/Dropbox/to aws/bootstrap functions.RData")

results_step2.reoffender <- run_my_bootstraps(results_step2.reoffender, n_samples = n)
save.image("~/Dropbox/to aws/MI after 2b - reoffence.RData")
rm(list = ls())

load("~/Dropbox/to aws/MI after 2b - closed.RData")
load("~/Dropbox/to aws/bootstrap functions.RData")

results_step2.closed <- run_my_bootstraps(results_step2.closed, n_samples = n)
save.image("~/Dropbox/to aws/MI after 2b - closed.RData")
rm(list = ls())
  
  


# Grab the standardised differences from the original data
std.diff_and_CIs.age        <- create_sdiff_CI_df(results_step2.age[["configural fit"]],     
                                                  results_step2.age[["bootstrapped coefs"]])
std.diff_and_CIs.violence   <- create_sdiff_CI_df(results_step2.violence[["configural fit"]],
                                                  results_step2.violence[["bootstrapped coefs"]])
std.diff_and_CIs.previous   <- create_sdiff_CI_df(results_step2.previous[["configural fit"]],
                                                  results_step2.previous[["bootstrapped coefs"]])
std.diff_and_CIs.reoffence  <- create_sdiff_CI_df(results_step2.reoffender[["configural fit"]], 
                                                  results_step2.reoffender[["bootstrapped coefs"]])
std.diff_and_CIs.closed     <- create_sdiff_CI_df(results_step2.closed[["configural fit"]],
                                                  results_step2.closed[["bootstrapped coefs"]])

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
std.diff_and_CIs.all_5$item <- sub(pattern = " \\| .*$", replacement = "", 
                                   x = std.diff_and_CIs.all_5$item)
std.diff_and_CIs.all_5$item <- sub(pattern = " ", replacement = "", 
                                   x = std.diff_and_CIs.all_5$item)
std.diff_and_CIs.all_5$item <- sub(pattern = " .*$", replacement = "", 
                                   x = std.diff_and_CIs.all_5$item)

# Create a column indicating which parameter type is tested

std.diff_and_CIs.all_5$parameter_type[grepl(pattern = "=~",
                                            rownames(std.diff_and_CIs.all_5))] <- "Loading"
std.diff_and_CIs.all_5$parameter_type[grepl(pattern = "t1",
                                            rownames(std.diff_and_CIs.all_5))] <- "Threshold 1"
std.diff_and_CIs.all_5$parameter_type[grepl(pattern = "t2",
                                            rownames(std.diff_and_CIs.all_5))] <- "Threshold 2"


# Temporarily seperate loadings and thresholds

std.diff_table.loadings <- std.diff_and_CIs.all_5[grep(pattern = "=~", x = rownames(std.diff_and_CIs.all_5)), ]
std.diff_table.thresholds <- std.diff_and_CIs.all_5[grep(pattern = " \\| ", x = rownames(std.diff_and_CIs.all_5)), ]


# Create a column with factors in std.diff_table.loadings
  # Simply pull the factors from the row names (loading strings)
std.diff_table.loadings$factor <- rownames(std.diff_table.loadings)
std.diff_table.loadings$factor <- sub(pattern = " =~ .*$", replacement = "", x = std.diff_table.loadings$factor)

# Create a column with factors in std.diff_table.threshold
  # More involved. Thesholds of an item can pertain to two factors. We here create a table where the thresholds appear for
  # both factors.

  # Grab parameter table
  ptable <- lavaanify(results_step2.age$`base model`)
  # Subset the loadings
  loadings_table <- subset(ptable, op == "=~")
  # Grab items and factors
  items    <- unique(loadings_table$rhs)
  factors  <- unique(loadings_table$lhs)
  # Create a vector of the loadings
  loadings <- paste(loadings_table$lhs, loadings_table$op, loadings_table$rhs)

  # Use purrr:map to match each factor with all its items in a data.frame
  
  library(purrr)

  # First create the function that will be used in purrr:map_df
  match_factor_items <- function(factor) {
    # Grab the loadings for the defined factor
    factor_loadings <- loadings[grep(pattern = factor, x = loadings)]
    # Isolate the item form that loading string
    factor_items    <- sub(pattern = "^.* =~ ", replacement = "", x = factor_loadings)
    # Put facots and loadings in a data frame
    df              <- merge(factor, factor_items)
    colnames(df)    <- c("factor", "item")
    return(df)
}

  # Apply match_factor_items to all factors in the 'factors' vector. Result in a data.frame.
  factors_and_items <- map_df(.x = factors, .f = match_factor_items)
   
  # Finally, merge data.frames, adding a column to with factos to 'std.diff.threshold'
  # If there are two matching factors for an item in 'factor_and_items' the merge will result in two seperate rows
  # (but with the same values)
  # This table should have nrow = [number of loadings * 2 thresholds - 1 (that has only one threshold)] * 5 groupings
  # = (44 * 2 - 1 )* 5 = 435
  std.diff_table.thresholds <- merge(factors_and_items, std.diff_table.thresholds)
  nrow(std.diff_table.thresholds) == (44 * 2 - 1 )* 5   # TRUE

  
# Combine loadings and thresholds back.
rbind(std.diff_table.loadings, std.diff_table.thresholds)


# Find the loadings and thresholds furthest away from 0
head(std.diff_table.loadings[order(abs(std.diff_table.loadings$std_diff), decreasing = TRUE),  ], 10)
head(std.diff_table.thresholds[order(abs(std.diff_table.thresholds$std_diff), decreasing = TRUE),  ], 10)

#   The coordinates will be flipped

p <- ggplot(data = std.diff_and_CIs.all_5[std.diff_and_CIs.all_5$grouping == "Younger", ], 
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

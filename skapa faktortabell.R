pt <- lavaanify(Mod6facMI) # Ändra till single group

# Skapa tabell med items och factors som kan användas senare i figuren.
# items_and_factors

loadings_table <- subset(pt, op == "=~")
items    <- unique(loadings_table$rhs)
factors  <- unique(loadings_table$lhs)

loadings <- paste(loadings_table$lhs, loadings_table$op, loadings_table$rhs)

library(dplyr)                  
library(purrr)

items_and_factors <- data_frame(items)

get_grepl_x <- function(pattern, x) {
  x[grepl(pattern = pattern, x = x)]
}

items_and_factors$loadings <- map(.x = items, get_grepl_x, x = loadings)



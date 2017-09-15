rm(list = ls())

library(foreign, pos=4)
FinPrisonData <- read.spss("../Dropbox/to aws/data/recidivists and nonrecidivsts all cases.sav", 
                          use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)

# From local machine
FinPrisonData <- read.spss("C:/Users/benny_000/Dropbox/to aws/data/recidivists and nonrecidivsts all cases.sav",
use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)

# set NA on i_partnerRelation and i_parenting as 0 = no problem
FinPrisonData$i_partnerRelation <- ifelse(is.na(FinPrisonData$i_partnerRelation), 0, FinPrisonData$i_partnerRelation)
FinPrisonData$i_parenting       <- ifelse(is.na(FinPrisonData$i_parenting),       0, FinPrisonData$i_parenting)


# Create collapsed variables, used in first manuscript
FinPrisonData$ic_accomodation <- (FinPrisonData$i_housingShortTerm + FinPrisonData$i_housingAppropriate)

FinPrisonData$ic_drugUseAndEffects <- FinPrisonData$i_drugFrequency + FinPrisonData$i_drugIntravenous + 
  FinPrisonData$i_drugCriminalActs + FinPrisonData$i_drugViolence + 
  FinPrisonData$i_drugEffectWork + FinPrisonData$i_drugEffectHealth + FinPrisonData$i_drugEffectRelations

# Create combined item for i_drugIntravenous and i_drugEffectHealth (need shown in 1a - best drug model)

FinPrisonData$ic_drugHealthRisk <- FinPrisonData$i_drugIntravenous + FinPrisonData$i_drugEffectHealth



# Recode into scale 0,1,2 with the 0 as 0, up to a mean of 1 = 1, above = 2
require(car)
FinPrisonData$ic_accomodation <- Recode(FinPrisonData$ic_accomodation, 
                                        recodes = "0 = 0; c(1, 2) = 1; c(3, 4) = 2")

FinPrisonData$ic_drugUseAndEffects <- Recode(FinPrisonData$ic_drugUseAndEffects, 
                                             recodes = "0 = 0; 1:7 = 1; 8:hi = 2")

FinPrisonData$ic_drugHealthRisk <- Recode(FinPrisonData$ic_drugHealthRisk, 
                                        recodes = "0 = 0; c(1, 2) = 1; c(3, 4) = 2")

# Create variables for grouping in measurement invariance. 
# Changed from previous A, B coding to ordered variables with expicit 1, 2 labels.

  # Participants with entire sentence in closed prison versus all other

FinPrisonData$allClosed     <- as.ordered(
  ifelse(FinPrisonData$openPrison == "Closed prison" & 
           FinPrisonData$conditionalRelease == "No conditional release",
         yes = "2-Entire sentence in closed prison", no = "1-Open prison or conditional relase"))

  # Median split for 'ageAtRelase'

median(FinPrisonData$ageAtRelease)

FinPrisonData$ageMedSplit   <- as.ordered(
  ifelse(FinPrisonData$ageAtRelease > median(FinPrisonData$ageAtRelease), 
         yes = "2-older", no = "1-younger"))

  # Present term is a sentence for a "violent crime" (assault or homicide)

FinPrisonData$violentCrime  <- as.ordered(
  ifelse(FinPrisonData$o_assault == 1 | FinPrisonData$o_homicide == 1,
         yes = "2-violent crime", no = "1-no assault or homicide counts in present conviction"))

  # first prison term or one or more previous prison terms

FinPrisonData$prevReoffence <- as.ordered(
  ifelse(FinPrisonData$ps_prisonTerms > 1,
         yes = "2-one or more previous prison terms", no = "1-first prison term"))


# Also, code a variable to indicate granting of conditional release
# Rename original variable to avoid confusion

names(FinPrisonData)[names(FinPrisonData) == "conditionalRelease"] <- 'conditionalReleaseOutcome'


FinPrisonData$conditionalReleaseGranted <- as.ordered(
    ifelse(FinPrisonData$conditionalReleaseOutcome == "No conditional release",
           yes = "1-Not granted", no = "2-Granted"))


  

# Set pertinent variables to be ordered
names(FinPrisonData)
FinPrisonData[c(17:98, 101:108)] <- lapply(FinPrisonData[c(17:98, 101:108)], ordered)

summary(FinPrisonData[c(1,104:108)])
# Subset only males

FinPrisonMales <- subset(FinPrisonData, subset = gender == "male")


saveRDS(FinPrisonMales, "C:/Users/benny_000/Dropbox/AAAKTUELLT/MI/FinPrisonMales.rds")

### For now - don't do factor analysis on drug variables


# # Subset those who have at least some current drugproblems - for evaluating those items
# 
# usedDrugs <- subset(FinPrisonMales, subset = ic_drugUseAndEffects >  0)
# noDrugs   <- subset(FinPrisonMales, subset = ic_drugUseAndEffects ==  0)
# 
# 
# 
# # Create factor scores for drug factor
# # The use of this model is established in -1a - best drug model.R'
# drugMod2b       <- 'DrugL =~ i_drugFrequency + i_drugCriminalActs + i_drugViolence + 
#                             i_drugEffectWork + i_drugEffectRelations +  ic_drugHealthRisk'
# 
# drugFit2b <- cfa(drugMod2b, data = usedDrugs, std.lv = TRUE, estimator = "WLSMV")
# 
# # Calculate continous factor scores
# fs_drugUse <- predict(drugFit2b)
# 
# # Collapse into familiar 0, 1, 2 scoring. Indiividuals in noDrugs get 0 individuals in usedDrugs get
# # 1, or 2 based on median split of factor scores
# ifact_drugUse <- as.ordered(cut(fs_drugUse, breaks = 2, labels = c(1,2)))
# 
# # add new variables to usedDrugs
# 
# usedDrugs <- cbind(usedDrugs, ifact_drugUse)
# noDrugs   <- cbind(noDrugs, as.ordered(rep(0, times = nrow(noDrugs))))
# 
# names(noDrugs)[108] <- "ifact_drugUse"
# 
# FinPrisonMales2 <- rbind(noDrugs, usedDrugs)

# saveRDS(FinPrisonMales2, "C:/Users/benny_000/Dropbox/AAAKTUELLT/MI/FinPrisonMales2.rds")


# Clean up environment. Delete objects that are not needed anymore.
rm(ls())




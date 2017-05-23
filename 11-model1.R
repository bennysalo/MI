# Model with six factors based on first manuskript but with a new drug variable
# based on the factor score for the drug factor (and with )

Mod6facMI <-' 
economy    =~ i_financialManagement + i_financialObstacles + i_financialSituation + 
              i_dailyLifeManagement + ic_accomodation +  i_workApplication
alcohol    =~ i_alcFrequency + i_alcEffectWork + i_alcEffectRelations + i_alcEffectHealth +
              i_alcWithMeds + i_alcMotivationTreat + i_alcViolence
change     =~ i_motivationChange + i_attitudeStaff + i_attitudeSupervision + i_othersView + i_attitudeHostile +
              i_insight + i_manipulative + i_attitudeProcrime + i_socialSkills + i_alcMotivationTreat +
              i_instrumentalAggression + i_workAttitude
drugs      =~ i_peersCriminal + i_riskSeeking + i_drugHistory +ifact_drugUse +
              i_alcWithMeds + i _attitudeProcrime
aggression =~ i_impulsive + i_alcViolence + i_domViolPerp + i_instrumentalAggression + i_attitudeHostile +
              i_othersView + i_attitudeStaff
employment =~ i_workHistory + i_eduNeed + i_eduAttitude + i_workAttitude +
              i_remedialTeaching + i_workApplication'

# Check for multicollinerarity
  # extract items from model
  items <- unique(subset(lavaanify(Mod6facMI),op == "=~")$rhs)
  # subset data.frame
  data_Mod6facMI <- FinPrisonMales2[ , items]
  # calculate polychoric correlation matrix
  polycor_matrix <- lavCor(data_Mod6facMI)
  # Check for high polychoric squared multiple correlations
  smc(polycor_matrix)[order(smc(polycor_matrix))]
  KMO(polycor_matrix)
  
  # OK
  
  # Run model
  CFA6facMI <- cfa(Mod6facMI, data = FinPrisonMales2, std.lv = T, estimator = "WLSMV")
summary(CFA6facMI, fit = T)
head(modindices(CFA6facMI, sort. = TRUE))

# No changes from this model
# Clean up environment
rm(list = c("data_Mod6facMI", "items", "polycor_matrix"))
ls()

# Save results so far
save.image("~/Dropbox/to aws/MI workspace after 1a.RData")

# i_instrumentalAggression delted from change and i_remedialTeaching delted from employment
Mod6facMI2 <-' 
economy    =~ i_financialManagement + i_financialObstacles + i_financialSituation + 
              i_dailyLifeManagement + ic_accomodation +  i_workApplication
alcohol    =~ i_alcFrequency + i_alcEffectWork + i_alcEffectRelations + i_alcEffectHealth +
              i_alcWithMeds + i_alcMotivationTreat + i_alcViolence
change     =~ i_motivationChange + i_attitudeStaff + i_attitudeSupervision + i_othersView + i_attitudeHostile +
              i_insight + i_manipulative + i_attitudeProcrime + i_socialSkills + i_alcMotivationTreat + i_workAttitude
drugs      =~ i_peersCriminal + i_riskSeeking + i_drugHistory +ifact_drugUse +
              i_alcWithMeds + i _attitudeProcrime
aggression =~ i_impulsive + i_alcViolence + i_domViolPerp + i_instrumentalAggression + i_attitudeHostile +
              i_othersView + i_attitudeStaff
employment =~ i_workHistory + i_eduNeed + i_eduAttitude + i_workAttitude + i_workApplication'
           

CFA6facMI2 <- cfa(Mod6facMI2, data = FinPrisonMales2, std.lv = T, estimator = "WLSMV")
summary(CFA6facMI2, fit = T)
head(modindices(CFA6facMI2, sort. = TRUE))



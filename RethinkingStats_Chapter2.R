Identity_AProb <- .8
Identity_BProb <- .65
True_AProb <- (Identity_AProb*.5)/(Identity_AProb*.5 + (1-Identity_BProb)*.5)
print(True_AProb)
SpeciesA_Prob <- .1
SpeciesB_Prob <- .2
Prior_Prob <- True_AProb

Posterior_Prob <- 1 - SpeciesA_Prob*Prior_Prob/(SpeciesA_Prob*Prior_Prob+SpeciesB_Prob*(1-Prior_Prob))
print(Posterior_Prob)

Prob_Twins = Posterior_Prob*SpeciesB_Prob + (1-Posterior_Prob)*SpeciesA_Prob
#print(Prob_Twins)

print(SpeciesA_Prob*(1-SpeciesA_Prob)/(SpeciesA_Prob*(1-SpeciesA_Prob) + SpeciesB_Prob*(1-SpeciesB_Prob)))




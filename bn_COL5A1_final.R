# The COL5A1 BN
# A Bayesian Network for COL5A1 AOP

# *** Libraries
library(bnlearn)
library(graph)
library(Rgraphviz)
library(RBGL)
library(gRain)
library(ggplot2)
library(RColorBrewer)


# *** Create the Model String 
# *** This describes the Bayesian Network nodes and parent-child relationships among them
modelStr <- paste(
  "[COL5A1_Mutation]",
  "[TypeV_Collagen_Function|COL5A1_Mutation]",
  "[Fibrils_Strength|TypeV_Collagen_Function]",
  "[Skin_Strength|Fibrils_Strength]",
  "[Tissue_Strength|Fibrils_Strength]",
  "[AAD|Tissue_Strength]", #AAD = Arthritis-Adrenaline Disorder
  "[Tendon_Strength|Fibrils_Strength]",
  "[Immunity|Tendon_Strength]",
  "[Wound_Healing|Skin_Strength:Tendon_Strength]",
  "[Chronic_Pain|Wound_Healing:Immunity]",
  "[Vein_Plasticity|Fibrils_Strength]",
  "[Blood_Flow_to_Brain|Vein_Plasticity]",
  "[GI_Efficiency|Vein_Plasticity]",
  "[POTS|Vein_Plasticity]",
  "[Brain_Fog|Blood_Flow_to_Brain:POTS]",
  "[Pharynx_Structure|Vein_Plasticity]",
  "[Sleep_Apnea|Pharynx_Structure]",
  "[Chronic_Fatigue|AAD:Sleep_Apnea]",
  sep=""
)

# *** Create the directed acyclic graph (DAG) from the string
dag = model2network(modelStr)
dag # Details


# *****************************************************************
# Plot the DAG
graphviz.plot(dag)

#highlight nodes we have data or probabilities for

graphviz.plot(dag, 
              highlight = list(nodes = c("COL5A1_Mutation","AAD", 
                                         "Immunity", "Wound_Healing","Chronic_Pain",
                                         "GI_Efficiency", "POTS", "Brain_Fog",
                                         "Sleep_Apnea", "Chronic_Fatigue"),
                               
              col = "black", fill = "#d6ffeb"))
# *****************************************************************

#Write the values for Mark's observed node variables at time points 1, 2, and 3

#Mark1
probsMark1 = 
  matrix(data = c(
    0.0, 0.0, 1.0,  # COL5A1_Mutation ("None", "Heterozygous", "Homozygous")
    0.3, 0.7, 0.0,  # AAD ("None", "Mild", "Severe")
    0.0, 1.0, 0.0,  # Immunity ("Weak", "Ok", "Strong")
    0.9, 0.1, 0.0,  # Wound_Healing ("Weak", "Ok", "Strong")
    0.7, 0.3, 0.0,  # Chronic_Pain ("None", "Mild", "Severe")
    0.0, 1.0, 0.0,  # GI_Efficiency ("Weak", "Ok", "Strong")
    0.2, 0.8, 0.0,  # POTS ("None", "Mild", "Severe")
    0.0, 0.4, 0.6,  # Brain_Fog ("None", "Mild", "Severe")
    0.0, 0.5, 0.5,  # Sleep_Apnea ("None", "Mild", "Severe")
    0.0, 0.2, 0.8), # Chronic_Fatigue ("None", "Mild", "Severe")
    nrow = 10, ncol = 3, byrow = TRUE,
    dimnames = list(c("COL5A1_Mutation","AAD", "Immunity", "Wound_Healing",
                      "Chronic_Pain", "GI_Efficiency", "POTS", "Brain_Fog",
                      "Sleep_Apnea", "Chronic_Fatigue"),c("1","2","3"))
  )

# Convert the chosen input probabilities to a data frame
probsMark1 <- as.list(data.frame(t(probsMark1)))

#Mark2
probsMark2 = 
  matrix(data = c(
    0.0, 0.0, 1.0,  # COL5A1_Mutation ("None", "Heterozygous", "Homozygous")
    0.3, 0.7, 0.0,  # AAD ("None", "Mild", "Severe")
    0.0, 0.0, 1.0,  # Immunity ("Weak", "Ok", "Strong")
    0.9, 0.1, 0.0,  # Wound_Healing ("Weak", "Ok", "Strong")
    0.7, 0.3, 0.0,  # Chronic_Pain ("None", "Mild", "Severe")
    0.0, 1.0, 0.0,  # GI_Efficiency ("Weak", "Ok", "Strong")
    0.2, 0.8, 0.0,  # POTS ("None", "Mild", "Severe")
    0.0, 0.8, 0.2,  # Brain_Fog ("None", "Mild", "Severe")
    0.0, 1.0, 0.0,  # Sleep_Apnea ("None", "Mild", "Severe")
    0.0, 0.6, 0.4), # Chronic_Fatigue ("None", "Mild", "Severe")
    nrow = 10, ncol = 3, byrow = TRUE,
    dimnames = list(c("COL5A1_Mutation","AAD", "Immunity", "Wound_Healing",
                      "Chronic_Pain", "GI_Efficiency", "POTS", "Brain_Fog",
                      "Sleep_Apnea", "Chronic_Fatigue"),c("1","2","3"))
  )

# Convert the chosen input probabilities to a data frame
probsMark2 <- as.list(data.frame(t(probsMark2)))


#Mark3
probsMark3 = 
  matrix(data = c(
    0.0, 0.0, 1.0,  # COL5A1_Mutation ("None", "Heterozygous", "Homozygous")
    0.4, 0.1, 0.5,  # AAD ("None", "Mild", "Severe")
    0.6, 0.4, 0.0,  # Immunity ("Weak", "Ok", "Strong")
    1.0, 0.0, 0.0,  # Wound_Healing ("Weak", "Ok", "Strong")
    0.0, 0.8, 0.2,  # Chronic_Pain ("None", "Mild", "Severe")
    0.0, 0.0, 1.0,  # GI_Efficiency ("Weak", "Ok", "Strong")
    0.2, 0.8, 0.0,  # POTS ("None", "Mild", "Severe")
    0.0, 0.9, 0.1,  # Brain_Fog ("None", "Mild", "Severe")
    0.5, 0.5, 0.0,  # Sleep_Apnea ("None", "Mild", "Severe")
    0.0, 0.8, 0.2), # Chronic_Fatigue ("None", "Mild", "Severe")
    nrow = 10, ncol = 3, byrow = TRUE,
    dimnames = list(c("COL5A1_Mutation","AAD", "Immunity", "Wound_Healing",
                      "Chronic_Pain", "GI_Efficiency", "POTS", "Brain_Fog",
                      "Sleep_Apnea", "Chronic_Fatigue"),c("1","2","3"))
  )


# Convert the chosen input probabilities to a data frame
probsMark3 <- as.list(data.frame(t(probsMark3)))



#set the vals for all nodes
COL5A1_Mutation.vals <- c("None", "Heterozygous", "Homozygous")
AAD.vals <- c("None", "Mild", "Severe")
Immunity.vals <- c("Weak", "Ok", "Strong")
Wound_Healing.vals <- c("Weak", "Ok", "Strong")
Chronic_Pain.vals <- c("None", "Mild", "Severe")
GI_Efficiency.vals <- c("Weak", "Ok", "Strong")
POTS.vals <- c("None", "Mild", "Severe")
Brain_Fog.vals <- c("None", "Mild", "Severe")
Sleep_Apnea.vals <- c("None", "Mild", "Severe")
Chronic_Fatigue.vals <- c("None", "Mild", "Severe")
TypeV_Collagen_Function.vals <- c("Weak", "Okay", "Strong")
Fibrils_Strength.vals <- c("Weak", "Okay", "Strong")
Skin_Strength.vals <- c("Weak", "Okay", "Strong")
Tendon_Strength.vals <- c("Weak", "Okay", "Strong")
Tissue_Strength.vals <- c("Weak", "Okay", "Strong")
Vein_Plasticity.vals <- c("Hyperplastic", "Normal", "Hypoplastic")
Blood_Flow_to_Brain.vals <- c("Weak", "Okay", "Strong")
Pharynx_Structure.vals <- c("Weak", "Okay", "Strong")




runTree <- function(probs){

  # Set the CPTs for each observed node (are these redundant with above?)
  
  
  COL5A1_Mutation.cpt <-
    array(probs$COL5A1_Mutation,
          dim=3, #3 = none, hetero, homo
          dimnames = list(COL5A1_Mutation=COL5A1_Mutation.vals))
  
  
  
  AAD.cpt <- array(probs$AAD, dim = c(3, 3), #3 by 3 because AAD and Tissue_Strength each have weak, ok, strong
                   dimnames = list(AAD = AAD.vals,
                                   Tissue_Strength = Tissue_Strength.vals))
  
  
  Immunity.cpt <- array(probs$Immunity, dim = c(3, 3),
                        dimnames = list(Immunity = Immunity.vals,
                                        Tendon_Strength = Tendon_Strength.vals))
  
  
  Wound_Healing.cpt <- array(probs$Wound_Healing, dim = c(3, 3, 3),
                             dimnames = list(Wound_Healing = Wound_Healing.vals,
                                             Skin_Strength = Skin_Strength.vals,
                                             Tendon_Strength = Tendon_Strength.vals))
  
  
  Chronic_Pain.cpt <- array(probs$Chronic_Pain, dim = c(3, 3, 3),
                            dimnames = list(Chronic_Pain = Chronic_Pain.vals,
                                            Wound_Healing = Wound_Healing.vals,
                                            Immunity = Immunity.vals))
  
  
  GI_Efficiency.cpt <- array(probs$GI_Efficiency, dim = c(3, 3),
                             dimnames = list(GI_Efficiency = GI_Efficiency.vals,
                                             Vein_Plasticity = Vein_Plasticity.vals))
  
  
  POTS.cpt <- array(probs$POTS, dim = c(3, 3),
                    dimnames = list(POTS = POTS.vals,
                                    Vein_Plasticity = Vein_Plasticity.vals))
  
  
  Brain_Fog.cpt <- array(probs$Brain_Fog, dim = c(3, 3, 3),
                         dimnames = list(Brain_Fog = Brain_Fog.vals,
                                         Blood_Flow_to_Brain = Blood_Flow_to_Brain.vals,
                                         POTS = POTS.vals))
  
  
  Sleep_Apnea.cpt <- array(probs$Sleep_Apnea, dim = c(3, 3),
                           dimnames = list(Sleep_Apnea = Sleep_Apnea.vals,
                                           Pharynx_Structure = Pharynx_Structure.vals))
  
  
  Chronic_Fatigue.cpt <- array(probs$Chronic_Fatigue, dim = c(3, 3, 3),
                               dimnames = list(Chronic_Fatigue = Chronic_Fatigue.vals,
                                               AAD = AAD.vals,
                                               Sleep_Apnea = Sleep_Apnea.vals))
  
  
  
  
  #Set the CPTs for the unobserved nodes using distributions we deem likely as the edges
  
  
  TypeV_Collagen_Function.cpt <-
      array(c(0.1, 0.1, 0.8,  0.4, 0.3, 0.3,  0.8, 0.2, 0.0), #these sets are based on collagen function being weak okay strong based on COL5A1
            #0.1 chance that collagen function will be weak based on no COL5A1 mutation
            #0.4 chance that collagen function will be weak based on hetero COL5A1 mutation
            #0.8 chance that collagen function will be weak based on homo COL5A1 mutation
      dim=c(3, 3),
      dimnames = list(TypeV_Collagen_Function=TypeV_Collagen_Function.vals,
                      COL5A1_Mutation=COL5A1_Mutation.vals))
  
  
  Fibrils_Strength.cpt <-
      array(c(1.0, 0.0, 0.0,  0.2, 0.6, 0.2,  0.1, 0.2, 0.7),
            #1.0 chance that fibrils will be weak based on weak collagen
            #0.2 chance that fibrils will be weak based on okay collagen
            #0.1 chance that fibrils will be weak based on strong collagen
      dim=c(3, 3),
      dimnames = list(Fibrils_Strength=Fibrils_Strength.vals, TypeV_Collagen_Function=TypeV_Collagen_Function.vals))
  
  
  
  Skin_Strength.cpt <-
      array(c(1.0, 0.0, 0.0,  0.2, 0.6, 0.2,  0.1, 0.2, 0.7),
      dim=c(3, 3),
      dimnames = list(Skin_Strength=Skin_Strength.vals, Fibrils_Strength=Fibrils_Strength.vals))
  
  
  
  Tendon_Strength.cpt <-
      array(c(1.0, 0.0, 0.0,  0.2, 0.6, 0.2,  0.1, 0.2, 0.7),
      dim=c(3, 3),
      dimnames = list(Tendon_Strength=Tendon_Strength.vals, Fibrils_Strength=Fibrils_Strength.vals))
  
  
  Tissue_Strength.cpt <-
      array(c(1.0, 0.0, 0.0,  0.2, 0.6, 0.2,  0.1, 0.2, 0.7),
      dim=c(3, 3),
      dimnames = list(Tissue_Strength=Tissue_Strength.vals, Fibrils_Strength=Fibrils_Strength.vals))
  
  
  Vein_Plasticity.cpt <-
      array(c(1.0, 0.0, 0.0,  0.2, 0.6, 0.2,  0.1, 0.2, 0.7),
      dim=c(3, 3),
      dimnames = list(Vein_Plasticity=Vein_Plasticity.vals, Fibrils_Strength=Fibrils_Strength.vals))
  
  
  Blood_Flow_to_Brain.cpt <-
      array(c(0.6, 0.2, 0.2,  0.4, 0.5, 0.1,  0.3, 0.3, 0.4),
      dim=c(3, 3),
      dimnames = list(Blood_Flow_to_Brain=Blood_Flow_to_Brain.vals, Vein_Plasticity=Vein_Plasticity.vals))
  
  
  Pharynx_Structure.cpt <-
      array(c(0.6, 0.2, 0.2,  0.4, 0.5, 0.1,  0.3, 0.3, 0.4),
      dim=c(3, 3),
      dimnames = list(Pharynx_Structure=Pharynx_Structure.vals, Vein_Plasticity=Vein_Plasticity.vals))
  
  
  
  
  # Combine the CPTs into a list
  cpt_list <- list(
    "COL5A1_Mutation" = COL5A1_Mutation.cpt,
    "AAD" = AAD.cpt,
    "Immunity" = Immunity.cpt,
    "Wound_Healing" = Wound_Healing.cpt,
    "Chronic_Pain" = Chronic_Pain.cpt,
    "GI_Efficiency" = GI_Efficiency.cpt,
    "POTS" = POTS.cpt,
    "Brain_Fog" = Brain_Fog.cpt,
    "Sleep_Apnea" = Sleep_Apnea.cpt,
    "Chronic_Fatigue" = Chronic_Fatigue.cpt,
    "TypeV_Collagen_Function" = TypeV_Collagen_Function.cpt,
    "Fibrils_Strength" = Fibrils_Strength.cpt,
    "Skin_Strength" = Skin_Strength.cpt,
    "Tissue_Strength" = Tissue_Strength.cpt,
    "Tendon_Strength" = Tendon_Strength.cpt,
    "Vein_Plasticity" = Vein_Plasticity.cpt,
    "Blood_Flow_to_Brain" = Blood_Flow_to_Brain.cpt,
    "Pharynx_Structure" = Pharynx_Structure.cpt
  )
  
  
  # *** "Fit" the dag and cpt together into a complete network model
  bn = custom.fit(dag, cpt_list)
  
  # *** Plot Nodes and Chart Probabilities to produce the probability of brain fog or CFS for each patient
  graphviz.chart(bn, type = "barprob", grid = TRUE, bar.col = "darkgreen", strip.bg = "lightskyblue")
  
  
  return(bn)

}

#Perform lots of fun inference with the resutls!


# Perform inference
inference_CF1 <- cpquery(runTree(probsMark1),
                     event = (Chronic_Fatigue == "Severe"),
                     evidence = (COL5A1_Mutation == "Homozygous"))

print(inference_CF1)


inference_BF1 <- cpquery(runTree(probsMark1),
                        event = (Brain_Fog == "Severe"),
                        evidence = (COL5A1_Mutation == "Homozygous"))

print(inference_BF1)

inference_CF2 <- cpquery(runTree(probsMark2),
                         event = (Chronic_Fatigue == "Severe"),
                         evidence = (COL5A1_Mutation == "Homozygous"))

inference_BF2 <- cpquery(runTree(probsMark2),
                        event = (Brain_Fog == "Severe"),
                        evidence = (COL5A1_Mutation == "Homozygous"))

inference_CF3 <- cpquery(runTree(probsMark3),
                         event = (Chronic_Fatigue == "Severe"),
                         evidence = (COL5A1_Mutation == "Homozygous"))

inference_BF3 <- cpquery(runTree(probsMark3),
                         event = (Brain_Fog == "Severe"),
                         evidence = (COL5A1_Mutation == "Homozygous"))

inference_CP1 <- cpquery(runTree(probsMark1),
                         event = (Chronic_Pain == "Severe"),
                         evidence = (COL5A1_Mutation == "Homozygous"))

inference_CP2 <- cpquery(runTree(probsMark2),
                         event = (Chronic_Pain == "Severe"),
                         evidence = (COL5A1_Mutation == "Homozygous"))

inference_CP3 <- cpquery(runTree(probsMark3),
                         event = (Chronic_Pain == "Severe"),
                         evidence = (COL5A1_Mutation == "Homozygous"))




#It works! So now I just need to run through each of Mark1, Mark2, and Mark3
# which represent three different time points, and then I need to graph the
# data which I care about. Then I can do some statistical analsyis to see which
# unobserved variables and which combinations of variables are most important
# to determining brain_fog and chronic_fatigue!



#Let's decrease Immunity and see how that affects chronic_fatigue and brain_fog over time
#Column 1 is immunity good (standard) and col 2 is immunity bad, rows are Mark1,2,3

immunity_CF1 <- cpquery(runTree(probsMark1),
                        event = (Chronic_Fatigue == "Severe"),
                        evidence = (Immunity == "Weak"))


immunity_BF1 <- cpquery(runTree(probsMark1),
                        event = (Brain_Fog == "Severe"),
                        evidence = (Immunity == "Weak"))

immunity_CP1 <- cpquery(runTree(probsMark1),
                        event = (Chronic_Pain == "Severe"),
                        evidence = (Immunity == "Weak"))


immunity_CF2 <- cpquery(runTree(probsMark2),
                        event = (Chronic_Fatigue == "Severe"),
                        evidence = (Immunity == "Weak"))

immunity_BF2 <- cpquery(runTree(probsMark2),
                        event = (Brain_Fog == "Severe"),
                        evidence = (Immunity == "Weak"))


immunity_CP2 <- cpquery(runTree(probsMark2),
                        event = (Chronic_Pain == "Severe"),
                        evidence = (Immunity == "Weak"))


immunity_CF3 <- cpquery(runTree(probsMark3),
                        event = (Chronic_Fatigue == "Severe"),
                        evidence = (Immunity == "Weak"))

immunity_BF3 <- cpquery(runTree(probsMark3),
                        event = (Brain_Fog == "Severe"),
                        evidence = (Immunity == "Weak"))

immunity_CP3 <- cpquery(runTree(probsMark3),
                        event = (Chronic_Pain == "Severe"),
                        evidence = (Immunity == "Weak"))



Chronic_Fatigue_Varied_by_Immunity <- data.frame("Strong Immunity" = c(inference_CF1, inference_CF2, inference_CF3), 
                             "Weak Immunity" = c(immunity_CF1, immunity_CF2, immunity_CF3))

Brain_Fog_Varied_by_Immunity <- data.frame("Strong Immunity" = c(inference_BF1, inference_BF2, inference_BF3), 
                                                 "Weak Immunity" = c(immunity_BF1, immunity_BF2, immunity_BF3))

Chronic_Pain_Varied_by_Immunity <- data.frame("Strong Immunity" = c(inference_CP1, inference_CP2, inference_CP3), 
                                           "Weak Immunity" = c(immunity_CP1, immunity_CP2, immunity_CP3))


Chronic_Fatigue_Varied_by_Immunity

Brain_Fog_Varied_by_Immunity

Chronic_Pain_Varied_by_Immunity

#RESULT ANALYSIS: Weak Immunity has no affect on Mark1 or Mark 2 for anything

#Weak Immunity makes Mark3 Chronic_Fatigue, Brain_Fog, and Chronic_Pain better




#Let's improve GI_efficiency and see how that affects chronic_fatigue and brain_fog over time

GI_CF1 <- cpquery(runTree(probsMark1),
                        event = (Chronic_Fatigue == "Severe"),
                        evidence = (GI_Efficiency == "Strong"))


GI_BF1 <- cpquery(runTree(probsMark1),
                        event = (Brain_Fog == "Severe"),
                        evidence = (GI_Efficiency == "Strong"))

GI_CP1 <- cpquery(runTree(probsMark1),
                        event = (Chronic_Pain == "Severe"),
                        evidence = (GI_Efficiency == "Strong"))


GI_CF2 <- cpquery(runTree(probsMark2),
                        event = (Chronic_Fatigue == "Severe"),
                        evidence = (GI_Efficiency == "Strong"))

GI_BF2 <- cpquery(runTree(probsMark2),
                        event = (Brain_Fog == "Severe"),
                        evidence = (GI_Efficiency == "Strong"))


GI_CP2 <- cpquery(runTree(probsMark2),
                        event = (Chronic_Pain == "Severe"),
                        evidence = (GI_Efficiency == "Strong"))


GI_CF3 <- cpquery(runTree(probsMark3),
                        event = (Chronic_Fatigue == "Severe"),
                        evidence = (GI_Efficiency == "Strong"))

GI_BF3 <- cpquery(runTree(probsMark3),
                        event = (Brain_Fog == "Severe"),
                        evidence = (GI_Efficiency == "Strong"))

GI_CP3 <- cpquery(runTree(probsMark3),
                        event = (Chronic_Pain == "Severe"),
                        evidence = (GI_Efficiency == "Strong"))



Chronic_Fatigue_Varied_by_GI <- data.frame("Ok GI" = c(inference_CF1, inference_CF2, inference_CF3), 
                                                 "Strong GI" = c(GI_CF1, GI_CF2, GI_CF3))

Brain_Fog_Varied_by_GI <- data.frame("Ok GI" = c(inference_BF1, inference_BF2, inference_BF3), 
                                           "Strong GI" = c(GI_BF1, GI_BF2, GI_BF3))

Chronic_Pain_Varied_by_GI <- data.frame("Ok GI" = c(inference_CP1, inference_CP2, inference_CP3), 
                                              "Strong GI" = c(GI_CP1, GI_CP2, GI_CP3))


Chronic_Fatigue_Varied_by_GI

Brain_Fog_Varied_by_GI

Chronic_Pain_Varied_by_GI

#RESULT ANALYSIS: Strong GI has no affect on Mark1 or Mark 2 for anything

#Strong GI almost imperceptibly makes Mark3 Chronic_Fatigue worse

#Strong GI almost imperceptibly makes Mark3 Brain_Fog and Chronic_Pain better




#Let's improve blood_flow_to_brain and see how that affects chronic_fatigue and brain_fog over time


BB_CF1 <- cpquery(runTree(probsMark1),
                        event = (Chronic_Fatigue == "Severe"),
                  evidence = (Blood_Flow_to_Brain == "Strong"))


BB_BF1 <- cpquery(runTree(probsMark1),
                        event = (Brain_Fog == "Severe"),
                  evidence = (Blood_Flow_to_Brain == "Strong"))

BB_CP1 <- cpquery(runTree(probsMark1),
                        event = (Chronic_Pain == "Severe"),
                  evidence = (Blood_Flow_to_Brain == "Strong"))


BB_CF2 <- cpquery(runTree(probsMark2),
                        event = (Chronic_Fatigue == "Severe"),
                  evidence = (Blood_Flow_to_Brain == "Strong"))

BB_BF2 <- cpquery(runTree(probsMark2),
                        event = (Brain_Fog == "Severe"),
                  evidence = (Blood_Flow_to_Brain == "Strong"))


BB_CP2 <- cpquery(runTree(probsMark2),
                        event = (Chronic_Pain == "Severe"),
                  evidence = (Blood_Flow_to_Brain == "Strong"))


BB_CF3 <- cpquery(runTree(probsMark3),
                        event = (Chronic_Fatigue == "Severe"),
                  evidence = (Blood_Flow_to_Brain == "Strong"))

BB_BF3 <- cpquery(runTree(probsMark3),
                        event = (Brain_Fog == "Severe"),
                  evidence = (Blood_Flow_to_Brain == "Strong"))

BB_CP3 <- cpquery(runTree(probsMark3),
                        event = (Chronic_Pain == "Severe"),
                        evidence = (Blood_Flow_to_Brain == "Strong"))



Chronic_Fatigue_Varied_by_Blood_Flow_to_Brain <- data.frame("Weak Blood_Flow_to_Brain" = c(inference_CF1, inference_CF2, inference_CF3), 
                                                 "Strong Blood_Flow_to_Brain" = c(BB_CF1, BB_CF2, BB_CF3))

Brain_Fog_Varied_by_Blood_Flow_to_Brain <- data.frame("Weak Blood_Flow_to_Brain" = c(inference_BF1, inference_BF2, inference_BF3), 
                                           "Strong Blood_Flow_to_Brain" = c(BB_BF1, BB_BF2, BB_BF3))

Chronic_Pain_Varied_by_Blood_Flow_to_Brain <- data.frame("Weak Blood_Flow_to_Brain" = c(inference_CP1, inference_CP2, inference_CP3), 
                                              "Strong Blood_Flow_to_Brain" = c(BB_CP1, BB_CP2, BB_CP3))


Chronic_Fatigue_Varied_by_Blood_Flow_to_Brain

Brain_Fog_Varied_by_Blood_Flow_to_Brain

Chronic_Pain_Varied_by_Blood_Flow_to_Brain

#RESULT ANALYSIS: Strong blood flow to brain ever so slightly increases the 
#  likelihood of Mark's Chronic_Fatigue over time, which I was not expecting

#Strong blood flow to brain ever so slightly decreases Mark's brain fog over time as expected

# Strong blood flow to the brain has no affect on chronic pain for Mark1 and Mark2, 
#   but does decrease likelihood of Chronic Pain for Mark 3




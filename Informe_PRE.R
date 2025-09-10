library(readxl)
library(mokken)
library(summarytools)
library(EFAtools)
library(lavaan)
library(lavaanPlot)
library(mirt)
library(psych)
ICC_PRE <- read_excel("ICC_PRE.xlsx",na = "999")
ICC_MID <- read_excel("ICC_MID.xlsx", na = "999")
ICC_END <- read_excel("ICC_END.xlsx", na = "999")
# Recodificación de los items
#PRE
I_PRE <- recode(
  as.data.frame(ICC_PRE[, paste0("ICC", 1:15)]),
  items  = c(1, 6, 8, 9, 13),  # ítems inversos
  values = 1:5
)
ICC_PRE[, paste0("ICC", 1:15)] <- I_PRE
# MID
I_MID <- recode(
  as.data.frame(ICC_MID[, paste0("ICC", 1:15)]),
  items  = c(1, 6, 8, 9, 13),
  values = 1:5
)
ICC_MID[, paste0("ICC", 1:15)] <- I_MID
# END
I_END <- recode(
  as.data.frame(ICC_END[, paste0("ICC", 1:15)]),
  items  = c(1, 6, 8, 9, 13),
  values = 1:5
)
ICC_END[, paste0("ICC", 1:15)] <- I_END
# Resumen base de datos
dfSummary(ICC_PRE)
dfSummary(ICC_MID)
dfSummary(ICC_END)
# Analisis en paralelo
N_FACTORS(I_PRE)
M_T<- "
Determinantes =~ ICC1 + ICC8 + ICC9 + ICC13 + ICC15
Valoracion   =~ ICC2 + ICC3 + ICC11 + ICC12 + ICC14
Practicas    =~ ICC4 + ICC5 + ICC6 + ICC7 + ICC10
Determinantes ~~ Valoracion
Determinantes ~~ Practicas
Valoracion    ~~ Practicas
"
MT<-cfa(M_T, data = I_PRE, estimator = "MLR") 
summary(MT, fit.measures = TRUE, standardized = TRUE)
fitMeasures(MT)
lavaanPlot(MT,stand = TRUE,stars = "regression",covs = TRUE,coefs = TRUE)
ME<-fa(I_PRE, nfactor=3, cor="poly", fm="minres", rotate = "oblimin")
fa.diagram(ME)
# Se eliminan los items 1,6,13, 14 y 15
I_PRE_2 <- I_PRE[, !(names(I_PRE) %in% c("ICC1","ICC6", "ICC13", "ICC14", "ICC15"))]
N_FACTORS(I_PRE_2)
EFA(I_PRE_2,2,method= "ULS",rotation = "oblimin")
irt.fa(I_PRE_2,nfactors = 2, fm="minres",rotate = "oblimin")

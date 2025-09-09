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
EFA(I_PRE,3,method= "ULS",rotation = "oblimin")
reliability(I_PRE,nfactors = 3)
omega(I_PRE,nfactors = 3)

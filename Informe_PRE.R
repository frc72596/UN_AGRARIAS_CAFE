install.packages("sjPlot")
#Paquetes
library(readxl)
library(mokken)
library(summarytools)
library(EFAtools)
library(lavaan)
library(lavaanPlot)
library(mirt)
library(sjPlot)
library(psych)
library(dplyr)
library(flextable)
library(officer)
#Bases de datos en el repositorio 
ICC_DEV <- read_excel("ICC_DEV.xlsx", sheet = "Base_Items_Completa")#Base de datos de desarrollo
ICC_ITEM_DEV <- read_excel("ICC_DEV.xlsx", sheet = "Items_Completa", na = "999")
ICC_PRE <- read_excel("ICC_PRE.xlsx",na = "999") 
ICC_MID <- read_excel("ICC_MID.xlsx", na = "999")
ICC_END <- read_excel("ICC_END.xlsx", na = "999")
# Descritptivos
niveles <- sort(unique(c(ICC_DEV$Estrato, ICC_PRE$ESTRATO)))
ICC_DEV <- ICC_DEV %>%
  mutate(Estrato = factor(Estrato, levels = niveles))

ICC_PRE <- ICC_PRE %>%
  mutate(ESTRATO = factor(ESTRATO, levels = niveles))
edad_dev <- paste0(round(mean(ICC_DEV$Edad_Ac, na.rm = TRUE),2),
                   " (", round(sd(ICC_DEV$Edad_Ac, na.rm = TRUE),2),")")

edad_pre <- paste0(round(mean(ICC_PRE$EDAD, na.rm = TRUE),2),
                   " (", round(sd(ICC_PRE$EDAD, na.rm = TRUE),2),")")
muj_dev <- sum(ICC_DEV$Genero_1_F_2_M == 1, na.rm=TRUE)
hom_dev <- sum(ICC_DEV$Genero_1_F_2_M == 2, na.rm=TRUE)
otr_dev <- sum(!ICC_DEV$Genero_1_F_2_M %in% c(1,2), na.rm=TRUE)

muj_pre <- sum(ICC_PRE$GEN == 1, na.rm=TRUE)
hom_pre <- sum(ICC_PRE$GEN == 2, na.rm=TRUE)
otr_pre <- sum(!ICC_PRE$GEN %in% c(1,2), na.rm=TRUE)
estr_dev <- table(ICC_DEV$Estrato)
estr_pre <- table(ICC_PRE$ESTRATO)
tabla <- tibble(
  Variable = c("Edad media (DE)",
               "Mujeres (n)",
               "Hombres (n)",
               "Otros géneros (n)",
               paste0("Estrato ", niveles)),
  
  Desarrollo = c(edad_dev,
                 muj_dev,
                 hom_dev,
                 otr_dev,
                 as.numeric(estr_dev)),
  
  Validacion = c(edad_pre,
                 muj_pre,
                 hom_pre,
                 otr_pre,
                 as.numeric(estr_pre))
)
ft <- flextable(tabla) %>%
  theme_booktabs() %>%
  autofit()
doc <- read_docx() %>%
  body_add_par("Tabla 1. Características sociodemográficas de las muestras", style = "heading 1") %>%
  body_add_flextable(ft)
print(doc, target = "tabla_muestras_ICC.docx")
# Analisis de construcción
tabla_items <- tab_itemscale(ICC_ITEM_DEV)
df_items <- tabla_items$df.list[[1]]
df_items$codigo <- rownames(df_items)
df_items <- df_items %>%
  rename(
    promedio = Mean,
    desviacion = SD,
    dificultad = `Item Difficulty`,
    discriminacion = `Item Discrimination`
  ) %>%
  mutate(
    promedio = as.numeric(promedio),
    desviacion = as.numeric(desviacion)
  )
df_items <- df_items %>%
  left_join(dic_items, by = "codigo")
df_items <- df_items %>%
  select(
    codigo,
    item,
    promedio,
    desviacion,
    dificultad,
    discriminacion
  ) %>%
  
  mutate(
    decision = case_when(
      discriminacion >= 0.20 & dificultad >= 0.20 & dificultad <= 0.80 ~ "Mantener",
      discriminacion >= 0.20 ~ "Revisar",
      TRUE ~ "Eliminar"
    )
  ) %>%
  
  # redondear
  mutate(
    across(c(promedio, desviacion, dificultad, discriminacion),
           ~round(.x,3))
  )

ft_items <- flextable(df_items) |>
  theme_booktabs() |>
  autofit()

ft_items
doc1 <- read_docx() |>
  body_add_par("Tabla. Análisis de ítems de la escala ICC", style = "heading 1") |>
  body_add_flextable(ft_items)
print(doc1, target = "tabla_items_ICC.docx")

# Recodificación de los items
#PRE
I_PRE <- recode(
  as.data.frame(ICC_PRE[, paste0("ICC", 1:15)]),
  items  = c(1, 6, 8, 9, 13),  # ítems inversos
  values = 1:5
)
ICC_PRE[, paste0("ICC", 1:15)] <- I_PRE
# Analisis exploratorio
fa.parallel(I_PRE,cor="poly")
fa5 <- fa(I_PRE, nfactors = 5, rotate = "oblimin", fm = "minres", cor = "poly")
fa4 <- fa(I_PRE, nfactors = 4, rotate = "oblimin", fm = "minres", cor = "poly")
fa3 <- fa(I_PRE, nfactors = 3, rotate = "oblimin", fm = "minres", cor = "poly")
fa.diagram(fa5)
fa.diagram(fa4)
fa.diagram(fa3)


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
dfSummary(ICC_DEV)
dfSummary(ICC_PRE)
dfSummary(ICC_MID)
dfSummary(ICC_END)
# Analisis en paralelo
N_FACTORS(I_PRE)
fa.parallel(I_PRE)
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
ME
fa.diagram(ME)
I_PRE_2<- I_PRE[, !(names(I_PRE) %in% c("ICC1", "ICC13", "ICC15"))]
N_FACTORS(I_PRE_2)
ME_2<- fa(I_PRE_2, nfactor = 2, cor = "poly", fm = "minres", rotate = "oblimin") #NO AJUSTO
ME_3<- fa(I_PRE_2, nfactor = 3, cor = "poly", fm = "minres", rotate = "oblimin")
ME_2
ME_3
fa.diagram(ME_2)
fa.diagram(ME_3)
ICC_ME_F1<- I_PRE_2[, c("ICC2","ICC3","ICC5","ICC14")]
ICC_ME_F2<- I_PRE_2[, c("ICC7","ICC10","ICC11","ICC12")]
ICC_ME_F3<- I_PRE_2[, c("ICC4","ICC6","ICC8", "ICC9")]
alpha(ICC_ME_F1)
alpha(ICC_ME_F2)
alpha(ICC_ME_F3)
I_PRE_3 <- I_PRE_2[, !(names(I_PRE_2) %in% "ICC4")]
N_FACTORS(I_PRE_3)
ME_4<- fa(I_PRE_3, nfactors = 2, cor = "poly", fm = "minres", rotate = "oblimin")
ME_5<- fa(I_PRE_3, nfactors = 3, cor = "poly", fm = "minres", rotate = "oblimin")
fa.diagram(ME_4)
fa.diagram(ME_5)           
ICC_ME2_F1<- I_PRE_2[, c("ICC2","ICC3","ICC5","ICC14")]
ICC_ME2_F2<- I_PRE_2[, c("ICC7","ICC10","ICC11","ICC12")]
ICC_ME2_F3<- I_PRE_2[, c("ICC6","ICC8", "ICC9")]
alpha(ICC_ME2_F1)
alpha(ICC_ME2_F2)
alpha(ICC_ME2_F3)
reliability(ICC_ME2_F3)

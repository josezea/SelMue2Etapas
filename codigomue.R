
rm(list = ls())

setwd("C:/Users/ASUS/Desktop/Nueva carpeta (2)")
# Dise???o parcial final

library(sampling)
library(dplyr)
library(stratification)

marco <- readRDS( "estudiantes.rds")
marco_bta <- subset(marco, NOMBREMUNICIPIO == "BOGOTÁ D.C.")

# Calcular el promedio del puntaje en matem???ticas
# Y: Puntaje de matem???ticos
# Estrato: Tama???o del cOlegio

tam_col <- marco_bta %>% group_by(CODIGO_ICFES) %>% summarise(N_h = n())


# Estratificaci???n del colegio
set.seed(291116)
library(stratification)
indica_LH <- strata.LH(tam_col$N_h, CV = 0.04, Ls = 5)
cortes <- c(min(tam_col$N_h), indica_LH$bh, max(tam_col$N_h)  )
tam_col$estrato_col <-  cut(tam_col$N_h, breaks = cortes, 
                            include.lowest = T, right = F, 
                            labels = c("Muy_Pequenos", "Pequenos",
                                       "Medianos", "Grandes", "Muy_Grandes" )) 
tam_col$estrato_col <- as.character(tam_col$estrato_col)
marco_bta <- merge(marco_bta, tam_col, by = "CODIGO_ICFES")
names(marco_bta)
dim(marco_bta)

marco_bta <- marco_bta[c("CODIGO_ICFES", "ID_estud", "AGSB_NOMBREINSTITUCION", "CODIGOMUNICIPIO", 
                         "NOMBREMUNICIPIO", "DEPARTAMENTO", 
                         "MATEMATICAS_PUNT", "estrato_col")]

length(unique(marco_bta$CODIGO_ICFES))


library(sampling)
bogota <- readRDS("marco_bta.rds")

# Selección de UPMS

c1 <- bogota %>% group_by(estrato_col, CODIGO_ICFES) %>% summarise(basura = n())
c1$basura <- NULL
c2 <- c1 %>% group_by(estrato_col) %>% summarise(NI_h = n())
c2$nI_h <- c(4, 6, 3, 8, 6 )

c1 <- arrange(c1, estrato_col)
set.seed(12345)
indicamueUPM <- sampling::strata(c1, stratanames = "estrato_col", size = c2$nI_h, 
                                 method="srswor",description = T)
mueUPM <- getdata(c1, indicamueUPM)
dim(mueUPM)

marcoUPM <- merge(mueUPM, bogota, by = "CODIGO_ICFES", all.x = T )
marcoUPM <- mueUPM %>% left_join(bogota, by =  "CODIGO_ICFES")
dim(marcoUPM)

# Selecciono estudiantes (20% en cada colegio)
c3 <- marcoUPM %>% group_by(CODIGO_ICFES) %>% summarise(N_i = n())
c3$n_i <- ceiling(c3$N_i * 0.2)
# which(c3$n_i < 2) # que colegio tengo menos de dos pelados en la muestra

marcoUPM <- arrange(marcoUPM, CODIGO_ICFES)
set.seed(12345)
indicamueUSM <- sampling::strata(marcoUPM, stratanames = "CODIGO_ICFES", 
                                 size = c3$n_i, 
                                 method="srswor", description = T)

muestra <- getdata(marcoUPM,indicamueUSM )
dim(muestra)
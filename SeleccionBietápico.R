
# Ejemplo Estimación del total MAS - MAS

library(TeachingSampling)
data(BigLucy)
library(dplyr)

# UPM: Zone
# USM: Empresas
View(BigLucy)

table(BigLucy$Zone) # 100 zonas
hist(table(BigLucy$Zone));
summary(as.numeric(table(BigLucy$Zone)))

# NI =  100 ; nI = 20
library(sampling)
set.seed(220517)
indica_UPM <- cluster(BigLucy, "Zone", size = 20, 
         description = T)
mueUPM <- getdata(BigLucy, indica_UPM)
#mueUPM <- BigLucy[indica_UPM$ID_unit,]

# Seleccionar las USM (10 % empresas)
N_i <- table(as.character(mueUPM$Zone)); length(N_i)
n_i <- round(0.1 * N_i)
# Ordenar la base por Zone
mueUPM <- mueUPM[order(mueUPM$Zone),]
#mueUPM <- arrange(mueUPM, Zone) # library(dplyr)
set.seed(220517)
indicaselUSM <- strata(data = mueUPM, stratanames = "Zone",
       size = n_i, method = "srswor", description = T)
muestra <- getdata(mueUPM, indicaselUSM )
tamanos <- as.data.frame(N_i) 
names(tamanos) <- c("Zone", "N_i")
muestra <- merge(muestra, tamanos, by = "Zone")

# Proceso de estimación
# paso 1: calcular los t_i gorro
muestra %>% group_by(Zone) %>% summarise(N_i = ,
                                         n_i = n(), SumaY_i = sum(Income))
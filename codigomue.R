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
# Cargando datos

library(readxl)
Vacas <- read_excel("~/MEGA/Israel Almanza. Nutrición y patologías período de transición/Vacas.xlsx", 
                    range = "A1:AF13")
View(Vacas)

str(Vacas)
names(Vacas)
rm(Vacas)

# Intalación de liberías

library(car)
library(epiR)
library(plotrix)


# Problemas pre-ración y durante la ración

Vacas$`Problemas pre-ración...11` <- as.factor(Vacas$`Problemas pre-ración...11`)
Vacas$`Problemas durante la ración` <- as.factor(Vacas$`Problemas durante la ración`)

Vacas$`Problemas pre-ración...11`<- factor(Vacas$`Problemas pre-ración...11`,
                                         levels = levels(Vacas$`Problemas pre-ración...11`),
                                         labels = c("Positivo antes", "Negativo antes"),
                                         ordered = F)

str(Vacas$`Problemas pre-ración...11`)


Vacas$`Problemas durante la ración`<- factor(Vacas$`Problemas durante la ración`,
                                           levels = levels(Vacas$`Problemas durante la ración`),
                                           labels = c("Positivo después", "Negativo después"),
                                           ordered = F)

str(Vacas$`Problemas durante la ración`)


mcnemar.test (Vacas$`Problemas pre-ración...11`, Vacas$`Problemas durante la ración`
              , correct = TRUE) # Test de McNemar

barplot(table(Vacas$`Problemas pre-ración...11`, Vacas$`Problemas durante la ración`),
        beside = T, col=c(2,4), ylim = c(0,6)) #Gráfico de barras agrupadas

grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = ("gray"))

barplot(table(Vacas$`Problemas pre-ración...11`, Vacas$`Problemas durante la ración`),
        beside = T, col=c(2,4), ylim = c(0,6), add = T)

legend(x = "topright",                            
       legend = rownames(table(Vacas$`Problemas pre-ración...11`, 
                               Vacas$`Problemas durante la ración`)),       
       col=c(2,4),          
       pch = 15,
       inset = c(-0.01, -0.2),
       xpd = TRUE, bty = "n")

prop.table(Vacas$`Problemas pre-ración...11`, Vacas$`Problemas durante la ración`)

epi.2by2(table(Vacas$`Problemas pre-ración...11`, Vacas$`Problemas durante la ración`), 
         "cohort.count")

round(table(Vacas$`Problemas pre-ración...11`)/
        sum(table(Vacas$`Problemas pre-ración...11`)),2)*100 #Incidencia acumulada preración
prop.test(x= 8, n=12, conf.level=0.95)$conf.int #Tamaño del efecto con IC al 95%

round(table(Vacas$`Problemas durante la ración`)/
        sum(table(Vacas$`Problemas pre-ración...11`)),2)*100 #Incidencia acumulada ración
prop.test(x= 3, n=12, conf.level=0.95)$conf.int #Tamaño del efecto con IC al 95%


(8/(12*1))*10 # TI pre-ración
(3/(12*1))*10 # TI después de la ración
(11/((12*1)*2))*10 #DI Total


Vacas$`Problemas pre y durante la ración` <- as.factor(Vacas$`Problemas pre y durante la ración`)

Vacas$`Problemas durante la ración`<- factor(Vacas$`Problemas pre y durante la ración`,
                                             levels = levels(Vacas$`Problemas pre y durante la ración`),
                                             labels = c("Positivo después", "Negativo después"),
                                             ordered = F)

pie3D(table(Vacas$`Problemas pre y durante la ración`), col = c(2,4),
      labels = paste(round(table(Vacas$`Problemas pre y durante la ración`)/
                       sum(table(Vacas$`Problemas pre y durante la ración`)),2)*100, "%"), 
      labelcex = 1, explode = 0)

legend(x = "topright",                            
       legend = c("Positivo", "Negativo"),       
       col = c(2,4),          
       pch = 15,
       inset = c(-0.1, -0.05),
       xpd = TRUE, bty = "n")


# Retención de placenta

prop.test(x= 0, n=12, conf.level=0.95)$conf.int
round(table(Vacas$`Retención placentaria durante la ración`)/
        sum(table(Vacas$`Retención placentaria durante la ración`)),2)*100 #Incidencia acumulada ración
prop.test(x= 1, n=12, conf.level=0.95)$conf.int

(1/(12*1))*10 # DI después de la ración
(1/((12*1)*2))*10 #DI Total

pie3D(table(Vacas$`Retención placentaria durante la ración`), col = c(2,4),
      labels = paste(round(table(Vacas$`Retención placentaria durante la ración`)/
                             sum(table(Vacas$`Retención placentaria durante la ración`)),2)*100, "%"), 
      labelcex = 1, explode = 0)

legend(x = "topright",                            
       legend = c("Positivo", "Negativo"),       
       col = c(2,4),          
       pch = 15,
       inset = c(-0.1, -0.05),
       xpd = TRUE, bty = "n")


# Indigestión

round(table(Vacas$`Indigestión pre-ración`)/
        sum(table(Vacas$`Indigestión pre-ración`)),2)*100 #Incidencia acumulada preración
prop.test(x= 8, n=12, conf.level=0.95)$conf.int

(1/(12*1))*10 # DI pre-ración
(1/((12*1)*2))*10 #DI Total

pie3D(table(Vacas$`Indigestión pre-ración`), col = c(2,4),
      labels = paste(round(table(Vacas$`Indigestión pre-ración`)/
                             sum(table(Vacas$`Indigestión pre-ración`)),2)*100, "%"), 
      labelcex = 1, explode = 0)

legend(x = "topright",                            
       legend = c("Positivo", "Negativo"),       
       col = c(2,4),          
       pch = 15,
       inset = c(-0.1, -0.05),
       xpd = TRUE, bty = "n")

# Flujos anormales

round(table(Vacas$`Flujos anormales ración`)/
        sum(table(Vacas$`Flujos anormales ración`)),2)*100 #Incidencia acumulada preración
prop.test(x= 2, n=12, conf.level=0.95)$conf.int

(2/(12*1))*10 # DI ración
(2/((12*1)*2))*10 #DI Total

pie3D(table(Vacas$`Flujos anormales ración`), col = c(2,4),
      labels = paste(round(table(Vacas$`Flujos anormales ración`)/
                             sum(table(Vacas$`Flujos anormales ración`)),2)*100, "%"), 
      labelcex = 1, explode = 0)

legend(x = "topright",                            
       legend = c("Positivo", "Negativo"),       
       col = c(2,4),          
       pch = 15,
       inset = c(-0.1, -0.05),
       xpd = TRUE, bty = "n")

# Metritis

round(table(Vacas$`Metritis ración`)/
        sum(table(Vacas$`Metritis ración`)),2)*100 #Incidencia acumulada preración
prop.test(x= 2, n=12, conf.level=0.95)$conf.int

(2/(12*1))*10 # DI ración
(2/((12*1)*2))*10 #DI Total

pie3D(table(Vacas$`Metritis ración`), col = c(2,4),
      labels = paste(round(table(Vacas$`Metritis ración`)/
                             sum(table(Vacas$`Metritis ración`)),2)*100, "%"), 
      labelcex = 1, explode = 0)

legend(x = "topright",                            
       legend = c("Positivo", "Negativo"),       
       col = c(2,4),          
       pch = 15,
       inset = c(-0.1, -0.05),
       xpd = TRUE, bty = "n")

# Acidez ruminal

round(table(Vacas$`Acidez ruminal pre-ración`)/
        sum(table(Vacas$`Acidez ruminal pre-ración`)),2)*100 #Incidencia acumulada preración
prop.test(x= 1, n=12, conf.level=0.95)$conf.int

(1/(12*1))*10 # DI ración
(1/((12*1)*2))*10 #DI Total

pie3D(table(Vacas$`Acidez ruminal pre-ración`), col = c(2,4),
      labels = paste(round(table(Vacas$`Acidez ruminal pre-ración`)/
                             sum(table(Vacas$`Acidez ruminal pre-ración`)),2)*100, "%"), 
      labelcex = 1, explode = 0)

legend(x = "topright",                            
       legend = c("Positivo", "Negativo"),       
       col = c(2,4),          
       pch = 15,
       inset = c(-0.1, -0.05),
       xpd = TRUE, bty = "n")


# Mastitis

Vacas$`Mastitis pre-ración` <- as.factor(Vacas$`Mastitis pre-ración`)
Vacas$`Mastitis durante la ración`<- as.factor(Vacas$`Mastitis durante la ración`)

Vacas$`Mastitis pre-ración`<- factor(Vacas$`Mastitis pre-ración`,
                                           levels = levels(Vacas$`Mastitis pre-ración`),
                                           labels = c("Positivo antes", "Negativo antes"),
                                           ordered = F)

str(Vacas$`Mastitis pre-ración`)


Vacas$`Mastitis durante la ración`<- factor(Vacas$`Mastitis durante la ración`,
                                             levels = levels(Vacas$`Mastitis durante la ración`),
                                             labels = c("Positivo después", "Negativo después"),
                                             ordered = F)

str(Vacas$`Mastitis durante la ración`)


mcnemar.test (Vacas$`Mastitis pre-ración`, Vacas$`Mastitis durante la ración`
              , correct = TRUE)

barplot(table(Vacas$`Mastitis pre-ración`, Vacas$`Mastitis durante la ración`),
        beside = T, col=c(2,4), ylim = c(0,10))

grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = ("gray"))

barplot(table(Vacas$`Mastitis pre-ración`, Vacas$`Mastitis durante la ración`),
        beside = T, col=c(2,4), ylim = c(0,10), add = T)

legend(x = "topright",                            
       legend = rownames(table(Vacas$`Mastitis pre-ración`, 
                               Vacas$`Mastitis durante la ración`)),       
       col=c(2,4),          
       pch = 15,
       inset = c(-0.01, -0.2),
       xpd = TRUE, bty = "n")

table(Vacas$`Mastitis pre-ración`, Vacas$`Mastitis durante la ración`)

epi.2by2(table(Vacas$`Mastitis pre-ración`, Vacas$`Mastitis durante la ración`), 
         "cohort.count")

round(table(Vacas$`Mastitis pre-ración`)/
        sum(table(Vacas$`Mastitis pre-ración`)),2)*100 #Incidencia acumulada preración
prop.test(x= 4, n=12, conf.level=0.95)$conf.int

round(table(Vacas$`Mastitis durante la ración`)/
        sum(table(Vacas$`Mastitis durante la ración`)),2)*100 #Incidencia acumulada preración
prop.test(x= 1, n=12, conf.level=0.95)$conf.int

(4/(12*1))*10 # TI pre-ración
(1/(12*1))*10 # TI ración
(4/((12*1)*2))*10 #TI Total

pie3D(table(Vacas$`Mastitis pre y durante la ración`), col = c(2,4),
      labels = paste(round(table(Vacas$`Mastitis pre y durante la ración`)/
                             sum(table(Vacas$`Mastitis pre y durante la ración`)),2)*100, "%"), 
      labelcex = 1, explode = 0)

legend(x = "topright",                            
       legend = c("Positivo", "Negativo"),       
       col = c(2,4),          
       pch = 15,
       inset = c(-0.1, -0.05),
       xpd = TRUE, bty = "n")

# Patologías en relación a la raza

Vacas$Raza...3 <- as.factor(Vacas$Raza...3)
Vacas$`Problemas pre y durante la ración`<- as.factor(Vacas$`Problemas pre y durante la ración`)

Vacas$Raza...3 <- factor(Vacas$Raza...3,levels = levels(Vacas$Raza...3),
                         labels = c("Holstein", "Jersey", "Holstein x Montbeliarde",
                                    "Jersey x Holstein", "Parda x Holstein"), ordered = F)

str(Vacas$Raza...3)
table(Vacas$Raza...3)

Vacas$`Problemas pre y durante la ración`<- factor(Vacas$`Problemas pre y durante la ración`,
                                                   levels = levels(Vacas$`Problemas pre y durante la ración`),
                                                   labels = c("Positivo", "Negativo"),
                                                   ordered = F)

str(Vacas$`Problemas pre y durante la ración`)



chisq.test (Vacas$Raza...3, Vacas$`Problemas pre y durante la ración`)

barplot(table(Vacas$`Problemas pre y durante la ración`, Vacas$Raza...3),
        beside = F, col=c(2,4), las=1, cex.names = 0.6 , 
        names.arg = c("Holstein", "Jersey", "Holstein-Montbeliarde",
                                                     "Jersey-Holstein", "Parda-Holstein"))

grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = ("gray"))

barplot(table(Vacas$`Problemas pre y durante la ración`, Vacas$Raza...3),
        beside = F, col=c(2,4), add = T, las=1, cex.names = 0.6 , 
        names.arg = c("Holstein", "Jersey", "Holstein-Montbeliarde",
                      "Jersey-Holstein", "Parda-Holstein") )

legend(x = "topright",                            
       legend = rownames(table(Vacas$`Problemas pre y durante la ración`)),       
       col=c(2,4),          
       pch = 15,
       inset = c(-0.01, -0.2),
       xpd = TRUE, bty = "n")

table(Vacas$Raza...3, Vacas$`Problemas pre y durante la ración`)

round(table(Vacas$Raza...3, Vacas$`Problemas pre y durante la ración`)/
        sum(table(Vacas$Raza...3, Vacas$`Problemas pre y durante la ración`)),2)*100 #Incidencia acumulada preración

prop.test(x= 2, n=12, conf.level=0.95)$conf.int

round(table(Vacas$`Mastitis durante la ración`)/
        sum(table(Vacas$`Mastitis durante la ración`)),2)*100 #Incidencia acumulada preración
prop.test(x= 1, n=12, conf.level=0.95)$conf.int

(3/(12*1)*2)*10 # TI Hostein
(1/(12*1)*2)*10 # TI Jersey
(2/((12*1)*2))*10 #TI Holstein x Montbeliarde
(1/((12*1)*2))*10 #TI Jersey x Holstein
(1/((12*1)*2))*10 #TI Holstein x Montbeliarde


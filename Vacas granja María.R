# Cargando datos

library(readxl)
Vacas <- read_excel("~/MEGA/Israel Almanza. Nutrición y patologías período de transición/Vacas.xlsx", 
                    range = "A1:AD13")
View(Vacas)

str(Vacas)
names(Vacas)

# Intalación de paquetes y liberías

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
              , correct = TRUE)

barplot(table(Vacas$`Problemas pre-ración...11`, Vacas$`Problemas durante la ración`),
        beside = T, col=c(2,4), ylim = c(0,6))

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

table(Vacas$`Problemas pre-ración...11`, Vacas$`Problemas durante la ración`)

epi.2by2(table(Vacas$`Problemas pre-ración...11`, Vacas$`Problemas durante la ración`), 
         "cohort.count")

8/12*100 #Incidencia acumulada preración
3/12*100 #Incidencia acumulada ración

12*60
8/720*100 # DI pre-ración
3/720*100 # DI después de la ración

prop.test(x= 8, n=12, conf.level=0.95)$conf.int #Tamaño del efecto con IC al 95%
prop.test(x= 3, n=12, conf.level=0.95)$conf.int



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
0/12*100
prop.test(x= 0, n=12, conf.level=0.95)$conf.int

1/12*100
prop.test(x= 1, n=12, conf.level=0.95)$conf.int

1/720*100

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


# Indigestión
0/12*100
prop.test(x= 0, n=12, conf.level=0.95)$conf.int

1/12*100
prop.test(x= 1, n=12, conf.level=0.95)$conf.int

1/720*100

# Flujos anormales
0/12*100
prop.test(x= 0, n=12, conf.level=0.95)$conf.int

2/12*100
prop.test(x= 2, n=12, conf.level=0.95)$conf.int

2/720*100

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

4/12*100 #Incidencia acumulada preración
1/12*100 #Incidencia acumulada ración

prop.test(x= 4, n=12, conf.level=0.95)$conf.int #Tamaño del efecto con IC al 95%
prop.test(x= 1, n=12, conf.level=0.95)$conf.int

12*60
4/720*100 # DI pre-ración
1/720*100 # DI después de la ración

prop.test(x= 4, n=12, conf.level=0.95)$conf.int #Tamaño del efecto con IC al 95%
prop.test(x= 1, n=12, conf.level=0.95)$conf.int




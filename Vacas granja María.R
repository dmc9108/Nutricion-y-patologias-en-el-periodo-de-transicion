# Cargando datos

library(readxl)
Vacas <- read_excel("~/MEGA/Israel Almanza. Nutrición y patologías período de transición/Vacas.xlsx", 
                    range = "A1:AD13")
View(Vacas)

str(Vacas)
names(Vacas)

# Intalación de paquetes y liberías

install.packages("carData")
library(car)

install.packages("epiR")
library(epiR)

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
3/12*10 #Incidencia acumulada ración

12*60
8/720*100 # DI pre-ración
3/720*100 # DI después de la ración



prop.test(x= 3, n=12, conf.level=0.95)$conf.int
prop.test(x= 0, n=12, conf.level=0.95)$conf.int

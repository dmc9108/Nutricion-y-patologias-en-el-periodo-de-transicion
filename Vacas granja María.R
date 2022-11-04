library(readxl)
Vacas <- read_excel("~/MEGA/Israel Almanza. Nutrición y patologías período de transición/Vacas.xlsx", 
                    range = "A1:K13")
View(Vacas)

# Tabla de contingencia

str(Vacas)
names(Vacas)

install.packages("carData")
library(car)

Vacas$`Problemas pre-ración...10` <- as.factor(Vacas$`Problemas pre-ración...10`)
Vacas$`Problemas durante la ración` <- as.factor(Vacas$`Problemas durante la ración`)

str(Vacas)

Vacas$`Problemas pre-ración...10`<- factor(Vacas$`Problemas pre-ración...10`,
                                         levels = levels(Vacas$`Problemas pre-ración...10`),
                                         labels = c("Positivo antes", "Negativo antes"),
                                         ordered = F)

str(Vacas$`Problemas pre-ración...10`)


Vacas$`Problemas durante la ración`<- factor(Vacas$`Problemas durante la ración`,
                                           levels = levels(Vacas$`Problemas durante la ración`),
                                           labels = c("Positivo después", "Negativo después"),
                                           ordered = F)

str(Vacas$`Problemas durante la ración`)


table(Vacas$`Problemas pre-ración...10`, Vacas$`Problemas durante la ración`)


mcnemar.test (Vacas$`Problemas pre-ración...10`, Vacas$`Problemas durante la ración`
              , correct = TRUE)

barplot(table(Vacas$`Problemas pre-ración...10`, Vacas$`Problemas durante la ración`),
        beside = T, col=2:3) 



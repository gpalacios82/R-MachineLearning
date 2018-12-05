# Datos de Kaggle
# Url: https://www.kaggle.com/c/titanic/data
setwd("C:/Users/Gus/Desktop/Titanic")
setwd("/Users/Gus/Desktop/RPrograms/Kaggle/Titanic/")

# Cargamos librerias

library(readr)
library(tidyverse)
library(C50)
library(rpart)
library(rpart.plot) 
library(randomForest)
library(corrplot)
library(neuralnet)

set.seed(754)

# Cargamos ficheros ofiginales
titanic_train <- read.csv("train.csv", stringsAsFactors = F)
titanic_test <- read.csv("test.csv", stringsAsFactors = F)

titanic_df <- full_join(titanic_test, titanic_train)

# Eliminamos variables que no aportan a la prediccion
#titanic_df$PassengerId <- NULL
#titanic_df$Ticket <- NULL

# Aprovechamos datos del nombre

titanic_df$Titulo <- gsub(".*, ", "", titanic_df$Name)
titanic_df$Titulo <- gsub("\\..*", "", titanic_df$Titulo)
titanic_df$NombreSinTitulo <- gsub("\\ \\(.*", "", paste(gsub("\\,\ .*", "", titanic_df$Name), gsub(".*\\. ", "", titanic_df$Name), sep=" "))
#titanic_df$Name <- NULL

NombresRepetidos <- table(titanic_df$NombreSinTitulo)
NombresRepetidos <- as_tibble(NombresRepetidos) %>% mutate(NombreSinTitulo = Var1, Billetes=n) %>% select(-Var1,-n)
titanic_df <- inner_join(titanic_df, NombresRepetidos, by="NombreSinTitulo")
                         
titanic_df$CodeTicket <- gsub("([0-9])", "", titanic_df$Ticket %>% toupper())
titanic_df$CodeTicket <- gsub("\\.", "", titanic_df$CodeTicket)
titanic_df$CodeTicket <- gsub("\\/", "", titanic_df$CodeTicket)
titanic_df$CodeTicket <- gsub("\\ ", "", titanic_df$CodeTicket)

titanic_df$CodeTicket[titanic_df$CodeTicket==""] <- "-"

# Cambiamos codigos a strings para tratarlos como variables categoricas de forma mas clara

titanic_df <- titanic_df %>%
  mutate(Clase = recode(Pclass, '1'='Alta', '2'='Media', '3'='Baja'),
         Familiares=(SibSp + Parch + 1),
         EmbarcadoEn = recode(Embarked, 'Q'='Queenstown', 'C' = 'Cherbourg', 'S' = 'Southampton'),
         Precio = round(Fare,2),
         CabinaPpal = substring(titanic_df$Cabin,1,1),
         Titulo = recode(Titulo, 
                         'Mr'='Mr',
                         'Mrs'='Mrs',
                         'Miss'='Miss',
                         'Dona'='Mrs',
                         'Master'='Tripulacion',
                         'Ms'='Miss',
                         'Dr'='Tripulacion',
                         'Don'='Mr',
                         'Major'='Tripulacion',
                         'Lady'='Mss',
                         'Sir'='Mr',
                         'Capt'='Tripulacion',
                         .default = 'Otros')
  )# %>%
  #select(-Pclass,-SibSp,-Parch,-Embarked,-Fare,-Cabin)

# Calculo datos CabinaPpal
titanic_df %>% 
  filter(CabinaPpal!="") %>%
  group_by(CabinaPpal) %>%
  summarise(PrecioMin = min(Precio), PrecioMax = max(Precio)) %>%
  select(CabinaPpal, PrecioMin, PrecioMax) %>%
  arrange(PrecioMax)

titanic_df <- titanic_df %>%
  mutate(CabinaPpal_c = cut(Precio,
                            breaks=c(0,16,35,39,81,113,134,263,99999),
                            labels=c('G','T','F','A','D','E','C','B'))) %>%
  select(-CabinaPpal)

#titanic_df$Precio <- NULL

# Trabajamos y Agrupamos variables (como la edad)
#titanic_df$Age <- round(titanic_df$Age,0)

#titanic_df %>% select(Age, Survived) %>%
#  arrange(Age) %>%
#  filter(!is.na(Survived)) %>%
#  mutate(GrupoEdad = cut(Age,
#                         breaks = c(-1,1,2,5,6,11,13),
#                         labels = c('Bebes','Bebes Jodidos','Ninyos Afortunados','Ninyos','Ninyos Jodidos','Chicos Afortunados')
#    )) %>%
#  group_by(Age) %>%
#  summarise(
#    Total = n(),
#    Supervivientes = sum(Survived),
#    Muertes = Total-Supervivientes,
#    Tasa = Supervivientes*100/Total) %>% ggplot(aes(x=Age,y=Tasa)) + 
#  geom_bar(stat = "identity")

# GGplot
titanic_df %>%
  filter(!is.na(Survived), !is.na(Age)) %>%
  group_by(Sex, CabinaPpal_c, Titulo, Age) %>%
  mutate(total = n()) %>%
  ggplot(aes(Age, Precio)) + 
  geom_jitter(aes(col=Sex, shape=Clase)) + 
  geom_smooth() + 
  scale_y_continuous("Precio", limits=c(5,50), expand = c(0,0)) +
  facet_grid(. ~ Survived)


# Calculamos edades
titanic_df$Age[is.na(titanic_df$Age)] <- mean(titanic_df$Age[!is.na(titanic_df$Age)])

# Factorizamos variables categoricas
titanic_df$Sex <- as.factor(titanic_df$Sex)
titanic_df$Age <- as.numeric(titanic_df$Age)
titanic_df$Titulo <- as.factor(titanic_df$Titulo)
titanic_df$Clase <- as.factor(titanic_df$Clase)
titanic_df$Familiares <- as.numeric(titanic_df$Familiares)
titanic_df$EmbarcadoEn <- as.factor(titanic_df$EmbarcadoEn)
titanic_df$CabinaPpal_c <- as.factor(titanic_df$CabinaPpal_c)
titanic_df$Billetes <- as.numeric(titanic_df$Billetes)
titanic_df$CodeTicket <- as.factor(titanic_df$CodeTicket)

# Intentamos ver algun insight
titanic_df %>% ggplot(aes(Age)) + geom_histogram()
titanic_df %>% ggplot(aes(log(Age))) + geom_histogram()
titanic_df %>% ggplot(aes(Precio)) + geom_histogram()
titanic_df %>% ggplot(aes(log(Precio))) + geom_histogram()
titanic_df %>% ggplot(aes(Billetes)) + geom_histogram()
titanic_df %>% ggplot(aes(log(Billetes))) + geom_histogram()

titanic_df$PrecioL <- log(titanic_df$Precio)
titanic_df$AgeL <- log(titanic_df$Age)

titanic_df$AgeS <- round(titanic_df$Age/3)
titanic_df$Precio[is.na(titanic_df$Precio)] <- 0
titanic_df$PrecioS <- round(titanic_df$Precio/20)


# Generamos modelo con arbol de decision. Esto se lo copio a Daniel

# Vamos a por ello, resultado sera el df a entregar

modelo1 <- randomForest(factor(Survived) ~ 
                          AgeS + Titulo + Clase + Familiares + Sex + PrecioS + Billetes + CodeTicket,
                        data=filter(titanic_df, !is.na(Survived)),ntree=10000, norm.votes=FALSE)
plot(modelo1)

titanic_df_na <- titanic_df %>% filter(is.na(Survived))
prediccion_final <- predict(modelo1, titanic_df, predict.all=TRUE)
df_prediccion_final <- as.data.frame(prediccion_final)

titanic_df_na <- cbind(titanic_df,df_prediccion_final)

resultado <- titanic_df_na %>% filter(is.na(Survived)) %>% select(PassengerId, Survived = aggregate)

# Generamos fichero
datos_a_entregar <- read.csv("test.csv", stringsAsFactors = F) %>% select(PassengerId)
resultado_a_entregar <- inner_join(resultado, datos_a_entregar)
write_csv(resultado_a_entregar, "resultado.csv")


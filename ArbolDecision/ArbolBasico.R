install.packages("C50")
install.packages("rpart")
install.packages("rpart.plot") 


library(C50)
library(rpart)
library(rpart.plot) 
data(churn); # carga tablas

Variables      <-c(4,7,16,19,17,20)               # variables elegidas
Entrenamiento  <-churnTrain[,Variables]           # tabla entrenamiento
Test           <-churnTest [,Variables]           # tabla  Test


# PASO 2:   Crea Arbol de Decision
# ---------------------------------------------------------------------------
ModeloArbol<-rpart(churn ~ .,data=Entrenamiento,parms=list(split="information"))


# PASO 3:  Predice Desafiliaci??n en datos de TEST
# ---------------------------------------------------------------------------
Prediccion <- predict(ModeloArbol, Test,type="class") # Prediccci??n en Test
MC         <- table(Test[, "churn"],Prediccion) # Matriz de Confusi??n


# PASO 4: Crea Grafico
# ---------------------------------------------------------------------------
rpart.plot(ModeloArbol, type=1, extra=100,cex = .7,
           box.col=c("gray99", "gray88")[ModeloArbol$frame$yval])
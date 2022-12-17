## ----setup, include=FALSE-----------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------------------------------------------
banco <- read.csv("~/Tarea_R/banco.csv")


## -----------------------------------------------------------------------------------------------------------------
names(banco)


## -----------------------------------------------------------------------------------------------------------------

length(names(banco))




## -----------------------------------------------------------------------------------------------------------------
nrow(banco)


## -----------------------------------------------------------------------------------------------------------------
str(banco)


## -----------------------------------------------------------------------------------------------------------------

banco$Online=factor(banco$Online,levels=c(0,1),labels = c("No","Si"))
banco$familia=factor(banco$familia)
banco$CreditCard=factor(banco$CreditCard,levels=c(0,1),labels = c("No","Si"))
banco$valores=factor(banco$valores,levels=c(0,1),labels = c("No","Si"))
banco$prestamo=factor(banco$prestamo,levels=c(0,1),labels = c("No","Si"))
banco$certificado=factor(banco$certificado,levels=c(0,1),labels = c("No","Si"))
banco$educacion=factor(banco$educacion)



## ----message=FALSE,warning=FALSE----------------------------------------------------------------------------------
library(ggplot2)
ggplot(banco) +
 aes(x = prestamo) +
 geom_bar(fill = "#112446") +
 labs(title = "Grafico de Prestamo") +
 theme_minimal()

ggplot(banco) +
 aes(x = familia) +
 geom_bar(fill = "#112446") +
 labs(title = "Grafico de Familia") +
 theme_minimal()

ggplot(banco) +
 aes(x = educacion) +
 geom_bar(fill = "#112446") +
 labs(title = "Grafico de Educacion") +
 theme_minimal()

ggplot(banco) +
 aes(x = valores) +
 geom_bar(fill = "#112446") +
 labs(title = "Grafico de Valores") +
 theme_minimal()

ggplot(banco) +
 aes(x = certificado) +
 geom_bar(fill = "#112446") +
 labs(title = "Grafico de Certificado") +
 theme_minimal()

ggplot(banco) +
 aes(x = Online) +
 geom_bar(fill = "#112446") +
 labs(title = "Grafico de Online") +
 theme_minimal()

ggplot(banco) +
 aes(x = CreditCard) +
 geom_bar(fill = "#112446") +
 labs(title = "Grafico de CreditCard") +
 theme_minimal()


## ----message=FALSE,warning=FALSE----------------------------------------------------------------------------------

library(dplyr)
banco_numeric<-banco %>% select_if(is.numeric)
banco_numeric<-select(banco_numeric,-ID,-ZIP_cod)


aux1<-banco_numeric %>% summarise_all(.funs = list(media = mean, mediana = median,minimo=min,maximo=max,desvi=sd)) %>% t
aux2<-banco_numeric %>% summarise_all(.funs = list(media = mean)) %>% t
aux3<-banco_numeric %>% summarise_all(.funs = list(desvi=sd)) %>% t
aux4<-as.data.frame((aux3/aux2))

aux1<-as.data.frame(aux1)
rownames( aux4 )<-c("edad_cv","experiencia_cv","ingreso_cv","gasto_tarjeta_cv","hipoteca_cv")
tabla1<-rbind(aux1,aux4)
colnames(tabla1)<-"Valor"
tabla1


## -----------------------------------------------------------------------------------------------------------------
library(janitor)

t2 <- banco %>%
  tabyl(prestamo)
t2


## -----------------------------------------------------------------------------------------------------------------
ggplot(banco) +
 aes(x = prestamo) +
 geom_bar(fill = "#112446") +
 labs(title = "Grafico de Prestamo") +
 theme_minimal()



## -----------------------------------------------------------------------------------------------------------------
library(fdth)
fdt(banco$gasto_tarjeta)



## -----------------------------------------------------------------------------------------------------------------
ggplot(banco_numeric) +
  aes(x = gasto_tarjeta) +
  geom_histogram(bins = 30L, fill = "#112446") +
  labs(
    x = "Gasto tarjeta",
    y = "Cantidad",
    title = "Histograma de gasto de tarjeta"
  ) +
  theme_light()


## -----------------------------------------------------------------------------------------------------------------

ggplot(banco_numeric) +
  aes(x = gasto_tarjeta) +
  geom_density(adjust = 1L, fill = "#112446") +
  labs(
    x = "Gasto tarjeta",
    y = "Densidad",
    title = "Gasto de tarjeta: Densidad"
  ) +
  theme_minimal()


## -----------------------------------------------------------------------------------------------------------------
qqnorm(banco$gasto_tarjeta)
qqline(banco$gasto_tarjeta, col="blue")


## -----------------------------------------------------------------------------------------------------------------
shapiro.test(banco$gasto_tarjeta)


## -----------------------------------------------------------------------------------------------------------------
ggplot(banco) +
 aes(x = prestamo, y = gasto_tarjeta) +
 geom_boxplot(fill = "#112446") +
 labs(title = "Gasto tarjeta vs Prestamo", 
 subtitle = "Grafico de caja y bigote") +
 theme_minimal()



## -----------------------------------------------------------------------------------------------------------------


apply(apply(banco, 2, is.na), 2,sum) 



## -----------------------------------------------------------------------------------------------------------------
mcorre<-cor(banco_numeric)
mcorre


## -----------------------------------------------------------------------------------------------------------------
library(corrplot)

corrplot(mcorre, method = "number")



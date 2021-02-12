library(dplyr)
library(tidyverse)
library(readxl)
library(forecast)
library(xts)
library(ggplot2)
library(zoo)
library(gridExtra)
library(ggpubr)
library(Hmisc)
library(dynlm)
library(tsoutliers)
library(janitor)
library(TSA)

rm(list=ls())
data <- read_excel("data.xlsx")

colgate <- data$Colgate
crest <- data$Crest
skimr::skim(data)




par(mfrow= c(2,1))
plot(colgate, type ="l")
plot(crest, type ="l")


fechas <- seq(as.Date('1958-01-08'), as.Date('1963-04-23'), by = 'week')

data$fecha <- as.Date(paste(data$Year, data$Week, 1, sep = "-"), "%Y-%U-%u")
data$Year <- NULL
data$Week <- NULL

data <- clean_names(data)
attach(data)


zcolgate <- as.zoo(colgate)
zcrest <- as.zoo(crest)


ggplot()+
  geom_line(data=data, aes(x=fecha,y=colgate,color="colgate"),size=0.75)+
  geom_line(data=data,aes(x=fecha,y=crest,color="crest"),size=0.75)+
  scale_color_manual(name = "Colors", values = c("colgate" = "darkgreen", "crest" = "darkred"))+
  geom_vline(xintercept=as.numeric(data$fecha[135]), colour="black",size=1.5)+
  ggtitle("Cuota del mercado")+
  ylab("%")+
  theme_minimal()
       


ggtsdisplay(zcrest)



ocolgate <- window(zcolgate, start = index(zcolgate[1]),
                   end = index(zcolgate[length(zcolgate) - 16]))
ocrest <- window(zcrest, start = index(zcrest[1]),
                   end = index(zcrest[length(zcrest) - 16]))
 
#arima

arima_colgate <- auto.arima(ocolgate)
summary(arima_colgate)

 
arima_crest <- auto.arima(ocrest)
summary(arima_crest)

checkresiduals(arima_colgate)
checkresiduals(arima_crest)


#OUTLIERS


detectAO(arima_colgate)
detectAO(arima_crest)

detectIO(arima_colgate)
detectIO(arima_crest)


#INTERVENCIÓN

crest_intervencion <- arimax(ocrest, 
                                    order =c(0,1,1),
                                    include.mean=TRUE,
                                    xtransf = data.frame(step=1*(seq(ocrest)>=135)),
                                    transfer = list(c(0,0)),
                                    xreg = data.frame(AO136 = 1*(seq(ocrest) == 136),
                                                      AO138 = 1*(seq(ocrest) == 138)),
                                    method = "ML")

plot(crest_intervencion$coef, type = "h") #B=0 (NO HAY CEROS) S=0 (DECAIMIENTO INMEDIATO); R=1 (CAE EXPONENICAL)
ggtsdisplay(crest_intervencion$residuals)
summary(crest_intervencion)


colgate_intervencion <- arimax(ocolgate, 
                             order =c(0,1,1),
                             include.mean=TRUE,
                             xtransf = data.frame(step=1*(seq(ocolgate)>=135)),
                             transfer = list(c(0,0)),
                             method = "ML")

plot(colgate_intervencion$coef, type = "h") #B=0 (NO HAY CEROS) S=0 (DECAIMIENTO INMEDIATO); R=1 (CAE EXPONENICAL)
ggtsdisplay(colgate_intervencion$residuals)
summary(colgate_intervencion)


par(mfrow=c(2,1))
plot(crest, type = "l")
points(fitted(crest_intervencion))
plot(colgate, type = "l")
points(fitted(colgate_intervencion))


#TRANSFERENCIA


crest_135 <- window(crest,end=134)
colgate_135 <- window(colgate,end=134)

mod0 <- arimax(colgate_135,
               order=c(0,1,1),
               include.mean=TRUE,
               xtransf=crest_135,
               transfer=list(c(0,20)),
               method="ML")

summary(mod0)
ggtsdisplay(mod0$residuals) #eligo hacer diferencias pese a qeu sale por poco
plot(mod0$coef, type = "h")



#-----------------------

crest_135_d <- diff(crest_135)
colgate_135_d <- diff(colgate_135)

mod0_d <- arimax(crest_135_d,
               order=c(0,1,1),
               include.mean=TRUE,
               xtransf=colgate_135_d,
               transfer=list(c(0,10)),
               method="ML")
summary(mod0_d)
ggtsdisplay(mod0_d$residuals) #sale peor
plot(mod0_d$coef, type = "h")
#-------------------------------


mod1 <- arimax(colgate_135,
               order=c(0,1,1),
               include.mean=TRUE,
               fixed=c(NA,NA,NA),
               xtransf=crest_135,
               transfer=list(c(1,0)),
               method="ML")

summary(mod1)
ggtsdisplay(mod1$residuals) #eligo hacer diferencias pese a qeu sale por poco
plot(mod1$coef, type = "h")


# analisis de impacto
crest_intervencion$coef
ADA_crest <- 1*(seq(ocrest)>=135)


colgate_intervencion$coef
ADA_colgate <- 1*(seq(ocolgate)>=135)


par(mfrow=c(2,1))
plot(ts(ADA_crest*(0.13359328)))
plot(ts(ADA_colgate*(-0.1015440)))




#prediccion

fore_arima_colgate <- forecast(arima_colgate, h=30)
plot(fore_arima_colgate)
lines(window(colgate),type="o")


fore_arima_crest <- forecast(arima_crest, h=30)
plot(fore_arima_crest)
lines(window(crest),type="o")



fore_arima_time <- function(x, h) {
  forecast(auto.arima(x), h = h)
}


cv_arima <- tsCV(ocolgate, fore_arima_time, h=1)
sqrt(mean(cv_arima^2, na.rm = T))

plot(cv_arima)

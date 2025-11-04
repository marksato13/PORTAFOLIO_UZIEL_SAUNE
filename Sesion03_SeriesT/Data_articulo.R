attach(tesita_zabbix)
names(tesita_zabbix)
install.packages("tseries")
install.packages("astsa")
install.packages("forecast")
install.packages("foreign")
install.packages("quantmod")

library(astsa)
library(tseries)
library(lubridate)
library(tidyverse)
library(forecast)

cpu <- tesita_zabbix$cpu_usage
cpu_ts <- ts(cpu, start = c(1), frequency = 720)
cpu_ts
plot(cpu_ts)
plot(cpu_ts, main="Serie temporal del uso de CPU", ylab="CPU Usage (%)", xlab="Día")
#Observamos que de lo que está en lo mínimo sube y luego baja a full, así que tenemos que hacer estacionariedad
#ya que es requisito para hacer el ModeloArima, lo siguiente que haremos es para estacionar mediante log
#Gráfica con logaritmos
serielog=log(cpu_ts)
serielog
plot(serielog,lty="dashed",ylab = "Uso de CPU (%)", col = "brown", main = "Serie de Tiempo del CPU")
#Verificamos que si es una serie estacionaria
adf.test(serielog,alternative = "stationary")

#Ahora haremos la función de auto-correlación y la de auto-correlación parcial, sirve para 
#saber cuantas medias móviles y cuantos auto-regresivos vamos a utilizar en nuestro modelo ARIMA
par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
acf(serielog)
pacf(serielog)
#El resago en ACF empieza de 0.0, así que tenemos que hacer que el segundo coincida con las frecuencias
acf(ts(serielog, frequency = 1))
pacf(ts(serielog, frequency = 1))
#Ahora vamos a generar nestro modelo 1 con 1 Auto-regresivo, 0 diferencias(no fue necesario), y 1 MA
modelo1 <- arima(cpu_ts, order = c(1, 0, 1))
modelo1
tsdiag(modelo1)
#Ahora para saber el promedio de neustro ruidoBlanco haremos la prueba de Ljung-Box
Box.test(residuals(modelo1), type = "Ljung-Box")
#Prueba personal y vemos que no hay tanto correlación así que va mejorando (p-value encima de la línea azul)
modelo2 <- arima(cpu_ts, order = c(2, 0, 2))
modelo2
tsdiag(modelo2)
Box.test(residuals(modelo2), type = "Ljung-Box")

#El que tiene mejor AIC es el que mejor se ajusta
AIC(modelo1, modelo2)

#El modelo 3 lo mejoraremos, la ejecutar observamos que ya todos estan por encima y es mejor aunque
#va a ser mas pesado al consumir y será mas complejo
modelo3 <- arima(cpu_ts, order = c(3, 0, 3))
modelo3
tsdiag(modelo3)
Box.test(residuals(modelo3), type = "Ljung-Box")

#Ahora lo haremos con el modelo de Auto-Arima haber que nos dice
modelo4_auto <- auto.arima(cpu_ts)
modelo4_auto
tsdiag(modelo4_auto)
Box.test(residuals(modelo4_auto), type = "Ljung-Box")

#RESUMEN
AIC(modelo1, modelo2, modelo3)

#Calculando el error del modelo
error=residuals(modelo3)
plot(error)

#Ahora hacemos el pronóstico del 7mo día
pronostico <- forecast::forecast(modelo3, h = 720)
pronostico
plot(pronostico)

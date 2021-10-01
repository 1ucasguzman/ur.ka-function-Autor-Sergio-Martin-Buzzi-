###### SCRIPT REUNION JUEVES 30/09 #####
library(urca)
library(quantmod)
library(readr)
library(lubridate)
library(ggplot2)
library(magrittr)

merval_usd <- read_delim("D:/Lucas/Economía/TESINA/merval_merval_usd.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(date = col_date(format = "%d/%m/%Y")), 
                         trim_ws = TRUE)

merval_usd <- xts(merval_usd$MERV_USD, order.by = as.Date(merval_usd$date))

merval_usd <- merval_usd['/2019']

names(merval_usd) <- "MERV"

merval_usd <- log(merval_usd$MERV)
#merval_usd <- log(to.monthly(merval_usd$MERV, OHLC=F))
index(merval_usd) <- as.Date(index(merval_usd))
autoplot(merval_usd)+
  theme_bw()


urdf<- ur.df(merval_usd$MERV, type = "trend")
summary(urdf) #No se puede rechazar no estacionariedad
plot(urdf)

urkpss<- ur.kpss(merval_usd, type='mu') #Se rechaza estacionariedad al 2.5%
summary(urkpss)
plot(urkpss)



urza<- ur.za(merval_usd, model = "both") #No se puede rechazar no estacionariedad. H0 de ZA es raíz unitaria.
summary(urza)
plot(urza)
merval_usd[urza@bpoint]

#for(i in seq(1,12,1)){
#  print(summary(ur.za(merval_usd, model="both", lag=i)))
#}

#ahora que tengo el punto 101, pruebo agregando la dummie de ese evento a los controles

#merval_usd$"dummie" <- ifelse(stringi::stri_sub(date(merval_usd),length = 7) == "2008-06",1,0)

#fit1<- lm(diff(merval_usd$MERV) ~ 1 + lag(merval_usd$MERV) + time(merval_usd$MERV) + diff(lag(merval_usd$MERV)) + merval_usd$dummie)
#
#summary(fit1)
#checkresiduals(fit1)
#checkresiduals(urza@testreg$residuals)
#
#breakpoint at 204
#merval_usd[204]
# diciembre 2016.
#
#ymd(merval_usd[[204]])
#
#autoplot.zoo(merval_usd$MERV)+
# geom_vline(xintercept = as.Date("2018-01-01"), color = "black", lwd = 0.5, lty=2)+
#  geom_vline(xintercept = as.Date("2020-12-01"), color = "black", lwd = 0.5, lty=2)+
#  geom_line()


#serie1 <- ts(merval_usd$MERV, start = c(2000,1), end= c(2019,12), frequency = 12)
#strucchange::breakpoints(serie1 ~ + time(serie1))

##### ur.ka ##### test de raiz unitaria con hasta 5 quiebres estructurales.

autoplot(merval_usd) +
  labs(x = "Año", y = "Log_Merval en USD")+
  theme_bw()

#geom_segment(aes(x = as.Date('2000-01-01'), y = 6, xend = as.Date('2005-01-01'), yend = 7))
################
for (i in 1){
  inicio<- Sys.time()
  urka <- ur.ka(merval_usd$MERV, model = 'trend', bp = 4, 36) #bp5 y 12 lag, trend, se rechaza, con 24 rezagos tmbn es "bueno" el ajuste
  #bp5, 24, both
  fin<-Sys.time()
  print(urka)
  print(stringr::str_c("tiempo de ejecucion ", fin-inicio))
}

plot(urka$testreg$residuals)

plot<- ggplot(merval_usd, aes(x = Index, y = MERV))+params
bp<-vector("list", length = length(urka$bpoints))
count<-0
for(i in urka$bpoints){
  print(merval_usd[i])
  count = count+1
  bp[[count]]<- geom_vline(xintercept = as.Date(index(merval_usd$MERV[i])), color = "black", lwd = 0.5, lty=2)
  print(plot+bp)
}

params<- list(geom_line(linetype=1, lwd=1, colour="steelblue"),
              labs(title = "Log Indice Merval - período 2000-2020"),
              theme_minimal())

#checkresiduals(lm(merval_usd$MERV - lag(merval_usd$MERV) ~ lag(merval_usd$MERV)))

#fit1<-(lm(urka$testreg$model$y ~ urka$testreg$model$y.l1+urka$testreg$model$trend+urka$testreg$model$du1+
#     urka$testreg$model$du2+urka$testreg$model$du3+urka$testreg$model$du4+urka$testreg$model$du5))

acf(urka$testreg$residuals)

summary(ur.df(merval_usd, type = "trend", selectlags = "AIC"))
summary(ur.df(merval_usd, type = "drift", selectlags = "AIC"))
summary(ur.df(merval_usd, type = "none", selectlags = "BIC"))






Box.test(urka$testreg$residuals, type="Ljung")


# NOT RUN {
x <- rnorm (100)
Box.test (x, lag = 1)
Box.test (x, lag = 2, type = "Ljung")
# }


m = c(ar, ma)
w = arima.sim(m, 120)
w = ts(w)
plot(w)
Box.test(w, type="Ljung-Box")



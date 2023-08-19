################################################################################
###### ANALISI TIME SERIES ECONOMICA SULLA PRODUZIONE EDILE IN GERMANIA   ######

rm(list = ls())

library(forecast)
library(tseries)
library(tsoutliers)
library(urca)
library(FinTS) 

source("C:/Users/alida/Desktop/UNI/3 anno/Statistica Economica/TSA-Useful-Functions.R")
source("C:/Users/alida/Desktop/UNI/3 anno/Statistica Economica/CalendarEffects-Student-Functions.R")
source("C:/Users/alida/Desktop/UNI/3 anno/Statistica Economica/TSA-Predict-Student-Functions.R")


file.data <- "C:/Users/alida/Desktop/ProductionVolume-ConstructionSector-Germany ts economica.csv"
data <- read.table(file = file.data, header = TRUE, sep = ",", quote = "", 
                   na.strings = ".", check.names = FALSE, comment.char = "")  
dati <- data[,c(9,10)]
dati$TIME_PERIOD <- paste(dati$TIME_PERIOD,"-15",sep="")               #aggiungo il giorno 15 per ogni mese
dati$TIME_PERIOD <- gsub("-","",dati$TIME_PERIOD)                      #funzione per rimuovere il trattino dalla stringa del tempo
dati$TIME_PERIOD <- as.Date(x =dati$TIME_PERIOD, format = "%Y %m %d")  #trasforma in data la variabile time


start <- as.numeric( c( format(dati$TIME_PERIOD[1], "%Y"), format(dati$TIME_PERIOD[1], "%m") ) )
y <- ts( data = dati$OBS_VALUE, start = start, frequency = 12)


##########################################################
######VARIBILI ESTERNE####################################
##########################################################

#### Calendar effects
cal <- .calendarEffects(time = dati$TIME_PERIOD, country = "it")
#### Select
#cal <- cal[, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "sh", "lh", "eh"), drop = FALSE]      #in fase di modellazione togliamo le variabili 
#cal <- cal[, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "sh", "lh", "eh"), drop = FALSE]             #di calendario meno significative una alla volta
#cal <- cal[, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "sh", "lh"), drop = FALSE]
#cal <- cal[, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "sh"), drop = FALSE]
#cal <- cal[, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), drop = FALSE]
#cal <- cal[, c("Mon", "Tue", "Wed", "Thu", "Fri"), drop = FALSE]
cal <- cal[, c( "Tue", "Wed", "Thu", "Fri"), drop = FALSE] 

#meglio mettere giorni della settimana staccati e vediamo se si comportano allo stesso modo

cal <- as.matrix(cal)  #si fa la matrice del modello con i regressori perchè non accetta data frame
#########################################################
#### Se avessi bisogno del drift
#drift <- cbind(drift = 1 : NROW(y))
#CREA UN VETTORE DRIFT DA METTERE COME REGRESSORE ESTERNO 
#########################################################








################################################################################
##              ANALISI PRELIMINARE                #############################
################################################################################
#analisi preliminare, si inizia con un po di grafici: timeseries plot, Acf, Pacf

par(mfrow = c(3,1))
x <- y
plot(x,type = "l", ylab = "", main="Produzione edile in Germania")
Acf(x = x, type = "correlation", na.action = na.pass, lag.max = 60, main=" Acf")  #Acf
Acf(x = x, type = "partial",     na.action = na.pass, lag.max = 60, main=" Pacf")  #Pacf


#Dai plot possiamo vedere che il trend è costante perchè la funzione ondeggia. La stagionalità è presente, si vede
#dalle punte che si creano a ogni lag che sono via via lineari. Quindi non è stazionario per stagionalità.
#Per questo mettiamo ordine D=1. Non solo ho un andamento lineare a distanza di ogni lag ma anche tra i lag e quindi mettiamo 
#ordine d=1. Anche dal Pacf possiamo vedere che c'è uno spaghettone alto 0.6 a lag=1 indice di non stazionarietà.




######### TEST DF E ADF CON TREND
cat("\n-----------------------------------------------------------------
  Unit root analysis\n")

#dickey-fuller 
df.1 <- ur.df(y = y, type = "trend", lags = 0, selectlags = "Fixed")  
df.2 <- ur.df(y = y, type = "drift", lags = 0, selectlags = "Fixed") 
df.3 <- ur.df(y = y, type = "none",  lags = 0, selectlags = "Fixed")  

#procedura di fonso-lisi
adf.1 <- ur.df(y = y, type = "trend", lags = 24, selectlags = "AIC")  #PASSO 1 E 2 

cat("\n-----\nTest1: ADF with trend\n")                                #accetto tutte e due i passi e vado al passo 3
print( adf.1@teststat )
print( adf.1@cval )


adf.2 <- ur.df(y = y, type = "drift", lags = 24, selectlags = "AIC") #PASSO 3 

cat("\n-----\nTest2: ADF with drift\n")
print( adf.2@teststat )
print( adf.2@cval )

#Accetto al passo 3 e ho quindi radici unitarie, si tratta di un rw senza drift.
#Accetto al passo 3 e ho quindi radici unitarie, si tratta di un rw senza drift.
#Ma quante radici ho e di chi è la colpa?Della parte stagionale o non stagionale?
#La unit root che ho sembrerebbe essere colpa della stagionalità quindi
#faccio le differenze dodicesime e noto che sono in un caso borderline ed
#essendo questi dei test poco potenti che tendono a rifiutare l'ipotesi nulla,
#anche in questo caso rifiuto anche se un po in dubbio e quindi sembrerebbe di non avere
#ur dalla parte non stagionale
diff12 <- diff(y,lag=12)
adf.3 <- ur.df(y = diff12, type = "trend", lags = 24, selectlags = "AIC")
print( adf.3@teststat )
print( adf.3@cval )




################################################################################
#####               ARIMA                  #####################################
################################################################################
#arima 
#Order = c(0, 1, 1), seasonal = list(order = c(1, 1, 2))
#Questo è il migliore

#ARIMA(0,1,0)(0,1,0)[12] AIC=2726.04   AICc=2726.06   BIC=2729.92
#ARIMA(1,1,0)(0,1,0)[12] AIC=2593.77   AICc=2593.81   BIC=2601.53 ar1 sign
#ARIMA(1,1,1)(0,1,0)[12] AIC=2514.16   AICc=2514.23   BIC=2525.79 ar1 sign, ma1 sign 
#ARIMA(1,1,1)(1,1,0)[12] AIC=2490.34   AICc=2490.45   BIC=2505.85 ar1 non sign, ma1 sign, sar1 sign
#ARIMA(1,1,1)(1,1,1)[12] AIC=2443.57   AICc=2443.74   BIC=2462.95 ar1 non sign, ma1 sign, sar1 sign, sma1 sign 
#ARIMA(0,1,1)(1,1,1)[12] AIC=2443.97   AICc=2444.09   BIC=2459.48 ma1 sign, sar1 sign , sma1 sign 
#ARIMA(0,1,1)(2,1,1)[12] AIC=2439.8    AICc=2439.97   BIC=2459.19 ma1 sign, sar1 non sign, sar2 non sign, sma1 sign
#ARIMA(0,1,1)(1,1,2)[12] AIC=2435.27   AICc=2435.44   BIC=2454.66 ma1 sign, sar1 sign, sma1 non sign, sma2 sign ma con aic, bic migliore

#Confronto tutti i modelli con AIC e BIC e con significatività dei parametri

##ARIMA
xreg <- NULL
fit <- Arima(y = y, 
             order = c(0,1, 1), seasonal = list(order = c(1, 1, 2)),
             xreg = xreg, include.constant = FALSE) 
print(summary(fit))
fit1 <- fit    


#Regression with ARIMA(0,1,1)(1,1,1)[12] errors AIC=2345.07   AICc=2345.48   BIC=2376.09
#Provo a cambiare gli ordini e AIC,BIC migliorano.

##ARIMA CON VARIABILI DI CALENDARIO
xreg <- cal
fit <- Arima(y = y,
             order = c(0, 1, 1), seasonal = list(order = c(1, 1, 1)),
             xreg = xreg, include.constant = FALSE)
print(summary(fit))
fit2 <- fit


################################################################################

#PLOT CHE RAPPRESENTA LE DIFFERENZE TRA PARTE AR E MA

par(mfrow = c(1,2))
root <- .arma.roots(fit = fit1)
.circle(win = 1.6)
points(root$root$ar, col = "red")
points(root$root$ma, col = "blue")

root <- .arma.roots(fit = fit2)
.circle(win = 1.6)
points(root$root$ar, col = "red")
points(root$root$ma, col = "blue")


#Parte Ar molto piu vicina in fit1 e molto piu lontana in fit2 


################################################################################


####FIT PER TROVARE OUTLIER
fit <- fit1
settings <- .Arima.settings(fit = fit)
xreg <- NULL
fit <- tso(y = y, xreg = xreg,
           types = c("AO", "LS", "TC"), delta = 0.7, cval =4 ,
           maxit = 10, maxit.iloop = 100, maxit.oloop = 10,
           tsmethod = "arima",
           args.tsmethod = list( order = settings$order, seasonal = settings$seasonal) )
fit1.o <- fit
print(fit)

#CON CVAL=4 NON VA A CONVERGENZA MENTRE CON CVAL=5 NON TROVA OUTLIER

################################################################################


#FUNZIONE CHE MI PRENDE OUTLIER DAL MODELLO
#oeff <- outliers.effects(mo = fit$outliers, n = NROW(y), pars = coef(fit$fit),
#                         weights = FALSE)
#xreg <- as.matrix( oeff )
#fit <- Arima(y = y,
#             order = settings$order, seasonal = settings$seasonal,
#             include.constant = settings$include.constant,
#             xreg = xreg)
## Only in case of transformed variable
# llstats.adj3 <- .loglik(fit = fit, g = "log")
#fit3 <- fit
#print(fit3)

#NON AVENDO OUTLIERS NON FACCIO IL MODELLO
################################################################################

#MODELLO CON OUTLIER SENZA VARIABILI DI CALENDARIO
#fit <- tso(y = y, xreg = xreg,
#           types = c("AO", "LS", "TC"), delta = 0.7, cval = 4,
#           maxit = 10, maxit.iloop = 100, maxit.oloop = 10,
#           # tsmethod = "auto.arima",
#           # args.tsmethod = list(allowdrift = false, ic = "bic", trace = true) )
#           tsmethod = "arima",
#           args.tsmethod = list( order = settings$order, seasonal = settings$seasonal) )
#fit1.o <- fit
#print(fit1.o)

#NON AVENDO OUTLIER NON FACCIO IL MODELLO

################################################################################


#MODELLO PER TROVARE OUTLIER NEL MODELLO CON VARIABILI DI CALENDARIO
fit <- fit2
settings <- .Arima.settings(fit = fit)
xreg <- cal
fit <- tso(y = y, xreg = xreg,
           types = c("AO", "LS", "TC"), delta = 0.7, cval = 5,
           maxit = 10, maxit.iloop = 100, maxit.oloop = 10,
           tsmethod = "arima",
           args.tsmethod = list( order = settings$order, seasonal = settings$seasonal) )
fit2.o <- fit
print(fit)
plot(fit2.o)

#MI TROVA OUTLIERS DI TIPO  E ADDITIVE OUTLIER 


#Outliers:
#  type ind    time coefhat  tstat
#1   TC  61 1996:01  -29.80 -6.719
#2   AO  73 1997:01  -27.54 -5.590

################################################################################


#ARIMA CON EFFETTI CALENDARIO, OUTLIER

oeff <- outliers.effects(mo = fit$outliers, n = NROW(y), pars = coef(fit$fit),
                         weights = FALSE)
xreg <- cbind(cal, oeff)
fit <- Arima(y = y,
             order = settings$order, seasonal = settings$seasonal,
             include.constant = settings$include.constant,
             xreg = xreg)
fit4 <- fit
print(fit4)



#AIC=2291.44   AICc=2292.08   BIC=2330.22
#Indici piu bassi rispetto agli altri fit


################################################################################
#ANALISI RESIDUI


fit <- fit4
npar1  <- NROW(fit$coef)                            ## NUMERO DI PARAMETRI
lag1   <- npar1 + c(1, 2, 5, 10, 15, 20)
res1   <- residuals(fit4)                           ## RESIDUI
resst1 <- ( res1 - mean(res1) ) / sqrt(fit$sigma2)  ## RESIDUI STANDARDIZZATI

####  #ANAISI RESIDUI CON TS PLOT, ACF, PACF 
par(mfrow = c(3,1))                                                                          
main <- "residuals"                                                                         
x1 <- res1
plot(x1, type = "l", main = "Residui Time series", ylab = "Residuals")
Acf(x = x1, type = "correlation", lag.max = 60, na.action = na.pass, main ="Residui Acf")
Acf(x = x1, type = "partial",     lag.max = 60, na.action = na.pass, main = "Residui Pacf")

#GRAFICO ACF CI MOSTRA CHE CON IL MODELLO FIT4 MIGLIORA DI TANTO

#Ljung-Box DEI RESIDUI
lb <- Box.test(x = res1, lag = 10, type = "Ljung-Box", fitdf = NROW(fit$coef))   
cat("\n", paste("Ljung-Box of", main, "at different lags\n") )
lb <- mapply(FUN = Box.test, lag = lag1, 
             MoreArgs = list(x = x1, type = "Ljung-Box", fitdf = npar1))[1:3, , drop = FALSE]
print(rbind(lag1,lb))

#Tutti i pvalue sono significativi tranne il 15


#RESIDUI AL QUADRATO
par(mfrow = c(2,1))
main <- "residuals^2"
x1 <- res1^2
plot(x1, type = "l", main = main, ylab = "")
Acf(x = x1, type = "correlation", lag.max = 60, na.action = na.pass, main = main)


#RESIDUI IN VALORE ASSOLUTO
par(mfrow = c(2,1))
main <- "|residuals|"
x1 <- abs(res1)
plot(x1, type = "l", main = main, ylab = "")
Acf(x = x1, type = "correlation", lag.max = 60, na.action = na.pass, main = main)



################################################################################
#ARCH TEST ETEROSCHEDASTICITA'
cat("\n-----------------------------------------------------------------
  ARCH based preliminary analyses\n")
cat("ARCH test on demeaned log-returns\n")
lag <- c(1, 2, 3, 6, 12, 24)
at <- mapply(FUN = ArchTest, lags = lag, 
             MoreArgs = list(x = res1^2, demean = TRUE))
print(at[1:3,])

#si accettano tutti

################################################################################

#PLOT E TEST DI NORMALITA' DEI RESIDUI
par(mfrow = c(1,2))
hist(x = resst1, breaks = 25, freq = FALSE, main = "residuals", xlab = "")
x1 <- seq(from = min(resst1), to = max(resst1)+1, length.out = 100) 
lines(x = x1, y = dnorm(x = x1, mean = 0, sd = 1), col = "red")
qqnorm(y = resst1, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
abline(a = 0, b = 1, col = "red")
################################################################################

## Test of normality 
cat("\nTest of normality\n")
print( shapiro.test(x = res1 ) )

#SI RIFIUTA LA NORMALITA' DEI RESIDUI
################################################################################




################################################################################
######              PREVSIONI EX POST           ################################
################################################################################


J  <- 12  #IN UNA TS MENSILE MENO DI 12 NON VA BENE(12 SONO IL PEZZO DI INFORMAZIONI LASCIATE FUORI)   
H  <- 1  #ORIZZONTE DI PREVISIONE                                             
t1 <- .predict.t1(nobs = NROW(y), J = J, n.ahead = H) #ABBIAMO T OSSERVAZIONI, J LE LASCIAMO FUORI,H=1, E SI CALCOLA L'ULTIMA 
#OSSERVAZIONE DENTRO L'INFORMAZIONE

#### SENZA REGRESSORI ESTERNI
pred1.1 <- .predict(object = fit1, n.ahead = H, t = t1, y = y,
                    fixed.n.ahead = TRUE)


#### SE HO VARIBILI DI CALENDARIO
newxreg <- cal
pred2.1 <- .predict(object = fit2, n.ahead = H, t = t1, y = y, xreg = newxreg,
                    fixed.n.ahead = TRUE)

#### SE HO AOUTLIER
#newxreg <- .oeff.4.predict(object = fit1.o, n.ahead = 0)#LI RIFABBRICO
#pred3.1 <- .predict(object = fit3, n.ahead = H, t = t1, y = y, xreg = newxreg,
#                    fixed.n.ahead = TRUE)

#### SE HO VARIBILI DI CALENDARIO E OUTLIER
x2 <- .oeff.4.predict(object = fit2.o, n.ahead = 0) 
newxreg <- as.matrix(cbind(cal, x2))
pred4.1 <- .predict(object = fit4, n.ahead = H, t = t1, y = y, xreg = newxreg,
                    fixed.n.ahead = TRUE)

#### NAIVE
predn.1 <- .predict.naive(fit = fit4, J = J, n.ahead = H)

#### BAMDE DI PREVISIONE
x1 <- .pred.bands(pred = pred1.1, alpha = 0.05)
x2 <- .pred.bands(pred = pred2.1, alpha = 0.05)
#x3 <- .pred.bands(pred = pred3.1, alpha = 0.05)
x4 <- .pred.bands(pred = pred4.1, alpha = 0.05)

#### MISURE DI ERRORE
em1.1  <- .ErrorMeasures(y = y, fit = x1$mean, naive = predn.1)
em2.1  <- .ErrorMeasures(y = y, fit = x2$mean, naive = predn.1)
#em3.1  <- .ErrorMeasures(y = y, fit = x3$mean, naive = predn.1)
em4.1  <- .ErrorMeasures(y = y, fit = x4$mean, naive = predn.1)
emn.1  <- .ErrorMeasures(y = y, fit = predn.1, naive = predn.1)


## STAMPA MISURE DI PREVISIONE
ErrorMeas <- data.frame(
  model = c("Arima", "Arima + Calendar","Arima + Calendar + Outliers", "Naive"),
  h = H,
  rbind( em1.1, em2.1, em4.1, emn.1, deparse.level = 0 ) )
#em3.1
print( ErrorMeas )



#### PLOT PREVISIONE EX POST
ind  <- (NROW(y) - J + 1) : NROW(y)
ind1 <- 1 : NROW(ind)
par(mfrow = c(1,1))
ylim <- range(
  x1$lower[ind1], x1$upper[ind1], x2$lower[ind1], x2$upper[ind1],x4$lower[ind1], x4$upper[ind1] )
#x3$lower[ind1], x3$upper[ind1]
time <- dati$TIME_PERIOD[ind]
plot(x = time, y = y[ind], ylim = ylim,
     main = "Previsioni ex-post degli ultimi 12 mesi", xlab = "time", ylab = "Produzione")
lines(x = time, y = x1$mean[ind1],  col = "black")
lines(x = time, y = x2$mean[ind1],  col = "blue")
#lines(x = time, y = x3$mean[ind1],  col = "violet")
lines(x = time, y = x4$mean[ind1],  col = "red")
lines(x = time, y = predn.1[ind1],  col = "green", lty = "solid")
lines(x = time, y = x4$lower[ind1], col = "red", lty = "dotted")
lines(x = time, y = x4$upper[ind1], col = "red", lty = "dotted")
legend("bottomright", inset=0, title="Previsione ex-post:", 
       legend=c("Arima","Arima+calendario","Arima+calendario+outlier","Naive"), 
       col=c("black","blue","red","green"), lty=1, cex=0.7)





################################################################################
########PREVSIONE EX ANTE#######################################################
################################################################################

H  <- 12        #PREVISIONE EX ANTE DA H=1 FINO A H=12
t1 <- NROW(y)   #SI SETTA T GRANDE CIOE' 
#L'ULTIMA OSSERVAZIONE DENTRO LINFORMAZIONE
#NUMERO DI RIGHE DI Y

time1 <- .extend.time(x = dati$TIME_PERIOD, n.ahead = H, by = "month")

#### No external regressors
newxreg <- NULL
pred1 <- .predict(object = fit1, n.ahead = H, t = t1, y = y, xreg = newxreg,
                  fixed.n.ahead = FALSE)

####  CON I GIORNI DI CALENDARIO
x1 <- .calendarEffects(time = time1)[, colnames(cal), drop = FALSE] #MI FABBRICO LE VARIABILI ESTERNE CHE MI SERVONO PER LA PREVISIONE
newxreg <- as.matrix(rbind(cal, x1))
pred2 <- .predict(object = fit2, n.ahead = H, t = t1, y = y, xreg = newxreg,
                  fixed.n.ahead = FALSE)

#### CON OUTLIER
#x2 <- .oeff.4.predict(object = fit1.o, n.ahead = H) #MI FABBRICO GLI OUTLIER PER LE PREVISIONI
#newxreg <- x2
#pred3 <- .predict(object = fit3, n.ahead = H, t = t1, y = y, xreg = newxreg,
#                  fixed.n.ahead = FALSE)

#### SE HO VARIABILI DI CALENDARIO E OUTLIER
x2 <- .oeff.4.predict(object = fit2.o, n.ahead = H) #VISTO CHE QUESTO E' UN ALTRO MODELLO MI DEVO RIFRABBRICARE GLI OUTLIERS
newxreg <- as.matrix( cbind( rbind(cal, x1), x2) )
pred4 <- .predict(object = fit4, n.ahead = H, t = t1, y = y, xreg = newxreg,
                  fixed.n.ahead = FALSE)


#### Naive
predn <- .predict.naive(fit = fit4, J = H, n.ahead = H) 

#### BANDE DI PREVISIONE
x1 <- .pred.bands(pred = pred1, alpha = 0.05)
x2 <- .pred.bands(pred = pred2, alpha = 0.05)
#x3 <- .pred.bands(pred = pred3, alpha = 0.05)
x4 <- .pred.bands(pred = pred4, alpha = 0.05)

print( cbind(t = x1$t, pred1 = x1$mean, pred2 = x2$mean, pred4 = x4$mean,
             naive = predn) )
#pred3 = x3$mean


#### PLOT CON BANDE DI PREVISIONE
par(mfrow = c(1,1))
ylim <- range(
  x1$lower, x1$upper, x2$lower, x2$upper,
  predn,x4$lower, x4$upper )
#x3$lower, x3$upper
time <- time1
plot(x = time, y = x1$mean, type = "l",
     main = "Previsioni ex-ante degli ultimi 12 mesi", xlab = "time", ylab = "Produzione",
     ylim = ylim)
lines(x = time, y = x2$mean,  col = "blue")
#lines(x = time, y = x3$mean,  col = "violet")
lines(x = time, y = x4$mean,  col = "red")
lines(x = time, y = x4$lower, col = "red", lty = "dotted")
lines(x = time, y = x4$upper, col = "red", lty = "dotted")
lines(x = time, y = predn, col = "green")
legend("bottomright", inset=0, title="Previsione ex-post:", 
       legend=c("Arima","Arima+calendario","Arima+calendario+outlier","Naive"), 
       col=c("black","blue","red","green"), lty=1, cex=0.7)

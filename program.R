# wczytanie bazy danych
dane <-read.csv2("dane_C.csv")

#1 - ustalenie zbioru uczącego i zbioru testowego
set.seed(1234)
(rozmiar.proby <- 0.60 * nrow(dane))
(index = sample(seq_len(nrow(dane)), size = rozmiar.proby ))
zbior.uczacy <-dane[index, ]
zbior.testowy<-dane[-index,]
####


#2 - oszacowanie i wybór modelu regresji liniowej
model.full<- lm(Rating~. , data = zbior.uczacy)

wybrany.model<-step(model.full, direction = "both", trace=FALSE ) 
#direction both oznacza wykorzystanie statystyki Cp Mallowsa do wyboru optymalnego modelu

# sprawdzenie współliniowości modelu
library(car)
vif(wybrany.model)

# sprawdzenie korelacji między zmiennymi objaśniającymi wybranymi do modelu
cor(zbior.uczacy[,c(1,2,4,10)] )

# ze względu na współczynnik vif oraz korelację, z modelu została usunięta zmienna objaśniająca Limit
wybrany.model<-lm(Rating~ Income + Cards + Balance, data=zbior.uczacy)

summary(wybrany.model)
coef(wybrany.model)
#####


#3 - dopasowanie modelu
wyniki<-summary(wybrany.model)
  
# współczynnik determinacji
wyniki$r.squared

# skorygowany współczynnik determinacji
wyniki$adj.r.squared

# kryterium Akaike (AIC)
library(stats)
AIC(wybrany.model)

# wykresy
plot(wybrany.model, which=1) #Residuals vs Fitted
plot(wybrany.model, which=2) #Normal Q-Q
plot(wybrany.model, which=3) #Scale Location 
plot(wybrany.model, which=4) #Cook?s distance
plot(wybrany.model, which=5) #Residuals vs Leverage
plot(wybrany.model, which=6) #Cook?s dist vs Leverage
####


#4 - testy diagnostyczne

# istotność parametrów
## test t-Studenta
wyniki$coefficients

## test F
wyniki$fstatistic
pValue.F<-pf(wyniki$fstatistic[1], wyniki$fstatistic[2], wyniki$fstatistic[3], lower.tail = FALSE)
pValue.F

wyniki

# współliniowość
library(car)
vif(wybrany.model)

# analiza reszt - testy diagnostyczne
reszty<-residuals(wybrany.model)

# normalność reszt
## test Shapiro-Wilka
shapiro.test(reszty)

## test Jarque-Bera
library(tseries)
jarque.bera.test(reszty)


# jednorodność wariancji
## test Breuscha-Pagana
library(lmtest)
bptest(wybrany.model)


## test Goldfelda-Quandta
library(tseries)

Income=as.factor(zbior.uczacy$Income)
Cards=as.factor(zbior.uczacy$Cards)
Balance=as.factor(zbior.uczacy$Balance)

gqtest(wybrany.model)
gqtest(wybrany.model, order.by=Income,point=0.5)
gqtest(wybrany.model, order.by=Cards,point=0.5)
gqtest(wybrany.model, order.by=Balance,point=0.5)


## test Harrisona-McCabe
hmctest(wybrany.model, plot=TRUE)
hmctest(wybrany.model, order.by=Income,point=0.5,plot=TRUE)
hmctest(wybrany.model, order.by=Cards,point=0.5, plot=TRUE)
hmctest(wybrany.model, order.by=Balance,point=0.5, plot=TRUE)

par(mfrow=c(1,3))
hmctest(wybrany.model, order.by=Income,point=0.5,plot=TRUE)
hmctest(wybrany.model, order.by=Cards,point=0.5, plot=TRUE)
hmctest(wybrany.model, order.by=Balance,point=0.5, plot=TRUE)

par(mfrow=c(1,1))


# niezależność reszt
## test Durbina-Watsona 
dwtest(wybrany.model, alternative = "two.sided")
dwtest(wybrany.model, order.by=Income, alternative = "two.sided")
dwtest(wybrany.model, order.by=Cards, alternative = "two.sided")
dwtest(wybrany.model, order.by=Balance, alternative = "two.sided")


## test Breuscha-Godfreya 
rzad<-2
bgtest(wybrany.model, order=rzad)
bgtest(wybrany.model, order.by=Income, order=rzad)
bgtest(wybrany.model, order.by=Cards, order=rzad)
bgtest(wybrany.model, order.by=Balance, order=rzad)


# liniowość modelu
## test Harveya-Colliera
harvtest(wybrany.model)


## test Ramseya RESET 
resettest(wybrany.model, power = 2)
resettest(wybrany.model, power = 3)
resettest(wybrany.model, power = 2:5)
####


#5 - wyznaczanie prognoz dla zbioru testowego
#head(zbior.testowy)
prognozy.testowy<-predict(wybrany.model, zbior.testowy)
prognozy.testowy
zestawienie<-data.frame(Rzeczywiste=zbior.testowy$Rating, Prognozy=prognozy.testowy)
head(zestawienie)

plot(zestawienie$Rzeczywiste, type="b",pch=20, col=3,ylab="Cena")
points(zestawienie$Prognozy,type="b", pch=18, col=2)
legend("top", c("Rzeczywiste", "Prognozy"), pch=c(20,18),lty=1, col=c(3,2))


#5 - obliczenie mierników ex-post błędów prognoz
rzeczywiste.testowy<-zbior.testowy$Rating
library(forecast)
bledy.ex.post<-accuracy(ts(prognozy.testowy),ts(rzeczywiste.testowy))
bledy.ex.post

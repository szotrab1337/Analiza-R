# Wczytanie bazy danych:
dane <-read.csv2("dane_C2.csv") #Wczytanie zbioru uczacego

#1
#Losowanie zbioru uczacego
set.seed(1234)
(rozmiar.proby <- 0.60 * nrow(dane))
(index = sample(seq_len(nrow(dane)), size = rozmiar.proby ))
zbior.uczacy <-dane[index, ]
zbior.testowy<-dane[-index,]
####

#2 sposob 3
model.full<- lm(Rating~. , data = zbior.uczacy)

#direction both oznacza wykorzystywanie statystyki Cp Mallowsa do wyboru optymalnego modelu
wybrany.model<-step(model.full, direction = "both", trace=FALSE ) 

summary(wybrany.model)
coef(wybrany.model)

wybrany.model <- lm(Rating~ Income + Limit + Cards + Balance , data = zbior.uczacy)
#####

# plot(wybrany.model, which=1) #Residuals vs Fitted
# plot(wybrany.model, which=2) #Normal Q-Q
# plot(wybrany.model, which=3) #Scale Location 
# plot(wybrany.model, which=4) #Cook?s distance
# plot(wybrany.model, which=5) #Residuals vs Leverage
# plot(wybrany.model, which=6) #Cook?s dist vs Leverage

#DOPASOWANIE MODELU: 
wyniki<-summary(wybrany.model)
  
# Wsp??czynnik determinacji:
wyniki$r.squared

# Skorygowany wsp??czynnik determinacji:
wyniki$adj.r.squared

# Kryterium informacyjne Akaike (AIC)
library(stats)
AIC(wybrany.model)
###

#ISTOTO?? PARAMETR?W:
# Test t-Studenta: 
wyniki$coefficients

# Test F: 
wyniki$fstatistic
pValue.F<-pf(wyniki$fstatistic[1], wyniki$fstatistic[2], wyniki$fstatistic[3], lower.tail = FALSE)
pValue.F
# Wyniki tego testu mo?na odczyta? z podsumowania estymacji:
wyniki
###

# 2. WSPӣLINIOWO??: 

library(car)
vif(wybrany.model)
###

# 4. ANALIZA RESZT
# 4.1 Wektor reszt: 
reszty<-residuals(wybrany.model)

# 4.7 Analiza reszt - testy diagnostyczne:

# 7.7.1 Normalno?ci reszt
# Shapiro-Wilka:
shapiro.test(reszty)

# Jarque?Bera
# library(normtest)
# jb.norm.test(reszty)

wybrany.model

# 4.7.2 Testowanie jednorodno?ci wariancji:
#Test Breuscha-Pagana:
library(lmtest)
bptest(wybrany.model)

#Test Goldfelda-Quandta:
install.packages("tseries")
library(tseries)

gqtest(wybrany.model)
gqtest(wybrany.model, order.by=Income,point=0.5)
gqtest(wybrany.model, order.by=Limit,point=0.5)
gqtest(wybrany.model, order.by=Cards,point=0.5)
gqtest(wybrany.model, order.by=Balance,point=0.5)

#Test Harrisona-McCabe:
hmctest(wybrany.model, plot=TRUE)
hmctest(wybrany.model, order.by=Income,point=0.5,plot=TRUE)
hmctest(wybrany.model, order.by=Limit,point=0.5,plot=TRUE)
hmctest(wybrany.model, order.by=Cards,point=0.5, plot=TRUE)
hmctest(wybrany.model, order.by=Balance,point=0.5, plot=TRUE)

par(mfrow=c(2,2))
hmctest(wybrany.model, order.by=Income,point=0.5,plot=TRUE)
hmctest(wybrany.model, order.by=Limit,point=0.5,plot=TRUE)
hmctest(wybrany.model, order.by=Cards,point=0.5, plot=TRUE)
hmctest(wybrany.model, order.by=Balance,point=0.2, plot=TRUE)

par(mfrow=c(1,1))

# 4.7.3 Testowanie niezale?no?ci reszt


# Test Durbina-Watsona 
dwtest(wybrany.model, alternative = "two.sided")
dwtest(wybrany.model, order.by=Income, alternative = "two.sided")
dwtest(wybrany.model, order.by=Limit, alternative = "two.sided")
dwtest(wybrany.model, order.by=Cards, alternative = "two.sided")
dwtest(wybrany.model, order.by=Balance, alternative = "two.sided")

# Test Breuscha-Godfreya 
rzad<-2
bgtest(wybrany.model, order=rzad)
bgtest(wybrany.model, order.by=Income, order=rzad)
bgtest(wybrany.model, order.by=Limit, order=rzad)
bgtest(wybrany.model, order.by=Cards, order=rzad)
bgtest(wybrany.model, order.by=Balance, order=rzad)

# 4.7.3 Testowanie liniowo?ci modelu

# Test Harveya-Colliera
harvtest(wybrany.model)
# Test Rainbow
raintest(wybrany.model,fraction = 0.4)
raintest(wybrany.model,fraction = 0.4, order.by=Income)
raintest(wybrany.model,fraction = 0.4, order.by=Limit)
raintest(wybrany.model,fraction = 0.4, order.by=Cards)
raintest(wybrany.model,fraction = 0.4, order.by=Balance)
# Test Ramseya RESET 
resettest(wybrany.model, power = 2)
resettest(wybrany.model, power = 3)
resettest(wybrany.model, power = 2:5)

###

# Wyznaczanie prognoz dla zbioru testowego:

head(zbior.testowy)
# Tworzymy zbi?r testowy uwzgl?dniaj?cy
prognozy.testowy<-predict(wybrany.model, zbior.testowy)
prognozy.testowy
# Zestawienie pronoz z warto?ciami rzeczywistymi:
zestawienie<-data.frame(Rzeczywiste=zbior.testowy$Rating, Prognozy=prognozy.testowy)
head(zestawienie)

plot(zestawienie$Rzeczywiste, type="b",pch=20, col=3,ylab="Cena")
points(zestawienie$Prognozy,type="b", pch=18, col=2)
legend("top", c("Rzeczywiste", "Prognozy"), pch=c(20,18),lty=1, col=c(3,2))

# ### - możliwe, ze niepotrzebne
# #MAE
# install.packages("Metrics")
# library(Metrics)
# mae <- mae(zestawienie$Rzeczywiste, zestawienie$Prognozy)
# mae
# 
# #RMSE
# rmse <- rmse(zestawienie$Rzeczywiste, zestawienie$Prognozy)
# rmse
# 
# #MPE
# mpe <- mean((zestawienie$Prognozy - zestawienie$Rzeczywiste) / zestawienie$Rzeczywiste) * 100
# mpe
# 
# mape <- mean(abs(zestawienie$Prognozy - zestawienie$Rzeczywiste) / abs(zestawienie$Rzeczywiste)) * 100
# mape
# ###

#B??dy ex post
rzeczywiste.testowy<-zbior.testowy$Rating
library(forecast)
bledy.ex.post<-accuracy(ts(prognozy.testowy),ts(rzeczywiste.testowy))
bledy.ex.post

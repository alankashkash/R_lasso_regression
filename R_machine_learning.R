#Wczytanie danych

#Zbior do modelowania
klienci <- read.table("G:/SGH/Statystyczne reguly decyzyjne/Projekt/klienci.txt", sep = ",", header = T)

#Zbior nowych klientow, dla ktorych podejmiemy decyzjê
klienci_final <- read.table("G:/SGH/Statystyczne reguly decyzyjne/Projekt/klienci_final.txt", sep = ",")

#Eksploracja danych

summary(klienci_final)
head(klienci_final)

#Sprawdzenie, czy wystepuja braki danych
sum(is.na(klienci_final))

#Kodowanie zmiennych
library(fastDummies)
klienci_final <- dummy_cols(klienci_final)
klienci_final <- klienci_final[,-c(1,4,5,7)]

#Liczba klientow w stosunku do ktorych bedziemy podejmowali decyzje
nrow(klienci_final)

summary(klienci)
head(klienci)

#Sprawdzenie, czy wystepuja braki danych
sum(is.na(klienci))

#Odsetek osob w poszczegolnych grupach w zaleznosci od wyslania oferty oraz zakupu
prop.table(table(klienci$wysylka_oferty, klienci$zakup))

#Kodowanie zmiennych
library(fastDummies)
klienci <- dummy_cols(klienci)
klienci <- klienci[,-c(1,4,5,7)]

#Podzial danych na 2 zbiory: z wyslana oferta i bez wyslanej oferty
klienci_1 <- klienci[klienci$wysylka_oferty == 1, ]
klienci_0 <- klienci[klienci$wysylka_oferty == 0, ]

#Licznosc obydwu zbiorow
nrow(klienci_1)
nrow(klienci_0)

#Podzial danych na zbiory treningowe i testowe 
set.seed(1)
set_1 <- sample.int(nrow(klienci_1), floor(0.75*nrow(klienci_1)), replace=F)
set_0 <- sample.int(nrow(klienci_0), floor(0.75*nrow(klienci_0)), replace=F)

klienci_1.train <- klienci_1[set_1,]
klienci_1.test <- klienci_1[-set_1,]

klienci_0.train <- klienci_0[set_0,]
klienci_0.test <- klienci_0[-set_0,]

#Budowa modelu

#Regresja logistyczna

##Model dla klientow, ktorym  wyslano oferte
model_regresja_w <- glm(formula = zakup ~.-wysylka_oferty, family = "binomial", data = klienci_1.train)
summary(model_regresja_w)
step(model_regresja_w, direction = "backward")

#model z najmniejszym AIC
model_1 <- glm(formula = zakup ~ wiek + wyksztalcenie_brak + wyksztalcenie_podstawowe + 
                 wyksztalcenie_policealne + wyksztalcenie_srednie + miejsce_m_500 + 
                 wojewodztwo_opol + wojewodztwo_podkar + wojewodztwo_warmaz, 
               family = "binomial", data = klienci_1.train)

summary(model_1)

#odrzucenie nieistotnych zmiennych na poziomie alfa = 5 %
model1 <- glm(formula = zakup ~ wiek + wyksztalcenie_brak + wyksztalcenie_podstawowe + 
                wyksztalcenie_policealne + wyksztalcenie_srednie + miejsce_m_500, 
              family = "binomial", data = klienci_1.train)

summary(model1)

#badanie wspolliniowosci
library(car)
vif(model1)

#predykcja na zbiorze testowym
predict.model1 <- predict(model1, newdata = klienci_1.test, type='response')
head(predict.model1)

#krzywa ROC dla modelu 1
library(pROC)
roc.model1 <- roc(klienci_1.test$zakup, predict.model1)
plot(roc.model1, main = "Regresja logistyczna - krzywa ROC dla modelu 1")
roc.model1
#AUC = 0.8493

##Model dla klientow, ktorym nie wyslano oferty
model_regresja_nw <- glm(formula = zakup ~.-wysylka_oferty, family = "binomial", data = klienci_0.train)
summary(model_regresja_nw)
step(model_regresja_nw, direction = "backward")

#model z najmniejszym AIC
model_2 <- glm(formula = zakup ~ wiek + wyksztalcenie_brak + wyksztalcenie_podstawowe + 
                 wyksztalcenie_policealne + wyksztalcenie_srednie + miejsce_m100_500 + 
                 wojewodztwo_podkar + wojewodztwo_swiêt + wojewodztwo_warmaz + 
                 wojewodztwo_wielkop, family = "binomial", data = klienci_0.train)
  
summary(model_2)

#odrzucenie nieistotnych zmiennych na poziomie alfa = 5 %
model2 <- glm(formula = zakup ~ wiek + wyksztalcenie_brak + wyksztalcenie_podstawowe + 
                wyksztalcenie_policealne + wyksztalcenie_srednie + 
                wojewodztwo_swiêt, family = "binomial", data = klienci_0.train)

summary(model2)

#badanie wspolliniowosci
library(car)
vif(model2)

#predykcja na zbiorze testowym
predict.model2 <- predict(model2, newdata = klienci_0.test, type='response')
head(predict.model2)

#krzywa ROC dla modelu 2
library(pROC)
roc.model2 <- roc(klienci_0.test$zakup, predict.model2)
plot(roc.model2, main = "Regresja logistyczna - krzywa ROC dla modelu 2")
roc.model2
#AUC = 0.9164

### RANDOM FOREST
library(randomForest)
set.seed(42)

##Model dla klientow, ktorym  wyslano oferte
sapply(klienci_1.train, class)
klienci_1.train <- transform(
  klienci_1.train,
  plec=as.factor(plec),
  kwota=as.integer(kwota),
  wysylka_oferty=as.factor(wysylka_oferty),
  zakup=as.factor(zakup),
  wyksztalcenie_brak=as.factor(wyksztalcenie_brak),
  wyksztalcenie_podstawowe=as.factor(wyksztalcenie_podstawowe),
  wyksztalcenie_policealne=as.factor(wyksztalcenie_policealne),
  wyksztalcenie_srednie=as.factor(wyksztalcenie_srednie),
  wyksztalcenie_wyzsze=as.factor(wyksztalcenie_wyzsze),
  miejsce_m_500=as.factor(miejsce_m_500),
  miejsce_m0_10=as.factor(miejsce_m0_10),
  miejsce_m10_100=as.factor(miejsce_m10_100),
  miejsce_m100_500=as.factor(miejsce_m100_500),
  miejsce_wies=as.factor(miejsce_wies),
  wojewodztwo_dolsl=as.factor(wojewodztwo_dolsl),
  wojewodztwo_kujpom=as.factor(wojewodztwo_kujpom),
  wojewodztwo_lubel=as.factor(wojewodztwo_lubel),
  wojewodztwo_lubus=as.factor(wojewodztwo_lubus),
  wojewodztwo_³ódz=as.factor(wojewodztwo_³ódz),
  wojewodztwo_ma³op=as.factor(wojewodztwo_ma³op),
  wojewodztwo_mazow=as.factor(wojewodztwo_mazow),
  wojewodztwo_opol=as.factor(wojewodztwo_opol),
  wojewodztwo_podkar=as.factor(wojewodztwo_podkar),
  wojewodztwo_podlas=as.factor(wojewodztwo_podlas),
  wojewodztwo_pomo=as.factor(wojewodztwo_pomo),
  wojewodztwo_sl¹s=as.factor(wojewodztwo_sl¹s),
  wojewodztwo_swiêt=as.factor(wojewodztwo_swiêt),
  wojewodztwo_warmaz=as.factor(wojewodztwo_warmaz),
  wojewodztwo_wielkop=as.factor(wojewodztwo_wielkop),
  wojewodztwo_zachpom=as.factor(wojewodztwo_zachpom)
)
sapply(klienci_1.train, class)

model_forest<- randomForest(zakup~.-wysylka_oferty, family = "binomial", data = klienci_1.train)
model_forest

# Predykcja na zbiorze train

pred.Train <- predict(model_forest, klienci_1.train, type="class")
# sprawdzenie classification accuracy
table(pred.Train, klienci_1.train$zakup)

#Predykcja na zbiorze testowym

klienci_1.test <- transform(
  klienci_1.test,
  plec=as.factor(plec),
  kwota=as.integer(kwota),
  wysylka_oferty=as.factor(wysylka_oferty),
  zakup=as.factor(zakup),
  wyksztalcenie_brak=as.factor(wyksztalcenie_brak),
  wyksztalcenie_podstawowe=as.factor(wyksztalcenie_podstawowe),
  wyksztalcenie_policealne=as.factor(wyksztalcenie_policealne),
  wyksztalcenie_srednie=as.factor(wyksztalcenie_srednie),
  wyksztalcenie_wyzsze=as.factor(wyksztalcenie_wyzsze),
  miejsce_m_500=as.factor(miejsce_m_500),
  miejsce_m0_10=as.factor(miejsce_m0_10),
  miejsce_m10_100=as.factor(miejsce_m10_100),
  miejsce_m100_500=as.factor(miejsce_m100_500),
  miejsce_wies=as.factor(miejsce_wies),
  wojewodztwo_dolsl=as.factor(wojewodztwo_dolsl),
  wojewodztwo_kujpom=as.factor(wojewodztwo_kujpom),
  wojewodztwo_lubel=as.factor(wojewodztwo_lubel),
  wojewodztwo_lubus=as.factor(wojewodztwo_lubus),
  wojewodztwo_³ódz=as.factor(wojewodztwo_³ódz),
  wojewodztwo_ma³op=as.factor(wojewodztwo_ma³op),
  wojewodztwo_mazow=as.factor(wojewodztwo_mazow),
  wojewodztwo_opol=as.factor(wojewodztwo_opol),
  wojewodztwo_podkar=as.factor(wojewodztwo_podkar),
  wojewodztwo_podlas=as.factor(wojewodztwo_podlas),
  wojewodztwo_pomo=as.factor(wojewodztwo_pomo),
  wojewodztwo_sl¹s=as.factor(wojewodztwo_sl¹s),
  wojewodztwo_swiêt=as.factor(wojewodztwo_swiêt),
  wojewodztwo_warmaz=as.factor(wojewodztwo_warmaz),
  wojewodztwo_wielkop=as.factor(wojewodztwo_wielkop),
  wojewodztwo_zachpom=as.factor(wojewodztwo_zachpom)
)
sapply(klienci_1.test, class)
pred.Test <- predict(model_forest, klienci_1.test, type = "class")

# Sprawdzenie classification accuracy
mean(pred.Test == klienci_1.test$zakup)                    
table(pred.Test,klienci_1.test$zakup)
pred.Test
importance(model_forest)

##Model dla klientow, ktorym nie wyslano oferty

sapply(klienci_0.train, class)
klienci_0.train <- transform(
  klienci_0.train,
  plec=as.factor(plec),
  kwota=as.integer(kwota),
  wysylka_oferty=as.factor(wysylka_oferty),
  zakup=as.factor(zakup),
  wyksztalcenie_brak=as.factor(wyksztalcenie_brak),
  wyksztalcenie_podstawowe=as.factor(wyksztalcenie_podstawowe),
  wyksztalcenie_policealne=as.factor(wyksztalcenie_policealne),
  wyksztalcenie_srednie=as.factor(wyksztalcenie_srednie),
  wyksztalcenie_wyzsze=as.factor(wyksztalcenie_wyzsze),
  miejsce_m_500=as.factor(miejsce_m_500),
  miejsce_m0_10=as.factor(miejsce_m0_10),
  miejsce_m10_100=as.factor(miejsce_m10_100),
  miejsce_m100_500=as.factor(miejsce_m100_500),
  miejsce_wies=as.factor(miejsce_wies),
  wojewodztwo_dolsl=as.factor(wojewodztwo_dolsl),
  wojewodztwo_kujpom=as.factor(wojewodztwo_kujpom),
  wojewodztwo_lubel=as.factor(wojewodztwo_lubel),
  wojewodztwo_lubus=as.factor(wojewodztwo_lubus),
  wojewodztwo_³ódz=as.factor(wojewodztwo_³ódz),
  wojewodztwo_ma³op=as.factor(wojewodztwo_ma³op),
  wojewodztwo_mazow=as.factor(wojewodztwo_mazow),
  wojewodztwo_opol=as.factor(wojewodztwo_opol),
  wojewodztwo_podkar=as.factor(wojewodztwo_podkar),
  wojewodztwo_podlas=as.factor(wojewodztwo_podlas),
  wojewodztwo_pomo=as.factor(wojewodztwo_pomo),
  wojewodztwo_sl¹s=as.factor(wojewodztwo_sl¹s),
  wojewodztwo_swiêt=as.factor(wojewodztwo_swiêt),
  wojewodztwo_warmaz=as.factor(wojewodztwo_warmaz),
  wojewodztwo_wielkop=as.factor(wojewodztwo_wielkop),
  wojewodztwo_zachpom=as.factor(wojewodztwo_zachpom)
)
sapply(klienci_0.train, class)

model_forest_nw<- randomForest(zakup~.-wysylka_oferty, family = "binomial", data = klienci_0.train)
model_forest_nw

# Predykcja na zbiorze train
pred.Train_nw <- predict(model_forest_nw, klienci_0.train, type="class")
# Sprawdzenie classification accuracy
table(pred.Train_nw, klienci_0.train$zakup)
# redykcja na zbiorze testowym
klienci_0.test <- transform(
  klienci_0.test,
  plec=as.factor(plec),
  kwota=as.integer(kwota),
  wysylka_oferty=as.factor(wysylka_oferty),
  zakup=as.factor(zakup),
  wyksztalcenie_brak=as.factor(wyksztalcenie_brak),
  wyksztalcenie_podstawowe=as.factor(wyksztalcenie_podstawowe),
  wyksztalcenie_policealne=as.factor(wyksztalcenie_policealne),
  wyksztalcenie_srednie=as.factor(wyksztalcenie_srednie),
  wyksztalcenie_wyzsze=as.factor(wyksztalcenie_wyzsze),
  miejsce_m_500=as.factor(miejsce_m_500),
  miejsce_m0_10=as.factor(miejsce_m0_10),
  miejsce_m10_100=as.factor(miejsce_m10_100),
  miejsce_m100_500=as.factor(miejsce_m100_500),
  miejsce_wies=as.factor(miejsce_wies),
  wojewodztwo_dolsl=as.factor(wojewodztwo_dolsl),
  wojewodztwo_kujpom=as.factor(wojewodztwo_kujpom),
  wojewodztwo_lubel=as.factor(wojewodztwo_lubel),
  wojewodztwo_lubus=as.factor(wojewodztwo_lubus),
  wojewodztwo_³ódz=as.factor(wojewodztwo_³ódz),
  wojewodztwo_ma³op=as.factor(wojewodztwo_ma³op),
  wojewodztwo_mazow=as.factor(wojewodztwo_mazow),
  wojewodztwo_opol=as.factor(wojewodztwo_opol),
  wojewodztwo_podkar=as.factor(wojewodztwo_podkar),
  wojewodztwo_podlas=as.factor(wojewodztwo_podlas),
  wojewodztwo_pomo=as.factor(wojewodztwo_pomo),
  wojewodztwo_sl¹s=as.factor(wojewodztwo_sl¹s),
  wojewodztwo_swiêt=as.factor(wojewodztwo_swiêt),
  wojewodztwo_warmaz=as.factor(wojewodztwo_warmaz),
  wojewodztwo_wielkop=as.factor(wojewodztwo_wielkop),
  wojewodztwo_zachpom=as.factor(wojewodztwo_zachpom)
)
sapply(klienci_0.test, class)
pred.Test_nw <- predict(model_forest_nw, klienci_0.test, type = "class")
# Sprawdzenie classification accuracy
mean(pred.Test_nw == klienci_0.test$zakup)                    
table(pred.Test_nw,klienci_0.test$zakup)
pred.Test_nw
importance(model_forest_nw)

#aplikacja instrukcji decyzyjnej

finaltest <- klienci_final

prawdopodob.badaw <- predict(model1, newdata = finaltest)
length(prawdopodob.badaw)

prawdopodob.kontrol <- predict(model2, newdata = finaltest)
length(prawdopodob.kontrol)

roznica <- prawdopodob.badaw - prawdopodob.kontrol
hist(roznica)
abline(v = 0.1, col = "red", lwd = 4, lty = 2)

decyzja <- ifelse(roznica > 0.1, 1, 0)
head(decyzja)

prawdopodob.badaw2 <- predict(model_forest, newdata = finaltest)
length(prawdopodob.badaw2)

prawdopodob.kontro2 <- predict(model_forest_nw, newdata = finaltest)
length(prawdopodob.kontrol2)

roznica2 <- prawdopodob.badaw2 - prawdopodob.kontrol2
hist(roznica2)
abline(v = 0.1, col = "red", lwd = 4, lty = 2)

decyzja2 <- ifelse(roznica > 0.1, 1, 0)
head(decyzja2)

roznica_final <- decyzja - decyzja2
hist(roznica_final)
abline(v = 0.1, col = "red", lwd = 4, lty = 2)

decyzja_final <- ifelse(roznica_final > 0.1, 1, 0)
head(decyzja_final)

write.table(decyzja_final,"G:/SGH/Statystyczne reguly decyzyjne/Projekt/output_sample2.txt", sep = ",")


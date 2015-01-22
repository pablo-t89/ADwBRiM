# Zadanie 6.1
dane = read.csv2("Wzrost_zal.csv")
head(dane)

# Operator ":" sprawia problemy przy rzeczach porzebnych do wykresow, wiec tym razem
# mozna sobie zrobic samemu wektor kolorow oczu-i-wlosow naraz.
kw = length(levels(dane$kolor.w這s闚)) # Tyle wystepuje roznych kolorow wlosow (= 2)
# Funkcja c(factor) faje wektor numerow etykier, wiec po zrobieniu tego mozna dzialac
# arytmetycznie na etykietach factora.
kolory = c(dane$kolor.w這s闚) + (c(dane$kolor.oczu) - 1) * kw
kolory = factor(kolory) # Robimy z powrotem factor
levels(kolory) = c("bc", "bj", "cc", "cj", "nc", "nj", "zc", "zj") # Nadajemy etykiety
kolory

# Normalnosc w grupach...
op = par(mfrow=c(2,4))
qqnorm(dane$wzrost[kolory=="bc"], main="bc")
qqline(dane$wzrost[kolory=="bc"], lwd=2.5, col="brown")
qqnorm(dane$wzrost[kolory=="cc"], main="cc")
qqline(dane$wzrost[kolory=="cc"], lwd=2.5, col="black")
qqnorm(dane$wzrost[kolory=="nc"], main="nc")
qqline(dane$wzrost[kolory=="nc"], lwd=2.5, col="blue")
qqnorm(dane$wzrost[kolory=="zc"], main="zc")
qqline(dane$wzrost[kolory=="zc"], lwd=2.5, col="darkgreen")
qqnorm(dane$wzrost[kolory=="bj"], main="bj")
qqline(dane$wzrost[kolory=="bj"], lwd=2.5, col="peru")
qqnorm(dane$wzrost[kolory=="cj"], main="cj")
qqline(dane$wzrost[kolory=="cj"], lwd=2.5, col="gray")
qqnorm(dane$wzrost[kolory=="nj"], main="nj")
qqline(dane$wzrost[kolory=="nj"], lwd=2.5, col="cyan")
qqnorm(dane$wzrost[kolory=="zj"], main="zj")
qqline(dane$wzrost[kolory=="zj"], lwd=2.5, col="green")
par(op)

# Nie wyglada zle, ciekawe co na to Shapiro-Wilk
shapiro.test(dane$wzrost[kolory=="bc"]) # Tak!
shapiro.test(dane$wzrost[kolory=="cc"]) # Tak!
shapiro.test(dane$wzrost[kolory=="nc"]) # Tak!
shapiro.test(dane$wzrost[kolory=="zc"]) # Tak!
shapiro.test(dane$wzrost[kolory=="bj"]) # Tak!
shapiro.test(dane$wzrost[kolory=="cj"]) # Tak!
shapiro.test(dane$wzrost[kolory=="nj"]) # Nie...
shapiro.test(dane$wzrost[kolory=="zj"]) # Tak!

# Tylko w jednej grupie nie wyszlo i to tez nie jakos tragicznie (np. jakby to
# porownac z zadaniami 5.*). Mozna mowic o normalnosci w grupach. Grupy nie sa jednak
# rownoliczne...

anova(lm(dane$wzrost ~ kolory))

# Odrzucamy hipoteze, jakoby istniala zaleznosc miedzy kolorami oczy/wlosow
# a wzrostem. Czy ANOVIE dla grup, ktore nie sa rownoliczne mozna jednak ufac?
# Powiedzialbym, ze taki test budzi jednak pewne watpliwosci.

# Wykres ramkowy
boxplot(dane$wzrost ~ kolory, col=heat.colors(8))
# Srednie arytmetyczne (na samym boxplot widac mediany)
points(1:8, by(dane$wzrost, kolory, mean), pch=16, cex=1.5)

# Dwa wykresy interakcji
op = par(mfrow=c(1,2))
interaction.plot(dane$kolor.oczu, dane$kolor.w這s闚, dane$wzrost)
interaction.plot(dane$kolor.w這s闚, dane$kolor.oczu, dane$wzrost)
par(op)

# Z ANOVY wyszedl nam brak zwiazku, mimo ze na tych wykresach niby cos widac
# (np. ze w grupie "cc" jest wyjatkowo duzy sredni wzrost). Ciekawe co wyjdzie
# w kolejnym zadaniu -- moze to przez nierowne grupy?


# Zadanie 6.2

# Funkcja mieszajaca wiersze ramki
mieszaj = function(ramka) {
    ramka[sample(nrow(ramka)),]
};

(n = min(table(kolory))) # Do tylu bedziemy chcieli obciac licznosc kazdej grupy

# Ramka rdane -- wyrownane dane
rdane = mieszaj(dane[kolory=="bc",])[1:n,]
rdane = rbind(rdane, mieszaj(dane[kolory=="bj",])[1:n,])
rdane = rbind(rdane, mieszaj(dane[kolory=="cc",])[1:n,])
rdane = rbind(rdane, mieszaj(dane[kolory=="cj",])[1:n,])
rdane = rbind(rdane, mieszaj(dane[kolory=="nc",])[1:n,])
rdane = rbind(rdane, mieszaj(dane[kolory=="nj",])[1:n,])
rdane = rbind(rdane, mieszaj(dane[kolory=="zc",])[1:n,])
rdane = rbind(rdane, mieszaj(dane[kolory=="zj",])[1:n,])

# Odpowiednik zmiennej kolory dla ramki rdane -- wybieramy ze zmiennej kolory
# elementy o odpowiednich numerkach
rkolory = kolory[rdane$Lp]

# Ciekawe, czy normalnosc w grupach sie nam nie popsula
op = par(mfrow=c(2,4))
qqnorm(rdane$wzrost[rkolory=="bc"], main="bc")
qqline(rdane$wzrost[rkolory=="bc"], lwd=2.5, col="brown")
qqnorm(rdane$wzrost[rkolory=="cc"], main="cc")
qqline(rdane$wzrost[rkolory=="cc"], lwd=2.5, col="black")
qqnorm(rdane$wzrost[rkolory=="nc"], main="nc")
qqline(rdane$wzrost[rkolory=="nc"], lwd=2.5, col="blue")
qqnorm(rdane$wzrost[rkolory=="zc"], main="zc")
qqline(rdane$wzrost[rkolory=="zc"], lwd=2.5, col="darkgreen")
qqnorm(rdane$wzrost[rkolory=="bj"], main="bj")
qqline(rdane$wzrost[rkolory=="bj"], lwd=2.5, col="peru")
qqnorm(rdane$wzrost[rkolory=="cj"], main="cj")
qqline(rdane$wzrost[rkolory=="cj"], lwd=2.5, col="gray")
qqnorm(rdane$wzrost[rkolory=="nj"], main="nj")
qqline(rdane$wzrost[rkolory=="nj"], lwd=2.5, col="cyan")
qqnorm(rdane$wzrost[rkolory=="zj"], main="zj")
qqline(rdane$wzrost[rkolory=="zj"], lwd=2.5, col="green")
par(op)

shapiro.test(rdane$wzrost[rkolory=="bc"])
shapiro.test(rdane$wzrost[rkolory=="cc"])
shapiro.test(rdane$wzrost[rkolory=="nc"])
shapiro.test(rdane$wzrost[rkolory=="zc"])
shapiro.test(rdane$wzrost[rkolory=="bj"])
shapiro.test(rdane$wzrost[rkolory=="cj"])
shapiro.test(rdane$wzrost[rkolory=="nj"])
shapiro.test(rdane$wzrost[rkolory=="zj"])

# Przynajmniej mi sie tak wylosowalo, ze akurat nadal w grupie o jasnych wlosach
# i niebieskich oczach nie ma normalnosci. Wykresy jak wykresy, o 5 punktach
# trudno cos madrego powiedziec.

anova(lm(rdane$wzrost ~ rkolory))

# No i znowu mamy podstawy do odrzucenia hipotezy, jakoby byl jakis zwiazek miedzy
# wzrostem a kolorami oczu/wlosow.
# Popatrzmy na reszte wymaganych wykresow...

# Wykres ramkowy
boxplot(rdane$wzrost ~ rkolory, col=heat.colors(8))
points(1:8, by(rdane$wzrost, rkolory, mean), pch=16, cex=1.5)

# Wykresy interakcji
op = par(mfrow=c(1,2))
interaction.plot(rdane$kolor.oczu, rdane$kolor.w這s闚, rdane$wzrost)
interaction.plot(rdane$kolor.w這s闚, rdane$kolor.oczu, rdane$wzrost)
par(op)

# Niby te srednie sie roznia, ale trudno tutaj wysnuc jakies sensowne spostrzezenia
# z wykresow interakcji. Wykres ramkowy pokazuje, ze mimo roznic w srednich
# i medianach te przedzialy sie w miare nachodza.
# Chyba ANOVA ma jednak racje i zwiazek mozna odrzucic.

# 
# Ciekawostka: puscilem wybieranie danych do rdane i ANOVE jeszcze kilka razy i czasem
# jednak wychodzi zwiazek. Ale przewaznie jednak nie. Te dane sa zwyczajnie za male,
# zeby wyniki testow traktowac powaznie.
# 


# Zadanie 6.3
cw = read.csv2("czas_wykonania.csv")
head(cw)

kwpl = cw$kwalifikacje + (c(cw$plec) - 1) * 3 # bo kwalifikacje to liczba 1..3
kwpl = factor(kwpl)
levels(kwpl) = c("K1", "K2", "K3", "M1", "M2", "M3")
kwpl

# Serio, grupy maja po 3 elementy. Badanie normalnosci w grupach to bylby wyglup.

anova(lm(cw$czas ~ kwpl))
# No, ANOVA dla tak malych danych normalnie nie jest zbyt wiarygodna, ale tutaj
# wyszlo nam, ze zwiazek zachodzi i to z poziomem ufnosci 99.9% (trzy gwiazdki).
# Zdecydowanie, brak podstaw do odrzucenia hipotezy o tym, ze wystepuje zwiazek miedzy
# czasem pracy a plcia+wyksztalceniem.

# Wykres ramkowy
boxplot(cw$czas ~ kwpl, col=heat.colors(6))
points(1:6, by(cw$czas, kwpl, mean), pch=16, cex=1.5)

# Interpretacja: te przedzialy nawet sie nie nachodza, tutaj chyba naprawde jest
# zwiazek.

# Wykresy interakcji
op = par(mfrow=c(1,2))
interaction.plot(cw$plec, cw$kwalifikacje, cw$czas)
interaction.plot(cw$kwalifikacje, cw$plec, cw$czas)
par(op)

# Wszystkie linie maja te sama monotonicznosc, przedzialy nie nachodza na siebie
# -- zatem mozna mowic o wyraznym zwiazku.




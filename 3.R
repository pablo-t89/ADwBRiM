# Zadanie 3.1
sluby = read.csv2("Sluby.csv")
head(sluby)

(boxplot(sluby$ZONA ~ sluby$MAZ, notch=TRUE))

(sz = boxplot.stats(sluby$ZONA))
(sm = boxplot.stats(sluby$MAZ))

# Czy 25 miesci sie w przedziale ufnosci na poziomie 95% dla mediany wieku zony?
sz$conf[1] <= 25 && 25 <= sz$conf[2]

# Czy 25 miesci sie w przedziale ufnosci na poziomie 95% dla mediany wieku meza?
sm$conf[1] <= 25 && 25 <= sm$conf[2]


# Zadanie 3.2
wzrost = read.csv2("Wzrost.csv", header=FALSE)[,1]
hist(wzrost, breaks=14, freq=FALSE, col="yellow") # Histogram
lines(density(wzrost), lwd=2.5, col="red") # Gestosc
x = seq(min(wzrost), max(wzrost), length=1000)
# 1000 rownomiernie rozlozonych punktow
# Wartosci funkcji gestosci dla rozkladu normalnego o odpowiednich parametrach
# mozna policzyc za pomoca dnorm.
lines(x, dnorm(x, mean(wzrost), sd(wzrost)), type="l", lwd=2.5, col="blue") 


# Zadanie 3.3

# 15 klas, rowne przedzialy, min jest srodkiem pierwszego, a max -- ostatniego
k = 15
rozstep = (max(wzrost) - min(wzrost)) / (k - 1)
podzial = rozstep * (0:k-0.5) + min(wzrost)
table(cut(wzrost, podzial))

# 6 klas, mniej-wiecej rowne licznosci
k = 6
podzial = quantile(wzrost, c(0:k/k))
table(cut(wzrost, podzial, include.lowest=TRUE))
hist(wzrost, podzial, freq=FALSE, col="yellow")


# Zadanie 3.4
(rdn = read.csv2("RDN_2010.csv"))

# Jak widac, na koncu sa jakies smieci -- pozbadzmy sie ich
rdn = rdn[1:365,]
tail(rdn) # Teraz lepiej!

op = par(mfrow=c(1,2)) # Jeden wiersz z dwoma wykresami

(sredniewgdni = aggregate(rdn$Przesy³8.22 ~ rdn$Dzieñ.Tyg, rdn, mean))
barplot(sredniewgdni[,2], names=c("pon", "wto", "sro", "czw", "pia", "sob", "nie"))

swieta = c(
    "2010-01-01", # Nowy Rok
    "2010-04-04", # Niedziela Wielkanocna
    "2010-04-05", # Poniedzialek Wielkanocny
    "2010-05-01", # Pierwszy maja
    "2010-05-03", # Swieto Konstytucji 3 maja
    "2010-06-03", # Boze Cialo
    "2010-08-15", # Wniebowziecie NMP
    "2010-11-01", # Wszystkich Swietych
    "2010-11-11", # Swieto Niepodleglosci
    "2010-12-25", # Boze Narodzenie
    "2010-12-26"  # Drugi dzien Swiat Bozego Narodzenia
)

poprawionedni = rdn$Dzieñ.Tyg
poprawionedni[is.element(rdn$Data, swieta)] = NA

(sredniewgpdni = aggregate(rdn$Przesy³8.22 ~ poprawionedni, rdn, mean))
barplot(sredniewgpdni[,2], names=c("pon", "wto", "sro", "czw", "pia", "sob", "nie"))
# Faktycznie, po usunieciu swiat w 2010 roku wyszly sensowniejsze poniedzialki

par(op) # Na koniec elegancko jest przywrocic domyslne ustawienia wykresow.


# Zadanie K.1.1
head(iris)

# Z czego liczyc te stednie i wariancje? Wybralem dlugosc lisci kielicha kwiatu,
# bo to pierwsza kolumna; mozna w tej linijce wpisac cos innego.
kolumna = iris$Sepal.Length

# Przydatne funkcje
srednia = mean
srednia_kwadratow = function(x) { mean(x^2) };
wariancja = function(x) { srednia_kwadratow(x) - srednia(x)^2 };
srednia_i_wariancja = function(x) {
    c("Srednia"=srednia(x),
      "Wariancja"=wariancja(x)
    )
};

# Srednia i wariancja wzgledem gatunku
tapply(kolumna, iris$Species, srednia_i_wariancja)

# Ocena normalnosci w grupach na podstawie wykresow
op = par(mfrow=c(1,3))
qqnorm(kolumna[iris$Species=="setosa"], main="setosa")
qqline(kolumna[iris$Species=="setosa"], lwd=2.5, col="purple")
qqnorm(kolumna[iris$Species=="versicolor"], main="versicolor")
qqline(kolumna[iris$Species=="versicolor"], lwd=2.5, col="violet")
qqnorm(kolumna[iris$Species=="virginica"], main="virginica")
qqline(kolumna[iris$Species=="virginica"], lwd=2.5, col="magenta")
par(op)
# Wyglada na dosc dobre dopasowanie, ale zrobimy tez Shapiro-Wilka, dla pewnosci
shapiro.test(kolumna[iris$Species=="setosa"]) # Brak podstaw do odrzucenia hipotezy
shapiro.test(kolumna[iris$Species=="versicolor"]) # j.w.
shapiro.test(kolumna[iris$Species=="virginica"]) # j.w.
# Brak podstaw do odrzucenia hipotezy o tym, ze kazda z grup jest normalna

# h0: roznice srednich w grupach sa istotne statystyczne
anova(lm(kolumna ~ iris$Species))
# Pr(>F) mniejsze od 0.05, wiec z poziomem ufnosci 95% stwierdzamy brak podstaw do
# odrzucenia h0.
# 
# Zrodlo: http://xkcd.com/882/


# Zadanie K.1.2

dlugosci = iris$Petal.Length[iris$Species=="setosa"]

op = par(mfrow=c(1,2))

# Jadrowy estymator gestosci
hist(dlugosci, freq=FALSE, col="yellow")
lines(density(dlugosci), lwd=2.5, col="red")

# Wykres kwantyl-kwantyl (qq)
qqnorm(dlugosci)
qqline(dlugosci, lwd=2.5, col="purple")

par(op)

# Test normalnosci probki
shapiro.test(dlugosci)
# Wyszlo p-value powyzej 0.05, czyli brak podstaw do odrzucenia hipotezy o normalnosci
# probki. Ale dla malej probki to nie jest najmarzejszy test, lepiej badac to przez
# zgodnosc (testem chi-kwadrat).

# Zrobie podzial na 5 klas (bo to w sam raz tak okolo 0.75*sqrt(50) w taki sposob,
# aby byly mniej-wiecej rownloiczne i zrobie test zgodnosci chi-kwadrat z rozkladem
# normalnym.
k = 5
(podzial = quantile(dlugosci, c(0:k/k), names=FALSE))

# Nieograniczony podzial
npodzial = podzial
npodzial[1] = -Inf
npodzial[k+1] = +Inf
npodzial

(szereg = table(cut(dlugosci, npodzial)))

# Estymowane parametry rozkladu normalnego
mi = mean(dlugosci)
sigma = sd(dlugosci)

# Wektor opisujacy pstwa, z jakimi proba z przedzialu normalnego o parametrach
# (mi, sigma) znajdzie sie w poszczegolnych przedzialach
pstwa = pnorm(npodzial[2:(k+1)], mi, sigma) - pnorm(npodzial[1:k], mi, sigma)

# Test chi-kwadrat -- p-value bedzie zle, bo estymujac mi i sigme zmienilismy liczbe
# stopni swobody (chcemy k-3, a to nam liczy dla k-1), ale chociaz policzy nam
# statystyke.
(t = chisq.test(szereg, p=pstwa))
# Interesujaca nas p-wartosc
1 - pchisq(t$statistic, k-3)
# Wyszlo ponad 0.05, wiec z poziomem ufnosci 95% mamy brak podstaw do odrzucenia
# hipotezy o zgodnosci wygenerowanego szeregu rozdzielczego z rozkladem normalnym.


# Zadanie K.1.3
poeci = read.csv2("Poeci.csv", header=F, col.names=c("Miasto", "Poeta"))
head(poeci)

# Aby zaobserwowac pewne zaleznosci, wystarczy spojrzec na tabele (no wyraznie sa)
table(poeci)

# Wersja procentowa
table(poeci) / nrow(poeci) * 100

# Test zgodnosci miedzy miastem a poeta
chisq.test(table(poeci))
# Nie estymowalismy dodatkowo zadnych parametrow, wiec wynik chisq.test jest dokladnie
# tym, co nas interesuje. Osiagnieto p-value zdecydowanie mniejsze od 0.05, wiec
# sa podstawy do odrzucenia hipotezy o zgodnosci rozkladu poetow z rozkladem miast.
# Mozna to zinterpretowac wlasnie tak, ze wynik testu odzwierciedla fakt, ze w roznych
# miastach lubiani sa rozni poeci, zatem zaleznosc jest istotna statystycznie.


# Zadanie K.1.1
head(iris)

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
tapply(iris$Petal.Length, iris$Species, srednia_i_wariancja) # dlugosc
tapply(iris$Petal.Width, iris$Species, srednia_i_wariancja) # szerokosc

# Ocena normalnosci w grupach na podstawie wykresow
op = par(mfrow=c(2,3))
qqnorm(iris$Petal.Length[iris$Species=="setosa"], main="setosa (dlugosc)")
qqline(iris$Petal.Length[iris$Species=="setosa"], lwd=2.5, col="purple")
qqnorm(iris$Petal.Length[iris$Species=="versicolor"], main="versicolor (dlugosc)")
qqline(iris$Petal.Length[iris$Species=="versicolor"], lwd=2.5, col="violet")
qqnorm(iris$Petal.Length[iris$Species=="virginica"], main="virginica (dlugosc)")
qqline(iris$Petal.Length[iris$Species=="virginica"], lwd=2.5, col="magenta")
qqnorm(iris$Petal.Width[iris$Species=="setosa"], main="setosa (szerokosc)")
qqline(iris$Petal.Width[iris$Species=="setosa"], lwd=2.5, col="purple")
qqnorm(iris$Petal.Width[iris$Species=="versicolor"], main="versicolor (szerokosc)")
qqline(iris$Petal.Width[iris$Species=="versicolor"], lwd=2.5, col="violet")
qqnorm(iris$Petal.Width[iris$Species=="virginica"], main="virginica (szerokosc)")
qqline(iris$Petal.Width[iris$Species=="virginica"], lwd=2.5, col="magenta")
par(op)
# Wyglada na dosc dobre dopasowanie, poza tym ze szerokosci sa male i przyjmuja dosc
# malo mozliwych wartosci. Wykonamy zatem tez testy Shapiro-Wilka.
shapiro.test(iris$Petal.Length[iris$Species=="setosa"]) # Brak podst. do odrzucenia
shapiro.test(iris$Petal.Length[iris$Species=="versicolor"]) # Brak podst. do odrzucenia
shapiro.test(iris$Petal.Length[iris$Species=="virginica"]) # Brak podst. do odrzucenia
shapiro.test(iris$Petal.Width[iris$Species=="setosa"]) # Sa podst. do odrzucenia
shapiro.test(iris$Petal.Width[iris$Species=="versicolor"]) # Sa podst. do odrzucenia
shapiro.test(iris$Petal.Width[iris$Species=="virginica"]) # Brak podst. do odrzucenia
# Dlugosci sa normalne, ale szerokosci niekoniecznie. Po wykresach kwantyl-kwantyl
# mozna jednak wnioskowac, ze przyczyna jest zbyt mala dokladnosc pomiaru szerokosci
# (liczba mozliwych wartosci, jaka moze przyjmowac szerokosc jest zbyt mala.

# h0: roznice srednich w grupach sa istotne statystyczne
anova(lm(iris$Petal.Length ~ iris$Species))
anova(lm(iris$Petal.Width ~ iris$Species))
# Pr(>F) mniejsze od 0.05 (dla obu wymiarow), wiec z poziomem ufnosci 95% stwierdzamy
# brak podstaw do odrzucenia h0. Dla dlugosci platka jest to dosc bezpieczne, lecz
# szerokosci platkow nie sa normalne w grupach, wiec wynik nie jest pewny.
# 
# Zrodlo: http://xkcd.com/882/


# Zadanie K.1.2

dlugosci = iris$Sepal.Length[iris$Species=="setosa"]

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
# probki. Zwazywszy, ze jest to mala probka, test Shapiro-Wikla jest bardziej stosowny
# niz badanie zgodnosci z rozkladem normalnym za pomoca testu zgodnosci chi kwadrat (!).
# (Podziekowania za wsparcie merytoryczne dla pewnych PN, AK i JS).


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


# Zadanie K.2.1
# Dla danych z pliku dane_13_1.csv, opisujacych decyzje o wyborze ulubionego sklepu
# wielkopowierzchnowego, zbadac, czy istnieje zaleznosc miedzy wyborem sklepu
# a wiekiem klienta.

sklepy = read.csv2("dane_13_1.csv")
head(sklepy)

# Informacja o wybranym sklepie jest typowo nominalna, a wiek mamy juz podzielony na
# dosc szerokie przedzialy. Mozemy to zatem robic tak samo jak zadanie K.1.3.
table(sklepy$wiek, sklepy$sklep)

# Takie samo zestawienie, tyle ze z wartosciami procentowymi.
table(sklepy$wiek, sklepy$sklep) / nrow(sklepy) * 100

# Test zgodnosci miedzy wiekiem a sklepem
chisq.test(table(sklepy$wiek, sklepy$sklep), simulate.p.value=TRUE)
# Zrodlo: http://stats.stackexchange.com/questions/81483/

# Wynik testu chi-kwadrat przekroczyl 0.05, wiec mamy brak podstaw do odrzucenia
# hipotezy o zgodnosci wieku klienta od marki wybranego sklepy wielkopowierzchniowego.
# Skoro rozklady sa zgodne, to znaczy ze jedno NIE ZALEZY od drugiego.

# Zauwazmy, ze wiek mozna potraktowac jako dana numeryczna
levels(sklepy$wiek) # Sprawdzamy, jakie sa wartosci
wiek_liczbowo = c(20, 27.5, 35, 45, 55, 65)
wektor_wieku = wiek_liczbowo[sklepy$wiek]

# Najpierw na to po prostu popatrzmy wykresami...

# Wykres ramkowy
boxplot(wektor_wieku ~ sklepy$sklep, col=heat.colors(9))
# Srednie arytmetyczne (na samym boxplot widac mediany)
points(1:9, by(wektor_wieku, sklepy$sklep, mean), pch=16, cex=1.5)

# Zeby uzyc ANOVY, wypada ocenic normalnosc w grupach.
# Sprawdzmy, czy wektor_wieku jest normalny w grupach zwiazanych z roznymi sklepami
# To sa male proby, test Shapiro-Wikla w sam raz
# Jest ich az 9, wykresy kwantyl-kwantyl raczej sobie odpuszcze (oczywiscie mozna ;) )
shapiro.test(wektor_wieku[sklepy$sklep=="Ach"]) # Podstawy do odrzucenia normalnosci
shapiro.test(wektor_wieku[sklepy$sklep=="Alb"]) # Podstawy do odrzucenia normalnosci
shapiro.test(wektor_wieku[sklepy$sklep=="Bdr"]) # Podstawy do odrzucenia normalnosci
shapiro.test(wektor_wieku[sklepy$sklep=="Crr"]) # Brak podstaw do odrzucenia
shapiro.test(wektor_wieku[sklepy$sklep=="Gnt"]) # Podstawy do odrzucenia normalnosci
shapiro.test(wektor_wieku[sklepy$sklep=="Hyp"]) # Brak podstaw do odrzucenia
shapiro.test(wektor_wieku[sklepy$sklep=="LdP"]) # Brak podstaw do odrzucenia
shapiro.test(wektor_wieku[sklepy$sklep=="Rel"]) # Podstawy do odrzucenia normalnosci
shapiro.test(wektor_wieku[sklepy$sklep=="inn"]) # Podstawy do odrzucenia normalnosci
# Testy mogly nie wyjsc dlatego, ze liczba roznych wartosci jakie przyjmuje wiek
# jest bardzo ograniczona. Mozna domniemywac, ze w tym wypadku wcale nie trzeba sie
# tym przejmowac.

# Slabo z ta normalnoscia w grupach, ANOVA nie bedzie zbyt wiarygodna. Ale i tak mozna
# ja przeprowadzic, zawsze jest to jakas przeslanka.
anova(lm(wektor_wieku ~ sklepy$sklep))
# Wyszlo Pr(>F) wieksze niz 0.05, wiec roznica miedzy grupami nie jest istotna
# statystycznie.

# Z obu zastosowanych metod wynika, ze wybrany sklep nie zalezy od wieku klienta.


# Zadanie K.2.2
# Dla danych iris (dostepne bezposrednio w R) policzyc srednia i wariancje z rozmiarow
# kielicha (Sepal) z podzialem na gatunki kwiatow (Species). Czy srednie rozmiary
# kielichow irysow roznych gatunkow roznia sie w sposob istotny statystycznie?

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
tapply(iris$Sepal.Length, iris$Species, srednia_i_wariancja) # dlugosc
tapply(iris$Sepal.Width, iris$Species, srednia_i_wariancja) # szerokosc

# Ocena normalnosci w grupach na podstawie wykresow
op = par(mfrow=c(2,3))
qqnorm(iris$Sepal.Length[iris$Species=="setosa"], main="setosa (dlugosc)")
qqline(iris$Sepal.Length[iris$Species=="setosa"], lwd=2.5, col="purple")
qqnorm(iris$Sepal.Length[iris$Species=="versicolor"], main="versicolor (dlugosc)")
qqline(iris$Sepal.Length[iris$Species=="versicolor"], lwd=2.5, col="violet")
qqnorm(iris$Sepal.Length[iris$Species=="virginica"], main="virginica (dlugosc)")
qqline(iris$Sepal.Length[iris$Species=="virginica"], lwd=2.5, col="magenta")
qqnorm(iris$Sepal.Width[iris$Species=="setosa"], main="setosa (szerokosc)")
qqline(iris$Sepal.Width[iris$Species=="setosa"], lwd=2.5, col="purple")
qqnorm(iris$Sepal.Width[iris$Species=="versicolor"], main="versicolor (szerokosc)")
qqline(iris$Sepal.Width[iris$Species=="versicolor"], lwd=2.5, col="violet")
qqnorm(iris$Sepal.Width[iris$Species=="virginica"], main="virginica (szerokosc)")
qqline(iris$Sepal.Width[iris$Species=="virginica"], lwd=2.5, col="magenta")
par(op)
# Wyglada na dosc dobre dopasowanie, ale zrobimy tez Shapiro-Wilka, dla pewnosci
shapiro.test(iris$Sepal.Length[iris$Species=="setosa"]) # Brak podst. do odrzucenia
shapiro.test(iris$Sepal.Length[iris$Species=="versicolor"]) # j.w.
shapiro.test(iris$Sepal.Length[iris$Species=="virginica"]) # j.w.
shapiro.test(iris$Sepal.Width[iris$Species=="setosa"]) # j.w.
shapiro.test(iris$Sepal.Width[iris$Species=="versicolor"]) # j.w.
shapiro.test(iris$Sepal.Width[iris$Species=="virginica"]) # j.w.
# Brak podstaw do odrzucenia hipotezy o tym, ze kazda z grup jest normalna (pod
# wzgledem obu wymiarow kielicha)

# h0: roznice srednich w grupach sa istotne statystyczne
anova(lm(iris$Sepal.Length ~ iris$Species))
anova(lm(iris$Sepal.Width ~ iris$Species))
# Pr(>F) mniejsze od 0.05 (dla obu wymiarow), wiec z poziomem ufnosci 95% stwierdzamy
# brak podstaw do odrzucenia h0.


# Zadanie K.2.3
# Dla danych iris wykonac model regresji liniowej opisujacy szerokosc kielicha
# (Sepal.Width) w zaleznosci od jego dlugosci (Sepal.Length) dla gatunkow (Species)
# setosa i versicolor. Dokonac oceny modelu regresji i sprawdzic zalozenia do tej oceny
# konieczne.

# Setosa
se = iris[iris$Species=="setosa",]

model_se = lm(Sepal.Width ~ Sepal.Length, data=se)
summary(model_se) # Ocena modelu regresji
# R^2 i skorygowane R^2 rzedu 0.55; czyli cos sie udalo dopasowac, ale punkty
# nie ukladaja sie jakos doslownie na wyznaczonej linii (do 1 jeszcze troche brakuje).
# (Propsy dla TJ.)

# Skrajne punkty
min_se = min(se$Sepal.Length)
max_se = max(se$Sepal.Length)
skrajne_se = se[se$Sepal.Length==min_se | se$Sepal.Length==max_se,]

# Wykres z punktami empirycznymi
plot(se$Sepal.Length, se$Sepal.Width)
# Dorysowanie linii regresji do wykresu
lines(skrajne_se$Sepal.Length, predict(model_se, skrajne_se), lwd=2.5, col="purple")

# Versicolor
ve = iris[iris$Species=="versicolor",]

model_ve = lm(Sepal.Width ~ Sepal.Length, data=ve)
summary(model_ve) # Ocena modelu regresji
# R^2 i skorygowane R^2 rzedu 0.27; zatem rozbieznosc punktow empirycznych od
# wyznaczonej linii mozna okreslic jako znaczna.

# Skrajne punkty
min_ve = min(ve$Sepal.Length)
max_ve = max(ve$Sepal.Length)
skrajne_ve = ve[ve$Sepal.Length==min_ve | ve$Sepal.Length==max_ve,]

# Wykres z punktami empirycznymi
plot(ve$Sepal.Length, ve$Sepal.Width)
# Dorysowanie linii regresji do wykresu
lines(skrajne_ve$Sepal.Length, predict(model_ve, skrajne_ve), lwd=2.5, col="violet")


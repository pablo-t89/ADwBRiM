# Zadanie 7.0

raty = read.csv2("Raty.csv")
head(raty)

# PRZY ZALOZENIU O ROWNOLEGLOSCI

# Wyznaczamy model liniowy zalezny od dochodow i rat (zatem wspolczynnik przy
# dochodach bedzie taki sam, niezaleznie od tego czy raty zostaly wziete, czy nie)
model = lm(Wydatek ~ Dochody + Raty, data=raty)
summary(model)

# Wyrozniamy cale zbiory probek z ratami oraz bez rat
wbezrat = raty[raty$Raty=="N",]
wzratami = raty[raty$Raty=="T",]

# Rysujemy wykres z punktami
plot(raty$Dochody, raty$Wydatek) # Zapewnia odpowiednie zakresy
points(wbezrat$Dochody, wbezrat$Wydatek, col="blue") # Punkty bez rat
points(wzratami$Dochody, wzratami$Wydatek, col="red") # Punkty z ratami

# Wybieramy punkty ze skrajnymi dochodami (dwa wystarcza do narysowania linii)
skrajne = raty[raty$Dochody==min(raty$Dochody) | raty$Dochody==max(raty$Dochody),]

# Przerabiamy je na wersje "bez rat" oraz "z ratami"
pbezrat = skrajne
pbezrat$Raty = "N"
pzratami = skrajne
pzratami$Raty = "T"

# Rysujemy linie regresji dla obu przypadkow
lines(pbezrat$Dochody, predict(model, pbezrat), lwd=2.5, col="blue")
lines(pzratami$Dochody, predict(model, pzratami), lwd=2.5, col="red")

# BEZ ZALOZENIA O ROWNOLEGLOSCI

# Robimy osobne modele dla kawalkow danych
modelzratami = lm(Wydatek ~ Dochody, data=wzratami)
summary(modelzratami)
modelbezrat = lm(Wydatek ~ Dochody, data=wbezrat)
summary(modelbezrat)

# Rysujemy wykres z punktami
plot(raty$Dochody, raty$Wydatek)
points(wbezrat$Dochody, wbezrat$Wydatek, col="blue")
points(wzratami$Dochody, wzratami$Wydatek, col="red")

# Rysujemy linie regresji dla obu przypadkow
lines(skrajne$Dochody, predict(modelbezrat, skrajne), lwd=2.5, col="blue")
lines(skrajne$Dochody, predict(modelzratami, skrajne), lwd=2.5, col="red")


# Zadanie 7.1
model = lm(Wydatek ~ Dochody + Wiek, data=raty)
summary(model)

# Wspolrzedne punktow kratowych do rysowania wykresu pseudo3d
wiekv = (0:5/5)*(max(raty$Wiek)-min(raty$Wiek)) + min(raty$Wiek)
dochodyv = (0:5/5)*(max(raty$Dochody)-min(raty$Dochody)) + min(raty$Dochody)

# Funkcja wynikajaca z wyznaczonego modelu regresji
fun = function(d, w) { model$coef[1] + d * model$coef[2] + w * model$coef[3] };

# Macierz wartosci w punktach kratowych
dv = outer(dochodyv, wiekv, fun)

# theta, phi -- katy obrotu perspektywy
wykres = persp(x=dochodyv, y=wiekv, z=dv, xlab="Dochody", ylab="Wiek",
               zlab="Wydatki", ticktype="detailed", theta=30, phi=0)

# Punkty empiryczne i odpowiadajace im punkty teoretyczne (z modelu)
punktye = trans3d(raty$Dochody, raty$Wiek, raty$Wydatek, wykres)
punktyt = trans3d(raty$Dochody, raty$Wiek, predict(model), wykres)

# Rysowanie punktow empirycznych i odcinkow pokazujacych, gdzie przesunela je
# regresja
points(punktye, col="red", lwd=1.5)
segments(punktye$x, punktye$y, punktyt$x, punktyt$y, col="lightpink")


# Zadanie 7.2
raty01 = c(raty$Raty)-1
model = glm(raty01 ~ Wiek, data=raty, family="binomial")
summary(model)

# Dodaje szum jednorodny, zeby punkty nie nachodzily na siebie (dzieki temu bede widzial,
# gdzie jest ich skupisko a gdzie wystepuja pojedynczo)
plot(raty$Wiek + runif(length(raty$Wiek), -1/3, 1/3), raty01, pch=4)
x = seq(min(raty$Wiek), max(raty$Wiek), length=300)
lines(c(min(raty$Wiek), max(raty$Wiek)), c(0.5, 0.5), lwd=2.5, col="gray", lty=2)
lines(x, 1/(1+exp(-predict(model, data.frame(Wiek=x)))), lwd=2.5, col="red")

fun = function(x) {
    y = 1/(1 + exp(-model$coef[1] - model$coef[2] * x))
    if(y < 0.5) {
        "N";
    } else {
        "T"
    };
};

(przewidzianedecyzje = factor(sapply(raty$Wiek, fun)))

# Jak czesto przewidywania na podstawie samego wieku zgadzaja sie z prawdziwymi
# decyzjami?
table(raty$Raty == przewidzianedecyzje)

# Tylko okolo 54% trafnosci, bez sensu
sum(raty$Raty == przewidzianedecyzje) / length(raty$Raty)

# Wniosek: tego, czy ktos wezmie kredyt czy nie lepiej nie oceniac na podstawie samego
# wieku.


# Zadanie 7.3
tr = read.csv2("test_regresji.csv")
head(tr)

model = nls(y ~ c + b*x + a*x^2, data=tr, start=list(c=0,b=0,a=0))
summary(model)

plot(tr$x, tr$y)
x = data.frame(x=seq(min(tr$x), max(tr$x), length=500))
lines(x$x, predict(model, x), lwd=2.5, col="red")

# Wyglada wiarygodnie, z podsumowania takze wynika, ze udalo sie dopasowac model
# dosc dokladnie.


# Zadanie 7.4
modeld = loess(y ~ x, data=tr)
summary(modeld)
# Domyslnie wyszedl wielomian stopnia drugiego ze span=0.75

# Model dla stopnia 0
modelst0 = loess(y ~ x, data=tr, degree=0)
summary(modelst0)

# Model dla stopnia 1
modelst1 = loess(y ~ x, data=tr, degree=1)
summary(modelst1)

# 2 to najwyzszy dopuszczalny stopien...

# Model dla malego span
modellsp = loess(y ~ x, data=tr, degree=2, span=0.4)
summary(modellsp)

# Model dla duzego span
modelhsp = loess(y ~ x, data=tr, degree=2, span=0.95)
summary(modelhsp)

op = par(mfrow=c(1,2))
plot(tr$x, tr$y)
lines(x$x, predict(modelst0, x), lwd=2.5, col="gold")
lines(x$x, predict(modelst1, x), lwd=2.5, col="orange")
lines(x$x, predict(modeld, x), lwd=2.5, col="red")
plot(tr$x, tr$y)
lines(x$x, predict(modellsp, x), lwd=2.5, col="purple")
lines(x$x, predict(modeld, x), lwd=2.5, col="red")
lines(x$x, predict(modelhsp, x), lwd=2.5, col="peru")
par(op)

# Obserwacje:
# - zmniejszenie stopnia popsulo dokladnosc dopasowania, lecz stopien 1 jeszcze
#   mozna uznac za przydatny.
# - zmniejszenie spanu powoduje wyznaczenie wiekszej liczby lokalnych wielomianow,
#   przez co pojawia sie wiecej punktow przegiecia
# - zwiekszenie spanu powoduje powstanie bardziej gladkiej linii, ale roznice miedzy
#   domyslna wartoscia 0.75 a zaproponowana 0.95 nie sa znaczne (wykresy prawie
#   sie pokrywaja)


# Zadanie 7.5
model = smooth.spline(tr$y ~ tr$x)
summary(model)

# Model z mala lambda (lambda to wspolczynnik wygladzania, regularyzacja pewnie)
models0 = smooth.spline(tr$y ~ tr$x, spar=0)
summary(models0)

# Model z duza lambda
models1 = smooth.spline(tr$y ~ tr$x, spar=1)
summary(models1)

plot(tr$x, tr$y)
lines(x$x, predict(models1, x$x)$y, lwd=2.5, col="green")
lines(x$x, predict(model, x$x)$y, lwd=2.5, col="red")
lines(x$x, predict(models0, x$x)$y, lwd=2.5, col="blue")

# Jak widac, znaczenie "wspolczynnika wygladzania" jest tutaj bardzo doslowne, nie
# ma co poza tym wyjasniac.


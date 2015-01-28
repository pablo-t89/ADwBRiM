# Zadanie 7.0
# Dla danych zapisanych w pliku raty.csv:
#  - Utworzyc liniowe modele regresji opisujace zaleznosc wydatku od dochodow
#    klienta z uwzglednieniem decyzji o zakupie ratalnym:
#     - przyjmujac, ze linie regresji, dla roznych decyzji o ratach, sa rownolegle
#     - nie przyjmujac powyzszego zalozenia
#  - Dla obu modeli narysowac wykresy linii regresji (oraz punktow danych)

raty = read.csv2("Raty.csv")
head(raty)

# PRZY ZALOZENIU O ROWNOLEGLOSCI

# Wyznaczamy model liniowy zalezny od dochodow i rat (zatem wspolczynnik przy
# dochodach bedzie taki sam, niezaleznie od tego czy raty zostaly wziete, czy nie)
model = lm(Wydatek ~ Dochody + Raty, data=raty)
summary(model)

# Wyrozniamy cale zbiory probek z ratami oraz bez rat
wyd_bez_rat = raty[raty$Raty=="N",]
wyd_z_ratami = raty[raty$Raty=="T",]

# Rysujemy wykres z punktami
plot(raty$Dochody, raty$Wydatek) # Zapewnia odpowiednie zakresy
points(wyd_bez_rat$Dochody, wyd_bez_rat$Wydatek, col="blue") # Punkty bez rat
points(wyd_z_ratami$Dochody, wyd_z_ratami$Wydatek, col="red") # Punkty z ratami

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
model_z_ratami = lm(Wydatek ~ Dochody, data=wyd_z_ratami)
summary(model_z_ratami)
model_bez_rat = lm(Wydatek ~ Dochody, data=wyd_bez_rat)
summary(model_bez_rat)

# Rysujemy wykres z punktami
plot(raty$Dochody, raty$Wydatek)
points(wyd_bez_rat$Dochody, wyd_bez_rat$Wydatek, col="blue")
points(wyd_z_ratami$Dochody, wyd_z_ratami$Wydatek, col="red")

# Rysujemy linie regresji dla obu przypadkow
lines(skrajne$Dochody, predict(model_bez_rat, skrajne), lwd=2.5, col="blue")
lines(skrajne$Dochody, predict(model_z_ratami, skrajne), lwd=2.5, col="red")


# Zadanie 7.1
# Utworzyc (i narysowac wykres) model regresji liniowej opisujacy zaleznosc wydatku
# od (rownoczesnie) dochodow i wieku klienta

model_dw = lm(Wydatek ~ Dochody + Wiek, data=raty)
summary(model_dw)

# Wspolrzedne punktow kratowych do rysowania wykresu pseudo3d
wiekv = (0:5/5)*(max(raty$Wiek)-min(raty$Wiek)) + min(raty$Wiek)
dochodyv = (0:5/5)*(max(raty$Dochody)-min(raty$Dochody)) + min(raty$Dochody)

# Funkcja wynikajaca z wyznaczonego modelu regresji
funkcja_dw = function(d, w) {
    model_dw$coef[1] + d * model_dw$coef[2] + w * model_dw$coef[3]
};

# Macierz wartosci w punktach kratowych
wydatekv = outer(dochodyv, wiekv, funkcja_dw)

# theta, phi -- katy obrotu perspektywy
wykres_dw = persp(x=dochodyv, y=wiekv, z=wydatekv, xlab="Dochody", ylab="Wiek",
                      zlab="Wydatki", ticktype="detailed", theta=30, phi=0)

# Punkty empiryczne i odpowiadajace im punkty teoretyczne (z modelu)
punkty_emp = trans3d(raty$Dochody, raty$Wiek, raty$Wydatek, wykres_dw)
punkty_teo = trans3d(raty$Dochody, raty$Wiek, predict(model_dw), wykres_dw)

# Rysowanie punktow empirycznych i odcinkow pokazujacych, gdzie przesunela je
# regresja
points(punkty_emp, col="red", lwd=1.5)
segments(punkty_emp$x, punkty_emp$y, punkty_teo$x, punkty_teo$y, col="lightpink")


# Zadanie 7.2
# Utworzyc (i narysowac wykres) model regresji opisujacy decyzje o zakupie na raty
# w zaleznosci od wieku klienta.

raty01 = c(raty$Raty)-1
model_w = glm(raty01 ~ Wiek, data=raty, family="binomial")
summary(model_w)

# Dodaje szum jednorodny, zeby punkty nie nachodzily na siebie (dzieki temu bede widzial,
# gdzie jest ich skupisko a gdzie wystepuja pojedynczo)
plot(raty$Wiek + runif(length(raty$Wiek), -1/3, 1/3), raty01, pch=4)
x = seq(min(raty$Wiek), max(raty$Wiek), length=300)
lines(c(min(raty$Wiek), max(raty$Wiek)), c(0.5, 0.5), lwd=2.5, col="gray", lty=2)
lines(x, 1/(1+exp(-predict(model_w, data.frame(Wiek=x)))), lwd=2.5, col="red")

funkcja_w = function(w) {
    b = 1/(1 + exp(-model_w$coef[1] - model_w$coef[2] * w))
    if(b < 0.5) {
        "N";
    } else {
        "T"
    };
};

(przewidziane_decyzje = factor(sapply(raty$Wiek, funkcja_w)))

# Jak czesto przewidywania na podstawie samego wieku zgadzaja sie z prawdziwymi
# decyzjami?
table(raty$Raty == przewidziane_decyzje)

# Tylko okolo 54% trafnosci, bez sensu
sum(raty$Raty == przewidziane_decyzje) / length(raty$Raty)

# Wniosek: tego, czy ktos wezmie kredyt czy nie lepiej nie oceniac na podstawie samego
# wieku.


# Zadanie 7.3
# Dla danych z pliku test_regresji.csv dopasowac krzywa obrazujaca zaleznosc y od x
# poprzez model regresji (parametrycznej) wielomianem stopnia 2. Narysowac wykres.

tr = read.csv2("test_regresji.csv")
head(tr)

model_kw = nls(y ~ c + b*x + a*x^2, data=tr, start=list(c=0,b=0,a=0))
summary(model_kw)

plot(tr$x, tr$y)
x = data.frame(x=seq(min(tr$x), max(tr$x), length=500))
lines(x$x, predict(model_kw, x), lwd=2.5, col="red")

# Wyglada wiarygodnie, z podsumowania takze wynika, ze udalo sie dopasowac model
# dosc dokladnie.


# Zadanie 7.4
# Dla powyzszych danych dopasowac krzywa metoda regresji lokalnie wielomianowej,
# z uzyciem domyslnych opcji, a nastepnie zaobserwowac wplyw parametrow sterujacych
# (span i degree) na sposob dopasowania. Narysowac odpowiednie wykresy.

model_domyslny = loess(y ~ x, data=tr)
summary(model_domyslny)
# Domyslnie wyszedl wielomian stopnia drugiego ze span=0.75

# Model dla stopnia 0
model_st0 = loess(y ~ x, data=tr, degree=0)
summary(model_st0)

# Model dla stopnia 1
model_st1 = loess(y ~ x, data=tr, degree=1)
summary(model_st1)

# 2 to najwyzszy dopuszczalny stopien...

# Model dla malego span
model_msp = loess(y ~ x, data=tr, degree=2, span=0.4)
summary(model_msp)

# Model dla duzego span
model_dsp = loess(y ~ x, data=tr, degree=2, span=0.95)
summary(model_dsp)

op = par(mfrow=c(1,2))
plot(tr$x, tr$y)
lines(x$x, predict(model_st0, x), lwd=2.5, col="gold")
lines(x$x, predict(model_st1, x), lwd=2.5, col="orange")
lines(x$x, predict(model_domyslny, x), lwd=2.5, col="red")
plot(tr$x, tr$y)
lines(x$x, predict(model_msp, x), lwd=2.5, col="purple")
lines(x$x, predict(model_domyslny, x), lwd=2.5, col="red")
lines(x$x, predict(model_dsp, x), lwd=2.5, col="peru")
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
# Dla tych samych danych dopasowac krzywa metoda gladkich funkcji sklejanych
# z uzyciem domyslnych opcji. Nastepnie zaobserwowac wplyw parametru spar na
# sposob dopasowania. Narysowac odpowiednie wykresy.

model_ls_domyslny = smooth.spline(tr$y ~ tr$x)
summary(model_ls_domyslny)

# Model z mala lambda (lambda to wspolczynnik wygladzania, regularyzacja pewnie)
model_mlam = smooth.spline(tr$y ~ tr$x, spar=0)
summary(model_mlam)

# Model z duza lambda
model_dlam = smooth.spline(tr$y ~ tr$x, spar=1)
summary(model_dlam)

plot(tr$x, tr$y)
lines(x$x, predict(model_dlam, x$x)$y, lwd=2.5, col="green")
lines(x$x, predict(model_ls_domyslny, x$x)$y, lwd=2.5, col="red")
lines(x$x, predict(model_mlam, x$x)$y, lwd=2.5, col="blue")

# Jak widac, znaczenie "wspolczynnika wygladzania" jest tutaj bardzo doslowne, nie
# ma co poza tym wyjasniac.


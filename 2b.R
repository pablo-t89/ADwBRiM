# Moje przydatne funkcje

dlugosc = length
srednia = mean
srednia_kwadratow = function(x) { mean(x^2) };
wariancja = function(x) { srednia_kwadratow(x) - srednia(x)^2 };
odchylenie_st = function(x) { sqrt(wariancja(x)) };
skosnosc = function(x) { srednia((x - srednia(x))^3) / wariancja(x)^(3/2) };
kurtoza = function(x) { mean((x - srednia(x))^4) / wariancja(x)^2 - 3 };

e_odchylenia_st = sd
e_wariancji = var
e_skosnosci = function(x) {
    n = dlugosc(x)
    (n / ((n-1)*(n-2))) * sum((x - srednia(x))^3) / e_odchylenia_st(x)^3
};
e_kurtozy = function(x) {
    n = dlugosc(x)
    wspolczynnik = (n*(n+1)) / ((n-1)*(n-2)*(n-3))
    odjemnik = 3 * (n-1)^2 / ((n-2)*(n-3))
    wspolczynnik * sum((x - srednia(x))^4) / e_wariancji(x)^2 - odjemnik
};


# Zadanie 2b.1
statystyki = function(x, n="Wartosc") {
    wiersze = c(
        "Srednia",
        "Odch. standardowe",
        "Skosnosc",
        "Kurtoza",
        "Minimum",
        "Maksimum"
    )
    wartosci = c(
        srednia(x),
        odchylenie_st(x),
        skosnosc(x),
        kurtoza(x),
        min(x),
        max(x)
    )
    # Bedziemy zwracac ladna ramke z opisami
    wynik = data.frame(wartosci, row.names=wiersze)
    names(wynik)[1] = n # Opcjonalnie: nazwa kolumny pobierana z drugiego argumentu
    wynik
};

x = rnorm(100, 0, 1)
y = rnorm(100, 0, 1)
px = sort(x)
py = sort(y)

cbind(
    statystyki(x, "x"),
    statystyki(y, "y"),
    statystyki(x+y, "x+y"),
    statystyki(x-y, "x-y"),
    statystyki(px+py, "p(x)+p(y)"),
    statystyki(px-py, "p(x)-p(y)")
)


# Interpretacja:
# - "x+y" zachowuje sie tak samo jak "x-y", bo y jest rozkladem normalnym o mi=0
#   (zatem -y zachowuje sie podobnie jak y)
# - minumum oraz maksimum x i y maja zazwyczaj podobne wartosci bezwzgledne
#   i przeciwne znaki; dla x+y/x-y maja nieco wieksze wartosci niz dla x/y (bo tam
#   jest wieksze odchylenie standardowe); dla p(x)+p(y) sa bardzo duze (bo tam
#   minumum to suma minimow, a maksimum -- suma maksimum), zas dla p(x)-p(y) --
#   w miare bliskie 0 (tak samo jak bliskie sobie byly wartosci bezwzgledne minimow
#   i maksimow x oraz y)
# - powyzsze wyjasnia, ze srednie za kazdym razem sa bliskie 0 (gdyby przed dowolna
#   z tych zmiennych losowych postawic znak minus, to jej zachowanie by sie nie
#   zmienilo)
# - odchulenie standardowe x oraz y jest bliskie 1 (bo tak mu kazalismy); wariancje
#   sie sumuja wiec dla x+y/x-y mamy okolo sqrt(2); jednak dla p(x)+p(y) odchylenie
#   jest bliskie liczbie 2 (odchylenia od sredniej sumuja sie tam dosc literalnie),
#   zas p(x)-p(y) to wektor samych liczb bliskich 0, stad bardzo male odchulenie
# - wektor p(x)-p(y) powinien miec najwieksza (co do modulu, znak wyjdzie przypadkowy)
#   skosnosc, bo pozostale sa z zalozenia symetryczne (glownnymi skladnikami sa rozklady
#   normalne), a ten jest dosc przypadkowy (niezerowe wartosci wynikaja w zasadzie
#   z szumu spowodowanego skonczonoscia liczby probek)
# - znaczenie kurtozy wg. angielskiej wikipedii to "peadekness" (kazdy wie o co
#   chodzi, ale jak to przetlumaczyc? "sklonnosc do wystepowania przypadkowych
#   wierzcholkow"?)... w rozkladzie normalnym ze sporej liczby probek raczej tego nie
#   ma, ale p(x)-p(y) zbyt normalne nie jest, wiec tam moze byc duza kurtoza
#   ta kurtoza bedzie dodatnia, bo tam jest bardzo duza koncentracja wartosci wokol
#   punktu 0 (rozklady gdzie nie ma wyraznego ekstremum gestosci (np. jednostajny) to
#   rozklady "platykuryczne" i maja ujemna kurtoze; rozklad normalny jest "mezokurtyczny"
#   i ma kurtoze 0; rozklady z bardziej wyraznym ekstremum w poblizu wartosci sredniej
#   sa "leptokurtyczne" i maja dodatnia kurtoze)

# Zadanie 2b.2

# Ciekawe jest tylko liczenie sredniej geometrycznej.
# Trzeba je obsluzyc osobno dlatego, ze inaczej byloby podnoszenie do potegi 1/0.
# Mozna by policzyc iloczyn wszystkich liczb i wyciagnac z niego pierwiastek n-tego
# stopnia gdzie n to rozmiar proby (prod(x)^(1/dlugosc(x)), ale okazuje sie ze prod(x)
# moze byc zbyt duze jak na typy liczbowe w R. Jednak mozna tak:
#
# prod(x) = exp(sum(log(x)))
# 
# prod(x)^(1/dlugosc(x) = (exp(sum(log(x))))^(1/dlugosc(x)) =
# = exp(sum(log(x))/dlugosc(x))
# 
srednia_p = function(x, p) {
    if(p == 0) {
        exp(sum(log(x))/dlugosc(x))
    } else {
        (sum(x^p)/dlugosc(x))^(1/p)
    };
};

liczby = runif(1500, 1, 10)

wiersze = c(
    "Srednia harmoniczna",
    "Srednia geometryczna",
    "Srednia arytmetyczna",
    "Srednia kwadrarowa"
)

wartosci = c(
    srednia_p(liczby, -1),
    srednia_p(liczby,  0),
    srednia_p(liczby,  1),
    srednia_p(liczby,  2)
)

data.frame(wartosci, row.names=wiersze)


# Zadanie 2b.3
predkosci = read.csv2("Formula1.csv", header=FALSE)[,1]

# Wynikiem jest srednia harmoniczna. Kazde okrazenie ma dlugosc s_i = s oraz
# zostalo pokonane z predkoscia v_i, zatem czas kazdego okrazenia wynosil
# t_i = s / v_i, gdzie i=1..n (n -- liczba okrazen). Zatem srednia predkosc
# to:
# 
# vsr = suma_{i=1..n}(s_i) / suma_{i=1..n}(t_i) = (n * s) / suma_{i=1..n}(s / v_i) =
#     = (n * s) / (s * suma_{i=1..n}(s / v_i)) = n / suma_{i=1..n}(1 / v_i) =
#     = 1 / (suma_{i=1..n}(1 / v_i) / n), czyli srednia harmoniczna, QED.
#
(vsr = srednia_p(predkosci, -1))


# Zadanie 2b.4
wz = read.csv2("Wyksz_Zarobki.csv")
head(wz)

(znaczenie = c(0, (1:10-0.5)*1000, 13500))
wartosci_zarobkow = znaczenie[wz$Zarobki]
median(wartosci_zarobkow)

# Na tym mozna by skonczyc, chyba ze sie nie spi tylko robi R...
t = table(wartosci_zarobkow)
licznosci = as.vector(t)
n = sum(licznosci) # Liczba probek
k = length(licznosci) # Liczba klas
srodki = as.numeric(names(t))
# Ten podzial jest troche oszukany, bo mamy klase o zerowej dlugosci -- w tej metodzie
# jednak to nie przeszkadza.
(podzial = c(0, 0, (srodki[3:k] + srodki[2:(k-1)]) / 2, 2 * srodki[k] - srodki[k - 1]))
q = 0.5 # Rzad kwantyla -- tutaj chcemy mediane...

wczesniej = 0
for(i in 1:k) {
    poczatek = podzial[i]
    koniec = podzial[i+1]
    if(wczesniej + licznosci[i] >= n * q) {
        wspolczynnik = (n * q - wczesniej) / licznosci[i]
        porzadna_mediana = poczatek + (koniec - poczatek) * wspolczynnik
        break
    };
    wczesniej = wczesniej + licznosci[i]
};
porzadna_mediana


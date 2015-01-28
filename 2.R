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

# a -- poziom ufnosci wyraza sie wzorem 1-a
dl_przedzialu_ufnosci_sredniej = function(x, a) {
    n = dlugosc(x)
    2 * qt(1-a/2, n-1) * e_odchylenia_st(x) / sqrt(n)
};
przedzial_ufnosci_sredniej = function(x, a) {
    dl = dl_przedzialu_ufnosci_sredniej(x, a)
    c(srednia(x) - dl/2, srednia(x) + dl/2)
};

przedzial_ufnosci_wariancji = function(x, a) {
    n = dlugosc(x)
    c(n * e_wariancji(x) / qchisq(1-a/2, n-1), n * e_wariancji(x) / qchisq(a/2, n-1))
};


# Zadanie 2.1
# Dla danych zawartych w pliku wzrost.csv (probka statystyczna opisujaca wzrost
# doroslego mezczyzny w Polsce) wyznaczyc podstawowe statystyki opisowe:
#  - mediana, kwartyle, rozstep;
#  - srednia, wariancja - wraz z przedzialem ufnosci na poziomie 0,9;
#  - skosnosc i kurtoza

wzrost = read.csv2("Wzrost.csv", header=FALSE)[,1]

q = quantile(wzrost, prob=c(0.25, 0.5, 0.75), names=FALSE)
(mediana = q[2])
q[1]
q[3]

srednia(wzrost)
przedzial_ufnosci_sredniej(wzrost, 0.9)

wariancja(wzrost)
przedzial_ufnosci_wariancji(wzrost, 0.9)

skosnosc(wzrost)
kurtoza(wzrost)


# Zadanie 2.2
# Dla danych zawartych w pliku zakupy.csv (probka opisujaca kwote zakupow w sklepie
# wielkopowierzchniowym w zaleznosci od pewnych cech klienta) obliczyc wybrane
# statystyki z podzialem na kobiety i mezczyzn:
#  - srednia, wariancja;
#  - mediana, kwartyle, rozstep;
#  - skosnosc i kurtoza

zakupy = read.csv2("Zakupy.csv")
head(zakupy)

# Obliczanie potrzebnych statystyk dla wektora liczb
# Funkcja c(...) zostala wykorzystana w specyficzny sposob -- elementy wektora posiadaja
# konkretne nazwy (zamiast wymienic po przecinkach wartosci elementow, zastosowany
# zostal zapis "nazwa"=wartosc).
statystyki = function(x) {
    kwantyle = quantile(x, prob=c(0.25, 0.5, 0.75), names=FALSE)
    c("Srednia" = srednia(x),
      "Wariancja" = wariancja(x),
      "Q1" = kwantyle[1],
      "Mediana" = kwantyle[2],
      "Q3" = kwantyle[3],
      "Rozstep" = max(x) - min(x),
      "Skosnosc" = skosnosc(x),
      "Kurtoza" = kurtoza(x)
    )
};

# Funkcja tapply pozwala zastosowac dowolna funkcje na danych podanych w pierwszym
# argumencie z podzialem na grupy wedlug wartosci drugiego argumentu.
tapply(zakupy$WYDATEK, zakupy$PLEC, statystyki)


# Zadanie 2.3
# Wyznaczyc wartosc oczekiwana i wariancje rozkladu podanego w tabeli:
# wartosc  1     2     3     4     5
# p-stwo   7/30  1/6   4/15  2/15  1/5

wartosci = 1:5
pstwa = c(7/30, 1/6, 4/15, 2/15, 1/5)

# Rownie dobrze moze byc (ex = sum(wartosci * pstwa))
(ex = weighted.mean(wartosci, pstwa))

# E(X^2)
ex2 = weighted.mean(wartosci^2, pstwa)

# Wariancja
(v = ex2 - (ex^2))


# Zadanie 2.4
# Napisac funkcje pu_test(n, mi, sigma, poziom), ktora:
#  - losuje n liczb z rozkladu normalnego N(mi, sigma)
#  - w oparciu o uzyskane dane wyznacza przedzial ufnosci [x, y] dla sredniej na
#    poziomie poziom (przyjmujac nieznana wariancje)
#  - zwraca wartosc TRUE, jesli mi wpada do wyznaczonego przedzialu i FALSE
#    w przeciwnym razie.
# Utworzona funkcje wykorzystac do nastepujacego eksperymentu: policzyc ile razy
# wypadnie falsz przy 1000 wykonaniach dla n=100 i poziom = 0.9.

# rnorm -- losowanie wektora liczb z przedzialu normalnego o zadanych parametrach
# replicate -- powtarzanie tego samego wiele razy

pu_test = function(n, mi, sigma, poziom) {
    liczby = rnorm(n, mi, sigma)
    a = 1 - poziom
    pus = przedzial_ufnosci_sredniej(liczby, a)
    pus[1] <= mi && mi <= pus[2] 
};

wyniki = replicate(1000, pu_test(100, 0, 1, 0.9))
table(wyniki)

# W tym zadaniu chodzi o to, ze faktycznie przy wspolczynniku 0.9 mamy prawde w 90%
# wywolan funkcji pu_test. Jak podamy zamiast 0.9 inna liczbe to tez sie bedzie zgadzac.


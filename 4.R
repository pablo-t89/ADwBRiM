# Zadanie 4.1
# Przetestowac normalnosc dla danych z pliku wzrost.csv za pomoca testu chi-kwadrat,
# w oparciu o szereg rozdzielczy o 20 klasach o, w przyblizeniu, jednakowej licznosci
# empirycznej.

wzrost = read.csv2("Wzrost.csv", header=FALSE)[,1]

k = 20
(podzial = quantile(wzrost, c(0:k/k), names=FALSE))

# Nieograniczony podzial
npodzial = podzial
npodzial[1] = -Inf
npodzial[k+1] = +Inf
npodzial

(szereg = table(cut(wzrost, npodzial)))

mi = mean(wzrost)
sigma = sd(wzrost)

# pnorm -- dystrybuanta rozkladu normalnego
# zatem ponizszy wektor opisuje pstwa, z jakimi proba z przedzialu normalnego
# o parametrach (mi, sigma) znajdzie sie w poszczegolnych przedzialach
pstwa = pnorm(npodzial[2:(k+1)], mi, sigma) - pnorm(npodzial[1:k], mi, sigma)

# Test chi-kwadrat to test zgodnosci, ale skoro badamy zgodnosc z rozkladem normalnym
# (wyznaczonym za pomoca pnorm), to dziala jako test normalnosci.
(t = chisq.test(szereg, p=pstwa))

# O braku podstaw do odrzucenia hipotezy o zgodnosci rozkladow mozna mowic, gdy p-value
# przekracza 0.05 (tzn. 1 - poziom ufnosci; zazwyczaj obchodzi nas 95%).

# Odczytanie p-value z samego chisq.test w tym wypadku jednak nie wystarcza. Podane
# wektory danych (szereg i pstwa) maja dlugosc k, wiec domyslnie p-value liczone
# jest tak, jakbysmy w tescie chi-kwadrat mieli k-1 stopni swobody. Jednak rozklad
# normalny zostal wygenerowany w oparciu o mi i sigme pochodzace z danych, wiec tak
# naprawde dwie dodatkowe informacje zostaly narzucone przez nas. Dlatego zamiast k-1
# stopni swobody powinnismy zastosowac o dwa mniej (czyli k-3), a zamiast odczytywac
# p-value wprost z chisq.test, mozemy policzyc ja prawidlowo sami w oparciu o statystyke
# zwracana przez test, w sposob przedstawiony ponizej.
# (Podziekowania za wsparcie merytoryczne dla pewnych MS, GG i KS).

1 - pchisq(t$statistic, k-3)

# To jest nasza prawdziwa p-wartosc. Nadal jest wieksza niz 0.05, zatem nie ma podstaw
# do odrzucenia hipotezy o zgodnosci rozkladow. Zgodnosc badanego rozkladu z rozkladem
# normalnym mozna interpretowac jako jego normalnosc, co nalezalo zbadac.

# Przedstawmy wynik w postaci graficznej
hist(wzrost, podzial, col="yellow")
x = seq(min(wzrost), max(wzrost), len = 500)
lines(x, dnorm(x, mi, sigma), lwd=2.5, col="red")


# Zadanie 4.2
# Przetestowac normalnosc dla danych z pliku wzrost.csv za pomoca testu chi-kwadrat,
# w oparciu o szereg rozdzielczy o 15 klasach o jednakowej licznosci teoretycznej
# (przyklad z wykladu). Nastepnie wykonac ten sam test, ale przed utworzeniem szeregu
# rozdzielczego dokonac zaszumienia danych, zaburzajac je o wartosc z rozkladu
# jednostajnego na przedziale [-1/2, 1/2].

# zmienne wzrost, mi i sigma zostaja z poprzedniego podpunktu

k = 15
podzial = qnorm(0:k/k, mi, sigma)

(szereg = table(cut(wzrost, podzial)))

# Tym razem nie musimy liczyc zadnego wektora "pstwa" do porownywania, bo nasz podzial
# jest specjalnie zrobiony tak, zeby proba z rozkladu normalnego wpadala z takim
# samym pstwem do kazdego z przedzialow).

(t = chisq.test(szereg))
1 - pchisq(t$statistic, k-3) # Bo do generowania szeregu uzylismy mi i sigmy z danych.
# Tym razem sa podstawy od odrzucenia hipotezy. W oparciu o nie mozna powiedziec, ze ten
# rozklad nie jest zgodny z normalnym.

zwzrost = wzrost + runif(length(wzrost), -0.5, 0.5) # Wzrosty zaburzone wg polecenia
(zszereg = table(cut(zwzrost, podzial)))

(t = chisq.test(zszereg))
1 - pchisq(t$statistic, k-3)
# Po zaszumieniu danych okazalo sie, ze prawidlowa p-wartosc (druga linijka)
# przekroczyla (nieznacznie, ale to w sam raz dla 95% poziomu ufnosci) 0.05, wiec
# brak jest podstaw do hipotezy o zgodnosci otrzymanego rozkladu z rozkladem normalnym.

# Skonczony podzial
spodzial = podzial
spodzial[1] = min(wzrost) - 0.5
spodzial[k + 1] = max(wzrost) + 0.5

op = par(mfrow=c(1,2))
hist(wzrost, spodzial, col="yellow")
lines(x, dnorm(x, mi, sigma), lwd=2.5, col="red")
hist(zwzrost, spodzial, col="cyan")
lines(x, dnorm(x, mi, sigma), lwd=2.5, col="red")
par(op)

# Blekitny histogram przeszedl test zgodnosci z rozkladem normalnym, ale zolty juz nie
# -- nie jest to oczywiste, ale ma pewien sens po spojrzeniu na wykresy.


# Zadanie 4.3
# Dla wszystkich (szesciu) par roznego wyksztalcenia (dane zakupy.csv) wykonac testy
# rownosci przecietnego wydatku miedzy grupami osob o danym wyksztalceniu.

zakupy = read.csv2("Zakupy.csv")

p = zakupy$WYDATEK[zakupy$WYKSZTALCENIE=="P"]
z = zakupy$WYDATEK[zakupy$WYKSZTALCENIE=="Z"]
s = zakupy$WYDATEK[zakupy$WYKSZTALCENIE=="S"]
w = zakupy$WYDATEK[zakupy$WYKSZTALCENIE=="W"]

# t.test -- test rownosci srednich, im wieksze p-value tym lepiej
# Mozna sie np. umowic, ze odrzucamy hipoteze gdy p-value jest mniejsze od 0.05.
t.test(p, z) # brak podstaw od odrzucenia hipotezy zerowej, tzn. srednie w miare rowne
t.test(p, s) # sa podstawy do odrzucenia h0, srednie niezbyt rowne
t.test(p, w) # sa podstawy do odrzucenia h0
t.test(z, s) # brak podstaw do odrzucenia h0
t.test(z, w) # sa podstawy do odrzucenia h0
t.test(s, w) # brak podstaw do odrzucenia h0

# Ej, zauwazyl ktos, ze to ma sens? Hipoteza o rownych srednich przeszla tylko
# w przypadkach, gdy roznice w wyksztalceniu sa relatywnie najmniejsze
# (tzn. miedzy P a Z, Z a S oraz S a W)


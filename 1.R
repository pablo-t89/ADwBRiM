# Zadanie 1.1
c(4,3,2,1,0)
4:0
seq(4, 0)  


# Zadanie 1.2
(1:20)^3  


# Zadanie 1.3 

seq(10,30,2)^2 # bierzemy same liczby parzyste i liczymy ich kwadraty 

# Spostrzezenie: mozemy zamiast tego wziac liczby od 5 do 15 i liczyc kwadraty
# dwukrotnosci. Ponadto kwadraty dwukrotnosci sa tym samym co czterokrotnosci kwadratow.
4*(5:15)^2


# Zadanie 1.4

sum((1:98)^3) 
# Spostrzezenie: 1^3+...+n^3 = (n*(n+1)/2)^2 -- to jest znany wzor. Najprosciej
# zapisac 98*99/2 od razu jako 4851, wtedy wynik zostanie zapisany w szesciu znakach
# (co jest jeszcze lepsze niz podanie od razu wyniku, bo ma on osiem cyfr).
4851^2


# Zadanie 1.5

# Plik .csv z pliku .xls najlepiej sobie zrobic samemu. Jezeli pola beda rozdzielone
# srednikiem, to R sobie z tym poradzi najlepiej (nie trzeba bedzie podawac
# dodatkowych opcji dla read.csv2). Kodowanie znakow z ktorym sie spotkacie pod
# Windowsami / na labkach / w plikach Gordiego to (niestety) Windows-1250.
wz = read.csv2("Wyksz_Zarobki.csv")
head(wz)

# Trzeba sobie wymyslic co znaczy "Ponad 10000", przyjmiemy np. 13500
# wektor znaczenie bedzie zawietal zatem: 0, 500, 1500, ..., 8500, 9500, 13500
(znaczenie = c(0, (1:10-0.5)*1000, 13500))

wartosci_zarobkow = znaczenie[wz$Zarobki]

# No i to dobiero jest wynik
mean(wartosci_zarobkow)


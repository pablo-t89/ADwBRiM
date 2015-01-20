# Zadanie 1b.1
(A = matrix(c( 5,  1,  2, -1, -2,  8), nrow=3, ncol=2, byrow=TRUE))
(B = matrix(c( 1,  3,  3,  2,  1,  5), nrow=2, ncol=3, byrow=TRUE))

# Macierz 3x2 mozna bez problemu pomnozyc z macierza 2x3, wyjdzie macierz 3x3
(C = A %*% B)

# Macierz 3x3 jest kwadratowa, wiec nawet ma szanse posiadac macierz odwrotna.
# Oczywiscie o ile wyznacznik nie jest zerem...
det(C) # tak bardzo zero

# Gdyby ktos watpil, ze to jest zero, to warto zauwazyc ze macierz o wspolczynnikach
# calkowitych ma takze calkowity wyznacznik -- a zadnych innych liczb calkowitych
# nie ma w poblizu tej otrzymanej wartosci.


# Zadanie 1b.2

sumdiag = function(A) {
    # Wynikiem bedzie macierz kwadratowa, ktora ma co najwyzej tyle kolumn i wierzy co A,
    # zatem jej wymiar to:
    n = min(dim(A))
    B = matrix(0, nrow=n, ncol=n) # tutaj bedzie wynik (na poczatku: macierz zerowa)
    
    # Dla kazdego i od 1 do n wlacznie:
    for(i in 1:n) {
        # Na i-tym polu glownej przekatnej macierzy B zapisujemy sume elementow lezacych
        # w i-tym wierszu (A[i,] to wektor zawierajacy wszystkie elementy i-tego
        # wiersza) i i-tej kolumnie (A[i,] to wektor zawierajacy wszystkie elementy
        # i-tej kolumny) macierzy A. Zauwazmy, ze A[i, i] zawarty jest zarowno w i-tym
        # wierszu jak i w i-tej kolumnie, wiec skoro policzylismy go dwa razy, to
        # potem raz go odejmujemy.
        B[i, i] = sum(A[i,]) + sum(A[,i]) - A[i, i]
    };
    B
};

(A = matrix(1:12, nrow=3, ncol=4, byrow=TRUE)) # Przykladowa macierz
sumdiag(A) # Wynik dla przykladowej macierzy


# Zadanie 1b.3

skc = function(n) {
    wynik = 0
    
    while(n) { # Dopoki sa jeszcze jakies cyfry
        wynik = wynik + (n %% 10)^2 # Dodaj do wyniku kwadrat cyfry jednosci
        n = n %/% 10 # Podziel liczbe n przez 10 bez reszty
    };
    
    wynik
};

# Chcemy rozwazac dowolna liczbe naturalna, ale zauwazmy, ze dla liczb k-cyfrowych
# skc(n) wynosi co najwyzej 81*k, podczas gdy n wynosi co najmniej 10^(k-1).
# Dla k>=4 zachodzi jednak 10^(k-1) > 81*k (kto nie wierzy, niech sobie to udowodni
# pochodnymi). Skoro dla n>=100 funkcja skc jest scisle malejaca (a jest to funkcja z N
# na N), to jesli x_0 = n, to x_n < 1000 (bo dopoki nie zejdziemy ponizej 1000 to na pewno
# bedziemy przeskakiwac do coraz mniejszych liczb, a po zejsciu ponizej 1000 juz nie
# wyjdziemy z przedzialu 1..999, bo najwieksza wartosc jaka przyjmuje tam skc to 243).

# Mozna to dalej rozbijac porzez "zastanawianie sie", ale nie ma juz takiej potrzeby.

sprawdz = function(n) {
    wartosci = c() # Pusty wektor, to samo co NULL
    
    while(TRUE) {
        if(n == 1 || n == 4) {
            # Jesli doszlismy do 1 lub 4, to znaczy ze sprawdzenie sie udalo.
            return(TRUE)
        };
        
        if(is.element(n, wartosci)) {
            # Nie bylo 1 ani 4, ale doszlismy z powrotem do elemtntu w ktorym juz
            # bylismy: to znaczy ze bedziemy sie krecic w kolko i juz nic nowego nie
            # odkryjemy.
            return(FALSE)
        };
        
        wartosci = union(wartosci, n)
        n = skc(n)
    };
};

# Sprawdzmy, jakie wyniki daje funkcja sprawdz dla liczb 1..999
table(sapply(1:999, sprawdz))

# Z kazdej z tych liczb udalo sie dotrzec do 1 lub do 4. QED.


# WERSJA DLA UCZESTNIKOW MWPZ I INNYCH DEWIANTOW

# Zauwazmy, ze przy sprawdzaniu tezy dla liczb 1..999 powtarzamy te same czynnosci po
# kilka razy. Np. aby sprawdzic liczbe 999, sprawdzamy po kolei 243, 29, 85, 89, 145,
# 42, 20 i dopiero potem docieramy do liczby 4. Jednak w zaden sposob nie korzystamy
# potem z informacji, ze z liczb przejrzanych po drodze da sie dotrzec do 4 i np.
# sprawdzajac pozniej liczbe 243 jeszcze raz sprawdzamy 29, 85, 89, 142, 42 i 20.
# Gdyby jednak zapamietywac posrednie kroki, caly problem zbadania liczb 1..999
# udaloby sie rozstrzygnac wykonujac kilka razu mniej operacji.

# Tablica do zapamietywania, czy z danej liczby da sie dotrzec do 1 lub 4 (TRUE gdy
# tak, FALSE gdy nie, NA gdy jeszcze nie sprawdzono)
liczbaok = rep(NA, 999)

liczbaok[c(1, 4)] = TRUE # Z zalozenia

# Wektor "liczbaok" bedzie zmieniany "z wewnatrz" funkcji sprawdzmadrze -- operator
# przypisania do zmiennej globalnej to "<<-" (dziala tak samo jak <- lub =, ale
# pozwala na pisanie po zmiennych globalnych, czyli takich zadeklarowanych poza
# funkcja).

sprawdzmadrze = function(n) {
    if(is.na(liczbaok[n])) {
        s = skc(n)
        liczbaok[n] <<- FALSE
        if(is.na(liczbaok[s])) {
            sprawdzmadrze(s)
        };
        liczbaok[n] <<- liczbaok[s]
    };
    liczbaok[n]
};

table(sapply(1:999, sprawdzmadrze))


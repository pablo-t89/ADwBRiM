# Zadanie 1c.1

# Wystarczy wiedziec o istnieniu funkcji lfactorial (logarytm naturalny z silni.
# Zatem lfactorial(n)/log(10) to bedzie logarytm dziesietny z silni (log to logarytm
# naturalny). Liczba cyfr liczby to podloga z logarytmu dziesietnego powiekszona o 1.
cyfry_silni = function(n) {
    floor(lfactorial(n)/log(10))+1
};


# Zadanie 1c.2

# Ciekawostka: liczba zer na koncu rozwiniecia dziesietnego liczby n to najwieksze
# takie k, dla ktorego 10^k jest dzielnikiem n. Zatem jest to najwieksze takie k,
# dla ktorego zarowno 2^k i 5^k sa dzielnikami liczby n. Silnie liczb calkowitych
# zawsze maja po rozbiciu na dzielniki pierwsze wiekszy wykladnik przy 2 niz przy 5,
# wiec wystarczy sprawdzic dla 5.
# 
# Piatki w rozbiciu silni n na dzielniki pierwsze pochodza z: liczb podzielnych
# przez 5 nie wiekszych od n (jest ich n%/%5, gdzie %/% to dzielenie przez reszty),
# dodatkowo z liczb podzielnych przez 25, 125, itd. Zauwazmy, ze zamiast dzielic
# bez reszty przez kolejne potegi piatki (az stana sie one za duze i bedzie wychodzic
# ciagle 0, wiec bedzie mozna przestac liczyc) mozna jednak caly czas dzielic przez 5,
# zastepujac dotychczasowy wynik otrzymanym ilorazem.

zera_na_koncu_silni = function(n) {
    wynik = 0
    while(n >= 5) { # Jak n spadnie ponizej 5, to nic nowego juz nie dostaniemy.
        n = n %/% 5
        wynik = wynik + n
    };
    wynik
};


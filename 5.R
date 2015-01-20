# Zadanie 5.1

zakupy = read.csv2("Zakupy.csv")
head(zakupy)

podzial = quantile(zakupy$WIEK, probs=c(0, 1/3, 2/3, 1), names=FALSE)
(poziomwieku = cut(zakupy$WIEK, podzial, include.lowest=TRUE, labels=c("Y", "M", "O")))

# Chcemy uzyc ANOVY, ale do tego warto miec normalnosc w grupach (jedno z zalozen).
# Sprawdzmy ja, chocby z ciekawosci...

# Na oko:
op = par(mfrow=c(1,3))
qqnorm(zakupy$WYDATEK[poziomwieku=="Y"], main="Y")
qqline(zakupy$WYDATEK[poziomwieku=="Y"], lwd=2.5, col="blue")
qqnorm(zakupy$WYDATEK[poziomwieku=="M"], main="M")
qqline(zakupy$WYDATEK[poziomwieku=="M"], lwd=2.5, col="peru")
qqnorm(zakupy$WYDATEK[poziomwieku=="O"], main="O")
qqline(zakupy$WYDATEK[poziomwieku=="O"], lwd=2.5, col="magenta")
par(op)
# Moje oko mowi: "no niezbyt".

# Shapiro-Wilkiem:
shapiro.test(zakupy$WYDATEK[poziomwieku=="Y"]) # Nie
shapiro.test(zakupy$WYDATEK[poziomwieku=="M"]) # Nie
shapiro.test(zakupy$WYDATEK[poziomwieku=="O"]) # Nie
# Odrzucamy h0, gdy p-value mniejsze od 0.05 (a hipoteza zerowa jest taka, ze dane
# wrzucone do testu pochodza z rozkladu normalnego).

# Coz, nie mamy normalnosci w grupach, ale policzyc ANOVE mozna, kto zabroni. ;)

anova(lm(zakupy$WYDATEK ~ poziomwieku))

# 
# UWAGA: hipoteza zerowa do ANOVY jest taka, ze dane sa od siebie zalezne, a roznica
# srednich miedzy grupami JEST istotna statystycznie.
# Interpretacja: jak Pr(>F) wychodzi duze, to odrzucamy hipoteze zerowa. Brak podstaw
# jest jak wyjdzie mniej niz 0.05. Jak wychodzi jakos bardzo malo to R wypisuje gwiazdki
# przy tej wartosci (im wiecej gwiazdek, tym lepiej sie udalo).
# 
# "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", tzn. zeby byl poziom
# ufnosci 0.95 (nasze slynne 0.05) to musi byc przynajmniej gwiazdka przy wyniku.
# 

# Wniosek: sa podstawy do odrzucenia hipotezy, jakoby wydatki zalezaly od wieku.
# Roznica jest nieistotna statystycznie.

# Wniosek nie jest zbyt pewny, bo nie bylo normalnosci w grupach.


# Zadanie 5.2

# Tutaj jest duzo problemow, np. taki, ze grupy nie sa rownoliczne, tzn.
table(poziomwieku:zakupy$PLEC)

# Rzut okiem na normalnosc
op = par(mfrow=c(2,3))
qqnorm(zakupy$WYDATEK[poziomwieku=="Y" & zakupy$PLEC=="K"], main="YK")
qqline(zakupy$WYDATEK[poziomwieku=="Y" & zakupy$PLEC=="K"], lwd=2.5, col="cyan")
qqnorm(zakupy$WYDATEK[poziomwieku=="M" & zakupy$PLEC=="K"], main="MK")
qqline(zakupy$WYDATEK[poziomwieku=="M" & zakupy$PLEC=="K"], lwd=2.5, col="orange")
qqnorm(zakupy$WYDATEK[poziomwieku=="O" & zakupy$PLEC=="K"], main="OK")
qqline(zakupy$WYDATEK[poziomwieku=="O" & zakupy$PLEC=="K"], lwd=2.5, col="violet")
qqnorm(zakupy$WYDATEK[poziomwieku=="Y" & zakupy$PLEC=="M"], main="YM")
qqline(zakupy$WYDATEK[poziomwieku=="Y" & zakupy$PLEC=="M"], lwd=2.5, col="navy")
qqnorm(zakupy$WYDATEK[poziomwieku=="M" & zakupy$PLEC=="M"], main="MM")
qqline(zakupy$WYDATEK[poziomwieku=="M" & zakupy$PLEC=="M"], lwd=2.5, col="tan")
qqnorm(zakupy$WYDATEK[poziomwieku=="O" & zakupy$PLEC=="M"], main="OM")
qqline(zakupy$WYDATEK[poziomwieku=="O" & zakupy$PLEC=="M"], lwd=2.5, col="purple")
par(op)

shapiro.test(zakupy$WYDATEK[poziomwieku=="Y" & zakupy$PLEC=="K"]) # Tak
shapiro.test(zakupy$WYDATEK[poziomwieku=="M" & zakupy$PLEC=="K"]) # Nie
shapiro.test(zakupy$WYDATEK[poziomwieku=="O" & zakupy$PLEC=="K"]) # Tak
shapiro.test(zakupy$WYDATEK[poziomwieku=="Y" & zakupy$PLEC=="M"]) # Nie
shapiro.test(zakupy$WYDATEK[poziomwieku=="M" & zakupy$PLEC=="M"]) # Nie
shapiro.test(zakupy$WYDATEK[poziomwieku=="O" & zakupy$PLEC=="M"]) # Nie
# Tylko kobiety w wieku innym niz sredni sa normalne. Ale tak ogolnie to trudno
# mowic o tym, ze zachodzi "normalnosc w grupach".
# 
# Ciekawostka: test Shapiro-Wilka pokazal dobra normalnosc dla mlodych kobiet,
# ale na wykresie one sie wcale nie trzymaja wykresu normalnosci. Warto jednak zwrocic
# uwage, ze odstepstwa z obu stron sa takie same.

# Oczywiscie ANOVE mozna zawsze puscic i probowac interpretowac.
anova(lm(zakupy$WYDATEK ~ poziomwieku:zakupy$PLEC))

# Pr(>F) wyszlo juz niby mniejsze, ale zeby przyjac taka hipoteze to musielibysmy
# miec poziom ufnosci 75% (powazna statystyka, nie ma co). Na domyslnym 95% znowu
# odrzucamy hipoteze, jakoby wiek i plec mialy wplyw na wydatki.

# Wniosek nie jest zbyt pewny, bo nie bylo normalnosci w grupach.


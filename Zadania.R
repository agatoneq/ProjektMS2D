library(moments)

salon1 <- c(102, 54, 98, 86, 110, 83, 119, 86, 90, 93, 110, 60, 75, 82, 90, 79, 100, 58, 108, 91, 67, 
            77, 60, 99, 73, 96, 75, 92, 84, 80, 88, 65, 104, 109, 97, 90, 101, 91, 101, 90, 90, 66, 72, 
            100, 107, 98, 102)

salon2 <- c(133, 61, 66, 80, 50, 86, 86, 105, 82, 107, 93, 87, 94, 105, 62, 102, 70, 72, 122, 97, 99, 
            123, 96, 111, 73, 60, 107, 112, 50, 108, 77, 73, 90, 115, 71, 77, 87, 64, 115, 105, 77, 
            111, 77, 62, 84, 73, 94, 62, 125, 44, 102, 70, 62)

#MIARY WEDLUG SZEREGU SZCZEGÓŁOWEGO
# Miary przeciętne
# średnia
mean(salon1)
mean(salon2)

# mediana
median(salon1)
median(salon2)

# moda
which.max(table(salon1))
which.max(table(salon1))

# Q1
quantile(salon1, 0.25)
quantile(salon2, 0.25)

# Q3
quantile(salon1, 0.75)
quantile(salon2, 0.75)


# Miary rozproszenia
# wariancja S2* populacji


# odchylenie standardowe S* populacji


# wariancja S2 próby
var(salon1)
var(salon2)

# odchylenie standardowe S próby
sd(salon1)
sd(salon2)

# odchylenie przeciętne


# odchylenie przeciętne od mediany
mad(salon1)
mad(salon2)

# odchylenie ćwiartkowe
IQR(salon1)
IQR(salon2)

# współczynnik zmienności w % 
100*sd(salon1)/mean(salon1)
100*sd(salon2)/mean(salon2)

# pozycyjny współczynnik zmienności w %
100*IQR(salon1)/median(salon1)
100*IQR(salon2)/median(salon2)


# Miary asymetrii i koncentracji
# skośność
skewness(salon1)
skewness(salon2)

# kurtoza
kurtosis(salon1)
kurtosis(salon2)

# eksces




# utworzenie histogramu dla salonu 1
hist_salon1 <- hist(salon1, breaks = 8)

# utworzenie szeregu rozdzielczego dla salonu 1
szereg_roz1 <- hist(salon1, seq(min(salon1), max(salon1), length.out = 8), plot = FALSE)
war_srodkowe1 <- szereg_roz1$mids
liczebnosci1 <- szereg_roz1$counts
weighted.mean(war_srodkowe1, liczebnosci1)

# utworzenie histogramu dla salonu 2
hist_salon2 <- hist(salon1, breaks = 8)

# utworzenie szeregu rozdzielczego dla salonu 2
szereg_roz2 <- hist(salon2, seq(min(salon2), max(salon2), length.out = 8), plot = FALSE)
war_srodkowe2 <- szereg_roz2$mids
liczebnosci2 <- szereg_roz2$counts



#MIARY WEDLUG SZEREGU ROZDZIELCZEGO
# Miary przeciętne
# średnia
weighted.mean(war_srodkowe1, liczebnosci1)
weighted.mean(war_srodkowe2, liczebnosci2)


# mediana
# moda
# Q1
# Q3

# Miary rozproszenia
# wariancja
# odchylenie standardowe
# wariancja
# odchylenie standardowe
# odchylenie przeciętne
# odchylenie przeciętne od mediany
# odchylenie ćwiartkowe
# współczynnik zmienności w % 
# pozycyjny współczynnik zmienności w %

# Miary asymetrii i koncentracji
# skośność
# kurtoza
# eksces
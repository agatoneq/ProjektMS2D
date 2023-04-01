library(moments)

salon1 <- c(102, 54, 98, 86, 110, 83, 119, 86, 90, 93, 110, 60, 75, 82, 90, 79, 100, 58, 108, 91, 67, 
            77, 60, 99, 73, 96, 75, 92, 84, 80, 88, 65, 104, 109, 97, 90, 101, 91, 101, 90, 90, 66, 72, 
            100, 107, 98, 102)

salon2 <- c(133, 61, 66, 80, 50, 86, 86, 105, 82, 107, 93, 87, 94, 105, 62, 102, 70, 72, 122, 97, 99, 
            123, 96, 111, 73, 60, 107, 112, 50, 108, 77, 73, 90, 115, 71, 77, 87, 64, 115, 105, 77, 
            111, 77, 62, 84, 73, 94, 62, 125, 44, 102, 70, 62)

#ZADANIE 1

#MIARY WEDŁUG SZEREGU SZCZEGÓŁOWEGO
miary_sz_szczegolowego <- function(dane)
{
  szereg_sz <- sort(dane)
  
  # Miary przeciętne
  srednia = mean(szereg_sz)
  mediana = median(szereg_sz)
  moda = which.max(table(szereg_sz))
  Q1 = quantile(szereg_sz, 0.25)
  Q3 = quantile(szereg_sz, 0.75)
  
  # Miary rozproszenia
  wariancja = var(szereg_sz)
  odchylenie_standardowe = sd(szereg_sz)
 # wariancja = 
  #odchylenie_standardowe = 
  odchylenie_przecietne = 
  odchylenie_przecietne_od_mediany = mad(szereg_sz)
  odchylenie_cwiartkowe = IQR(szereg_sz)
  wspolczynnik_zmiennosci =  100*sd(szereg_sz)/mean(szereg_sz)
  pozycyjny_wspolczynnik_zmiennosci  =  100*IQR(szereg_sz)/median(szereg_sz)
  
  # Miary asymetrii i koncentracji
  skosnosc = skewness(szereg_sz)
  kurtoza =  kurtosis(szereg_sz)
  eksces = kurtoza - 3
}


# utworzenie histogramu dla salonu 1
hist_salon1 <- hist(salon1, breaks = 8)
# utworzenie histogramu dla salonu 2
hist_salon2 <- hist(salon1, breaks = 8)


#MIARY WEDLUG SZEREGU ROZDZIELCZEGO
miary_sz_rozdzielczego <- function(dane)
{
  # utworzenie szeregu rozdzielczego dla salonu
  szereg_roz <- hist(dane, seq(min(dane), max(dane), length.out = 8), plot = FALSE)
  war_srodkowe <- szereg_roz$mids
  liczebnosci <- szereg_roz$counts
  weighted.mean(war_srodkowe, liczebnosci)
  
  # Miary przeciętne
  srednia = weighted.mean(war_srodkowe, liczebnosci)
  mediana = 
  moda = 
  Q1 = 
  Q3 = 
  
  # Miary rozproszenia
  wariancja = 
  odchylenie_standardowe = 
  # wariancja = 
  #odchylenie_standardowe = 
  odchylenie_przecietne = 
  odchylenie_przecietne_od_mediany = 
  odchylenie_cwiartkowe = 
  wspolczynnik_zmiennosci = 
  pozycyjny_wspolczynnik_zmiennosci = 
  
  # Miary asymetrii i koncentracji
  skosnosc = 
  kurtoza = 
  eksces = kurtoza - 3
}
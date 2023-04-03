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
  wariancja_nieobciazona = wariancja * (n/(n-1))
  odchylenie_standardowe_NO = sqrt(wariancja_nieobciazona)
  odchylenie_przecietne = sum(abs(szereg_sz - srednia)) / length(szereg_sz)
  odchylenie_przecietne_od_mediany = sum(abs(szereg_sz - mediana)) / length(szereg_sz)
  odchylenie_cwiartkowe = IQR(szereg_sz)/2
  wspolczynnik_zmiennosci =  100*odchylenie_standardowe/srednia
  pozycyjny_wspolczynnik_zmiennosci  =  100*odchylenie_cwiartkowe/mediana
  
  # Miary asymetrii i koncentracji
  skosnosc = skewness(szereg_sz)
  kurtoza =  kurtosis(szereg_sz)
  eksces = kurtoza - 3
}


# utworzenie histogramu dla salonu 1
hist_salon1 <- hist(salon1, breaks = 8)
# utworzenie histogramu dla salonu 2
hist_salon2 <- hist(salon1, breaks = 8)


#funkcja dla kwantyli w szeregu rozdzielczym
kwantyl<-function(szereg_rozdz, q)
{
  kwantyl=0
  war_srodkowe <- szereg_rozdz$mids
  liczebnosc <- szereg_rozdz$counts
  
  l_skumulowana <- liczebnosc
  for(i in 1:length(liczebnosc))
  {
    l_skumulowana[i] = sum(liczebnosc[1:i])
  }
  
  
  if (sum(liczebnosc) %% 2 == 0)
  {
    indeks1 <- floor(sum(liczebnosc) * q)+1
    indeks2 <- floor(sum(liczebnosc) * q)+2
    
    koniec1 = 0
    i1 = 1
    while (koniec1 == 0)
    {
      if (l_skumulowana[i1] > indeks1)
      {
        kwantyl1 <- war_srodkowe[i1]
        koniec1 = 1
      }
      else
        i1 = i1 + 1
    }
    koniec2 = 0
    i2 = 1
    while (koniec2 == 0)
    {
      if (l_skumulowana[i2] > indeks2)
      {
        kwantyl2 <- war_srodkowe[i2]
        koniec2 = 1
      }
      else
        i2 = i2 + 1
    }
    kwantyl = (kwantyl1+kwantyl2)/2
  }
  
  else
  {
    indeks <- floor(sum(liczebnosc) * q)+1
    
    koniec = 0
    i = 1
    while (koniec == 0)
    {
      if (l_skumulowana[i] > indeks)
      {
        kwantyl = war_srodkowe[i]
        koniec = 1
      }
      else
        i = i + 1
    }
  }
  return (kwantyl)
}


#MIARY WEDLUG SZEREGU ROZDZIELCZEGO
miary_sz_rozdzielczego <- function(dane)
{
  # utworzenie szeregu rozdzielczego dla salonu
  szereg_roz <- hist(dane, seq(min(dane), max(dane), length.out = 8), plot = FALSE)
  war_srodkowe <- szereg_roz$mids
  liczebnosc <- szereg_roz$counts
  
  # Miary przeciętne
  srednia = weighted.mean(war_srodkowe, liczebnosc)
  mediana = kwantyl(szereg_roz,0.5)
  moda = war_srodkowe[which.max(liczebnosc)]
  Q1 = kwantyl(szereg_roz, 0.25)
  Q3 = kwantyl(szereg_roz, 0.75)

  # Miary rozproszenia
  wariancja = sum(((war_srodkowe-srednia) ^ 2) * liczebnosc) / sum(liczebnosc)
  odchylenie_standardowe = sqrt(wariancja)
  wariancja_nieobciazona = wariancja * (n/(n-1))
  odchylenie_standardowe_NO = sqrt(wariancja_nieobciazona)
  odchylenie_przecietne = sum(abs(war_srodkowe - srednia) * liczebnosc) / sum(liczebnosc)
  odchylenie_przecietne_od_mediany = sum(abs(war_srodkowe - mediana) * liczebnosc) / sum(liczebnosc)
  odchylenie_cwiartkowe = (Q1 - Q3) / 2
  wspolczynnik_zmiennosci = 100*odchylenie_standardowe/srednia
  pozycyjny_wspolczynnik_zmiennosci = 100*odchylenie_cwiartkowe/mediana
  
  # Miary asymetrii i koncentracji
  skosnosc = 3*(srednia - mediana) / odchylenie_standardowe
  kurtoza = (sum((war_srodkowe - srednia) ^ 4 * liczebnosc) / sum(liczebnosc)) / (odchylenie_standardowe ^ 4)
  eksces = kurtoza - 3
}
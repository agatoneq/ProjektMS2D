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
  wariancja_es_nieobciazony = wariancja * (length(szereg_sz)/(length(szereg_sz)-1))
  odchylenie_standardowe_NO = sqrt(wariancja_es_nieobciazony)
  odchylenie_przecietne = sum(abs(szereg_sz - srednia)) / length(szereg_sz)
  odchylenie_przecietne_od_mediany = sum(abs(szereg_sz - mediana)) / length(szereg_sz)
  odchylenie_cwiartkowe = IQR(szereg_sz)/2
  wspolczynnik_zmiennosci =  100*odchylenie_standardowe/srednia
  pozycyjny_wspolczynnik_zmiennosci  =  100*odchylenie_cwiartkowe/mediana
  
  # Miary asymetrii i koncentracji
  skosnosc = (sum((szereg_sz - srednia) ^ 3) / length(szereg_sz)) / (odchylenie_standardowe ^ 3)
  kurtoza = (sum((szereg_sz - srednia) ^ 4) / length(szereg_sz)) / (odchylenie_standardowe ^ 4)
  eksces = kurtoza - 3
  
  return(c(srednia, mediana, moda, Q1, Q3, wariancja, odchylenie_standardowe, wariancja_es_nieobciazony, odchylenie_standardowe_NO, odchylenie_przecietne, odchylenie_przecietne_od_mediany, odchylenie_cwiartkowe, wspolczynnik_zmiennosci, pozycyjny_wspolczynnik_zmiennosci, skosnosc, kurtoza, eksces))
}


# utworzenie histogramu dla salonu 1
hist_salon1 <- hist(salon1, breaks = 8)
# utworzenie histogramu dla salonu 2
hist_salon2 <- hist(salon2, breaks = 8)


#funkcja dla kwantyli w szeregu rozdzielczym
kwantyl <- function(szereg_rozdz, q)
{
  war_srodkowe <- szereg_rozdz$mids
  liczebnosc <- szereg_rozdz$counts
  pozycja = sum(szereg_rozdz$counts) * q
  
  l_skumulowana <- cumsum(szereg_rozdz$counts)
  indeks <- which(pozycja < l_skumulowana)[1]
  
  dolna_granica = szereg_rozdz$breaks[indeks]
  l_skum_poprzedniego = l_skumulowana[indeks-1]
  liczebnoscp = szereg_rozdz$counts[indeks]
  szerokosc = szereg_rozdz$breaks[indeks+1] - szereg_rozdz$breaks[indeks]
  
  kwantyl = dolna_granica + ((pozycja - l_skum_poprzedniego) * (szerokosc / liczebnoscp))
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
  srednia = sum(liczebnosc * war_srodkowe) / sum(liczebnosc)
  mediana = kwantyl(szereg_roz,0.5)
  moda = war_srodkowe[which.max(liczebnosc)]
  Q1 = kwantyl(szereg_roz, 0.25)
  Q3 = kwantyl(szereg_roz, 0.75)

  # Miary rozproszenia
  wariancja = sum(((war_srodkowe-srednia) ^ 2) * liczebnosc) / sum(liczebnosc)
  odchylenie_standardowe = sqrt(wariancja)
  wariancja_es_nieobciazony = wariancja * (sum(liczebnosc)/(sum(liczebnosc)-1))
  odchylenie_standardowe_NO = sqrt(wariancja_es_nieobciazony)
  odchylenie_przecietne = sum(abs(war_srodkowe - srednia) * liczebnosc) / sum(liczebnosc)
  odchylenie_przecietne_od_mediany = sum(abs(war_srodkowe - mediana) * liczebnosc) / sum(liczebnosc)
  odchylenie_cwiartkowe = (Q3 - Q1) / 2
  wspolczynnik_zmiennosci = 100*odchylenie_standardowe/srednia
  pozycyjny_wspolczynnik_zmiennosci = 100*odchylenie_cwiartkowe/mediana
  
  # Miary asymetrii i koncentracji
  skosnosc = (sum((war_srodkowe - srednia) ^ 3 * liczebnosc) / sum(liczebnosc)) / (odchylenie_standardowe ^ 3)
  kurtoza = (sum((war_srodkowe - srednia) ^ 4 * liczebnosc) / sum(liczebnosc)) / (odchylenie_standardowe ^ 4)
  eksces = kurtoza - 3
  
  return(c(srednia, mediana, moda, Q1, Q3, wariancja, odchylenie_standardowe, wariancja_es_nieobciazony, odchylenie_standardowe_NO, odchylenie_przecietne, odchylenie_przecietne_od_mediany, odchylenie_cwiartkowe, wspolczynnik_zmiennosci, pozycyjny_wspolczynnik_zmiennosci, skosnosc, kurtoza, eksces))
}

Miary <- c("Średnia", "Mediana", "Moda", "Q1", "Q3", "Wariancja", "Odchylenie standardowe", "Wariancja (estymator nieobciążony)", "Odchylenie standardowe (estymator nieobciążony)", "Odchylenie przeciętne", "Odchylenie przeciętne od mediany", "Odchylenie ćwiartkowe", "Współczynnik zmienności", "Pozycyjny współczynnik zmienności", "Skośność", "Kurtoza", "Eksces")


Salon_1 <- miary_sz_szczegolowego(salon1)
Salon_2 <- miary_sz_szczegolowego(salon2)
df1 <- data.frame(Miary, Salon_1, Salon_2)

Salon_1r <- miary_sz_rozdzielczego(salon1)
Salon_2r <- miary_sz_rozdzielczego(salon2)
df2 <- data.frame(Miary, Salon_1r, Salon_2r)
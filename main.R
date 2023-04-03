miary_sz_szczegolowego(salon1)
miary_sz_szczegolowego(salon2)

miary_sz_rozdzielczego(salon1)
miary_sz_rozdzielczego(salon2)





miary <- c("srednia", "mediana", "moda", "Q1", "Q3", "wariancja", "odchylenie_standardowe", "wariancja_es_nieobciazony", "odchylenie_standardowe_NO", "odchylenie_przecietne", "odchylenie_przecietne_od_mediany", "odchylenie_cwiartkowe", "wspolczynnik_zmiennosci", "pozycyjny_wspolczynnik_zmiennosci", "skosnosc", "kurtoza", "eksces")
Salon_1 <- miary_sz_szczegolowego(salon1)
df <- data.frame(miary, Salon_1)

print(df)

print(Salon_1)
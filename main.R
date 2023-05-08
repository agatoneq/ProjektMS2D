Miary <- c("Średnia", "Mediana", "Moda", "Q1", "Q3", "Wariancja", "Odchylenie standardowe", "Wariancja (estymator nieobciążony)", "Odchylenie standardowe (estymator nieobciążony)", "Odchylenie przeciętne", "Odchylenie przeciętne od mediany", "Odchylenie ćwiartkowe", "Współczynnik zmienności", "Pozycyjny współczynnik zmienności", "Skośność", "Kurtoza", "Eksces")

Salon_1 <- miary_sz_szczegolowego(salon1)
Salon_2 <- miary_sz_szczegolowego(salon2)
df1 <- data.frame(Miary, Salon_1, Salon_2)
print(df1)

Salon_1r <- miary_sz_rozdzielczego(salon1)
Salon_2r <- miary_sz_rozdzielczego(salon2)
df2 <- data.frame(Miary, Salon_1r, Salon_2r)
print(df2)

plot(hist_salon1)
plot(hist_salon2)
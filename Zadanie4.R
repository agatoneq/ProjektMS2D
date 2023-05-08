salon2 <- c(133, 61, 66, 80, 50, 86, 86, 105, 82, 107, 93, 87, 94, 105, 62, 102, 70, 72, 122, 97, 99, 
            123, 96, 111, 73, 60, 107, 112, 50, 108, 77, 73, 90, 115, 71, 77, 87, 64, 115, 105, 77, 
            111, 77, 62, 84, 73, 94, 62, 125, 44, 102, 70, 62)


#zadanie 4
#Czy na poziomie istotności 0,05 można twierdzić, że odchylenie standardowe miesięcznych wydatków na prasę w drugim salonie jest równe 21?
zadanie4<-function(salon2)
{
  #dane z polecenia:
  alfa=0.05 #poziom istotnosci
  odch_st=21 #odchylenie standardowe z polecenia
  
  n = length(salon2) #dlugosc ciagu salon2 = ilosc obserwacji; n=53
  s2 = var(salon2) # wariancja = s^2 = (odchylenie standardowe obliczone z danych)^2; s2=464.4332
  
  #statystyka testowa:
  x2=(n-1)*s2/(odch_st^2)#x2=54.7631 ("chi-kwadrat")
  
  #kwartyle o ufnosci alfa i (n-1) stopniach swobody:
  kwartyl_g=qchisq(1-alfa/2,n-1) #kwartyl gorny; kwartyl_g=73.80986
  kwartyl_d=qchisq(alfa/2, n-1) #kwartyl dolny; kwartyl_d=33.96813
  
  #obszary krytyczne: (-nieskonczonosc, kwartyl_d)  (kwartyl_g, +nieskonczonosc)
  if(x2>kwartyl_d && x2<kwartyl_g) #jesli statystyka testowa (x2) nie nalezy do obszaru krytycznego:
    {return(c("Nie mozna odrzucic hipotezy, ze na poziomie istotnosci 0,05, odchylenie standardowe wynosi 21"))}
  else #jesli statystyka testowa nalezy do obszaru krytycznego:
    {return(c("Mozna odrzucic hipoteze, ze na poziomie istotnosci 0,05, odchylenie standardowe wynosi 21"))}
}
#wywolanie funkcji
zadanie4(salon2)
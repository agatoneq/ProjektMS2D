test_fs <- function(wektor1,wektor2,alfa)
{
  cat("\nTest fishera-snedecora\n")
  cat("Hipoteza zerowa: wariancje sa sobie rowne\n")
  cat("Hipoteza alternatywna: wariancja z pierwszego salonu jest większa\n")
  wariancja1=var(wektor1)
  wariancja2=var(wektor2)
  
  if(wariancja1>wariancja2) 
  {
    statystyka_f=wariancja1/wariancja2
    kwantyl_f=qf(1-alfa,length(wektor1)-1,length(wektor2)-1)
  }
  else if(wariancja1<=wariancja2)
  {
    statystyka_f=wariancja2/wariancja1
    kwantyl_f=qf(1-alfa,length(wektor2)-1,length(wektor1)-1)
  }
  cat("Wartosc statystyki:",statystyka_f)
  cat("\nWartość graniczna przdziału krytycznego: (", kwantyl_f,")\n")
  if(statystyka_f<kwantyl_f)
  {
    cat("Nie ma podstaw do odrzucenia hipotezy zerowej, przyjmujemy ze wariancje sa w przyblizeniu rowne\n")
    return (1)
  }
  else
  {
    cat("Sa podstawy do odrzucenia hipotezy zerowej, przyjmujemy ze wariancje nie sa rowne\n")
    return (0)
  }
}


test_ts <- function(wektor1,wektor2,alfa)
{

  cat("\nStatystyka t studenta\n")
  cat("hipoteza zerowa: srednie sa rowne\n")
  cat("Hipoteza alternatywna: srednia wartosc miesiecznych wydatkow w drugim salonie jest wieksza od sredniej wartosci miesiecznych wydatkow w pierwszym salonie\n")
  
  #zmienne pomocnicze
  s1<- mean(wektor1) 
  s2<- mean(wektor2) 
  w1<- var(wektor1)  
  w2<- var(wektor2)
  l1<- length(wektor1) 
  l2<- length(wektor2)
  
  statystyka=(s1-s2)/sqrt(((((l2*w2+l1*w1)/(l2+l1-2)))*((1/l2)+(1/l1))))
  kwantyl=-qt(1-alfa,df=length(wektor1)+length(wektor2)-2)
  
  cat("Wartosc statystyki T-studenta:",statystyka)
  cat("\nWartosc graniczna przedzialu krytycznego:", kwantyl, "\n Prezdział jest lewostronny\n")
  if(kwantyl<statystyka)
  {
    cat("Na poziomie istotnosci ",alfa,"nie ma podstaw by odrzucić hipoteze ze wartość miesiecznych wydatkow w drugim salonie jest wieksza\n")
  }
  else
  {
    cat("Na poziomie istotnosci ",alfa,"można odrzucić hipotezę ze wartość miesiecznych wydatkow w drugim salonie jest wieksza\n")
  }
}
test_cc <- function(wektor1,wektor2,alfa)
{
  cat("\nStatystyka Cochrana_Coxa\n")
  cat("hipoteza zerowa: srednie sa rowne\n")
  cat("Hipoteza alternatywna: srednia wartosc miesiecznych wydatkow w drugim salonie jest wieksza od sredniej wartosci miesiecznych wydatkow w pierwszym salonie\n")
  #zmienne pomocnicze
  s1<- mean(wektor1) 
  s2<- mean(wektor2) 
  w1<- var(wektor1)  
  w2<- var(wektor2)
  l1<- length(wektor1) 
  l2<- length(wektor2)
  
  statystyka=(s1-s2)/(sqrt(w1/l1+w2/l2))
  kwantyl=-((w1/l1)*qt(1-alfa,l1)+(w2/l2)*qt(1-alfa,l2))/((w1/l1)+(w2/l2))
  cat("Wartosc statystyki:",statystyka)
  cat("\nWartość graniczna przdziału krytycznego:",kwantyl, "\n Prezdział jest lewostronny\n")
  
  if(kwantyl<statystyka)
  {
    cat("Na poziomie istotnosci ",alfa,"brak podstaw do odrzucenia hipotezy zerowej\n")
  }
  else
  {
    cat("Na poziomie istotnosci ",alfa,"można odrzucić hipoteze zerową i przyjąć hipoteze alternatywną\n")
  }
}

test_permutacyjny <- function(wektor1,wektor2, alfa) {
  cat("\nTest permutacyjny\n")
  cat("hipoteza zerowa: srednie sa rowne\n")
  cat("Hipoteza alternatywna: srednia wartosc miesiecznych wydatkow w drugim salonie jest wieksza od sredniej wartosci miesiecznych wydatkow w pierwszym salonie\n")

  d1 <- length(wektor1)
  d2 <- length(wektor2)
  rs<- mean(wektor1) - mean(wektor2)  
  cat("Różnica średnich:",rs)
  
  wek_Laczny <- c(wektor1, wektor2)
  B <- 10000 
  wek_per<- numeric(B) 
  
  for (i in 1:B) {
    perm <- sample(wek_Laczny, replace = FALSE) 
    perm1 <- perm[1:d1] 
    perm2 <- perm[(d1+1):(d1+d2)]
    wek_per[i] <- mean(perm1) - mean(perm2) 
  }
  
    proby <- mean(abs(wek_per) >= rs) 
  
  cat("\nWartosc prób:",proby)
  if (proby > alfa) {
    cat("\nNa poziomie istotnosci ",alfa,"brak podstaw do odrzucenia hipotezy zerowej, średnie są równe\n")
  } else 
  {
    cat("\nNa poziomie istotnosci ",alfa,"można odrzucić hipoteze zerową i przyjąć hipoteze alternatywną\n")
  }
  
}



Zadanie5 <- function(wektor1,wektor2,alfa) 
{
  if(test_fs(wektor1,wektor2,alfa)==1) 
  {
    test_ts(wektor1,wektor2,alfa)
  }
  else
  {
    test_cc(wektor1,wektor2,alfa)
  }
  
  test_permutacyjny(wektor1,wektor2,alfa) 
  
}

zadanie4<-function(salon2)
{
  #dane
  alfa=0.05 
  odch_st=21 
  
  n = length(salon2)
  s2 = var(salon2) 
  
  #statystyka testowa:
  x2=(n-1)*s2/(odch_st^2)
  
  #kwartyle
  kwartyl_g=qchisq(1-alfa/2,n-1) 
  kwartyl_d=qchisq(alfa/2, n-1)
  
  #obszary krytyczne
  if(x2>kwartyl_d && x2<kwartyl_g)
    {return(c("Nie mozna odrzucic hipotezy, ze na poziomie istotnosci 0,05, odchylenie standardowe wynosi 21"))}
  else 
    {return(c("Mozna odrzucic hipoteze, ze na poziomie istotnosci 0,05, odchylenie standardowe wynosi 21"))}
}
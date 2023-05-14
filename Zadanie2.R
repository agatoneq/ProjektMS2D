test_K_L<-function(wektor){

  n=length(wektor)
  dane=sort(wektor)
  

  k=0.886/sqrt(n)

  f=pnorm((dane-mean(dane))/sd(dane))
  
  dp=max(abs(seq(1:n)/n-f))
  dm=max(abs(f-(seq(1:n)-1)/n))
  
  dn=max(dm,dp)
  
  cat("ilość danych: ",n, "\n")
  cat("Wartosc krytyczna: ",k, "\n")
  cat("Wartosc dn: ",dn, "\n")
  
  if(dn<k){cat("Miesieczne wydatki na prase maja rozklad normalny.\n")}
  else{cat("Miesieczne wydatki na prase nie maja rozkladu normalnego.\n")}
}
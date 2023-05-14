sd_ <- function(x) 
{
  n <- length(x)
  srednia <- mean(x) 
  suma <- sum((x - srednia)^2)        
  odchylenie <- sqrt(suma / (n - 1))
  return(odchylenie)
}

zad3 <- function(salon1)
{
v <- length(salon1) 

x_bar <- mean(salon1) 
cat("wartość średnia",x_bar)

os<-sd_(salon1)
cat("odchylenie standardowe",sd_(salon1))

lsw <- v-1
cat("liczba stopni swobody ",lsw) 

t <- ((x_bar - 85)*sqrt(lsw)) / (os)
cat("parametr t",t)

wk <- qt(0.025, lsw)
cat("wartość krytyczna",-wk)


if(wk>t || -wk<t)   {
  cat("Odrzucenie hipotezy zerowej na rzecz alternatywnej ")
} else {
  cat("Brak podstaw do odrzucenia hipoztezy zerowej ")
}
}

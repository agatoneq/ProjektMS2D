zadanie3 <- function(salon1)
{
v <- length(salon1) 

x_bar <- mean(salon1) 
cat("wartość średnia",x_bar, "\n")

os<-sd(salon1)
cat("odchylenie standardowe",sd(salon1), "\n")

lsw <- v-1
cat("liczba stopni swobody ",lsw, "\n") 

t <- ((x_bar - 85)*sqrt(lsw)) / (os)
cat("parametr t",t, "\n")

wk <- qt(0.025, lsw)
cat("wartość krytyczna",-wk, "\n")


if(wk>t || -wk<t)   {
  cat("Odrzucenie hipotezy zerowej na rzecz alternatywnej ")
} else {
  cat("Brak podstaw do odrzucenia hipoztezy zerowej ")
}
}
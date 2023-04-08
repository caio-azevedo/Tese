rm(list=ls())

library(Rsolnp)
w<-c(100,60)
n <- length(w)
set.seed(123)
a<-runif(n,0.3,0.5)
v<-1
L<-rep(720,n)
R<-2000

# Função Objetivo - Função Bem-estar Atkinson e Feldstein, com Utilidade Cobb Douglas
# Sinal negativo por ser uma maximização

objetivo <- function(x) {
  x1 <- x[1]; x2 <- x[2]; w<-w[seq(1,n,1)];L<-L[seq(1,n,1)];a<-a[seq(1,n,1)]
  -(sum(((a*(x1 + (1-x2)*w*L))^a * ((1-a)*(x1+(1-x2)*w*L)/((1-x2)*L))^(1-a))^v))^(1/v)
}


# Restrição: 
eq <- function(x){
  x1 <- x[1]; x2 <- x[2]; w<-w[seq(1,n,1)]; L<-L[seq(1,n,1)] ;a<-a[seq(1,n,1)]
  R + sum(x1 - x2*w*(L-((1-a)*(x1+(1-x2)*w*L)/((1-x2)*w))))
}

ineq<-function(x){
  x1 <- x[1]; x2 <- x[2]; w<-w[seq(1,n,1)]; L<-L[seq(1,n,1)] ;a<-a[seq(1,n,1)]
  (1-a)*(x1+(1-x2)*w*L)/((1-x2)*w)
}

#Limite da restrição
restricao <- c(0)

# Limite inferior (lower) e superior (upper) da restrição de desigualdade
lx <- c(0,0)
ux <- c(0.2*R,1)

# Valores iniciais para o algoritmo
init <- c(100,0.05)

####     SOLUÇÃO     #####


solucao<- Rsolnp::solnp(pars = init, fun = objetivo, eqfun = eq, eqB = 0,
                        LB=lx, UB=ux)

# Valores da função objetivo em cada iteração
teste<-solucao[["pars"]]

trabalho<- function(B,t){
  print(Lz<-(1-a[1])*(B+(1-t)*w[1]*L[1])/((1-t)*w[1]))
}

trabalho(B=teste[1],t=teste[2])


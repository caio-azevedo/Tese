rm(list=ls())

library(Rsolnp)

alpha<-0.75
X1<- 150
X2<- 210
n <- 6

# Função Objetivo - Função Bem-estar Utilitarista com Utilidade Cobb Douglas
# Sinal negativo por ser uma maximização

objetivo <- function(x) {
x1 <- x[seq(1,n,2)]; x2 <- x[seq(2,n,2)] 
-sum(x1^alpha * x2^(1-alpha))
}


# Restrição: 
ineq <- function(x){
x1 <- x[seq(1,n,2)]; x2 <- x[seq(2,n,2)]
c(sum(x1),
  sum(x2))
}

#Limite da restrição
restricao <- c(X1,X2)

# Limite inferior (lower) e superior (upper) da restrição de desigualdade
lx <- rep(0,n)
ux <- c(rep(X1,n/2),rep(X2,n/2))

# Valores iniciais para o algoritmo
init <- rep(1,n)

####     SOLUÇÃO     #####


solucao<- Rsolnp::solnp(pars = init, fun = objetivo, 
                        ineqfun = ineq,ineqLB = rep(0,2),
                        ineqUB = restricao, LB=lx)

# Valores da função objetivo em cada iteração
solucao$par


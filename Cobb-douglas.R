rm(list=ls())

library(Rsolnp)

alpha<-0.75
# Função Objetivo - Cobb Douglas
# Sinal negativo por ser uma maximização
objetivo <- function(x){
  -(x[1]^alpha*x[2]^(1-alpha))
}
# Restrição: 
ineq <- function(x){
  3*x[1]+5*x[2]
}

#Limite da restrição
restricao <- c(500)

# Limite inferior (lower) e superior (upper) da restrição de desigualdade
lx <- c(0,0)
ux <- c(100,100)

# Valores iniciais para o algoritmo
init <- c(1,1)

####     SOLUÇÃO     #####


solucao<- Rsolnp::solnp(pars = init, fun = objetivo, 
                        ineqfun = ineq,ineqLB = 0,
                        ineqUB = restricao, LB=lx)

# Valores da função objetivo em cada iteração
solucao$par

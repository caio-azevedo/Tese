# Limpar
rm(list=ls())

# Pacotes
library(Pareto)
library(tidyverse)


# Dados
load("data/dadosPNADc.rData")
pnad <- dadosPNADc$variables
pnad<-pnad$VD4020

rm(dadosPNADc)

#Estimando índice de pareto por MMV

pnad<-data.frame(pnad)
dados<-pnad[complete.cases(pnad),]
dados<-as.data.frame(dados)
dados<-dados %>% 
  filter(dados>=900 & dados<50000) 

x<-dados[,1]


neg_log_lik <- function(x, alpha){
  # parâmetros para a distribuição de Pareto
  A <- min(x)
  n <- length(x)
  # log da verossimilhança da Pareto
  ll <- n*log(alpha)+n*alpha*log(A)-(alpha+1)*sum(log(x))
  # retornar o negativo para maximizar ao invés de minimizar 
  return(-ll)
}

fit <- stats::optim(par = 1, fn = neg_log_lik, x = x, 
                    method = "BFGS", hessian = TRUE)

MV<-fit$par

# Número de arquivos que você deseja criar
pop<-seq(100000,200000,50000)
arquivo<-vector('list', length(pop))

# Use um loop "for" para criar os arquivos
for (i in 1:length(pop)) {
  # Crie o nome do arquivo
  arquivo[[i]] <- paste0("data/Pareto/pareto_", i, ".csv")
  i<-i+1
}

# Criando as distribuições de Rendas teóricas


for (i in c(1:length(pop))) {
  
  set.seed(1)
  pareto<-data.frame(renda=rPareto(0.8*pop[i],900,MV,50000))
  set.seed(1)
  uniforme<-data.frame(renda=runif(0.2*pop[i],0,900))
  pareto<-rbind(uniforme,pareto)
  
  write.csv(pareto, file=arquivo[[i]], row.names = FALSE)
  }
# Limpar
rm(list=ls())


#Diretório
setwd("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/Tese")

library(Pareto)
library(PNADcIBGE)
library(dplyr)
library(ggplot2)
library(survey)


options(scipen = 999)

#Baixando dados da PNAD Contínua

dadosPNADc <- get_pnadc(year=2021, quarter=3, vars=c("VD4020", "V2007"))

#save(dadosPNADc,file='data/dadosPNADc.rData')

load("data/dadosPNADc.rData")


#Média
mediarenda <- svymean(x=~VD4020, design=dadosPNADc, na.rm=TRUE)
mediarenda

# Mediana

medianarenda <- svyquantile(~VD4020, dadosPNADc, quantiles = .5, na.rm = T, ci = TRUE)
medianarenda

#Quartis 

quantisrenda <- svyquantile(~VD4020, dadosPNADc, quantiles = c(.25,.75), na.rm = T)
quantisrenda

#Mínimo e máximo
pnad <- dadosPNADc$variables
pnad<-pnad$VD4020

min(pnad, na.rm=T)
max(pnad, na.rm=T)


#Decis

decisrenda <- svyquantile(~VD4020, dadosPNADc, 
                            quantiles = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1), na.rm = T)
decisrenda

# Histograma

svyhist(~ as.numeric(VD4020), main="",dadosPNADc, xlab = "Rendimento Mensal Efetivo (em R$)", 
        ylab="Densidade", freq = FALSE, xlim=c(0,30000),breaks = 500)

dev.copy(pdf,"graf1.pdf")
dev.off()

svyhist(~ as.numeric(VD4020), main="",dadosPNADc, xlab = "Rendimento Mensal Efetivo (em R$)", 
        ylab="Densidade", freq = FALSE, xlim=c(0,10000),breaks = 500)

dev.copy(pdf,"graf2.pdf")
dev.off()

# Boxsplot

svyboxplot(VD4020 ~ 1, dadosPNADc, main = "", ylim=c(0,5000))
dev.copy(pdf,"graf3.pdf")
dev.off()


#Estimando índice de pareto por MMV

pnad<-data.frame(pnad)
dados<-pnad[complete.cases(pnad),]
dados<-as.data.frame(dados)
dados<-dados %>% 
  filter(dados>=900 & dados<50000) 
x<-as.vector(dados)
x<-x[,1]


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

a<-fit$par


fisher.information.fit <- solve(fit$hessian)
standard.deviance.fit <- sqrt(diag(fisher.information.fit))


t.fit <- fit$par/standard.deviance.fit
pvalue.fit <- 2*(1-pt(abs(t.fit), length(x)-length(fit$par)))
results.fit <- cbind(fit$par, standard.deviance.fit, t.fit, pvalue.fit)
colnames(results.fit) <- c("parâmetros", "desvio-padrão", "estatística t", "p-valor")
rownames(results.fit) <- c("alpha")
print(results.fit, digits = 3)


# Gerar valores distribuição de Pareto
set.seed(1)
pareto<-data.frame(renda=rPareto(800000,900,a,50000))
mean(pareto$renda)

set.seed(1)
uniforme<-data.frame(renda=runif(200000, 0,900))
mean(uniforme$renda)




pareto<-rbind(uniforme,pareto)

hist(pareto$renda, xlab = "Rendimento Mensal Efetivo (em R$)", main="",
     ylab="Densidade", freq = FALSE, xlim=c(0,30000),breaks = 150)
dev.copy(pdf,"graf5.pdf")
dev.off()

hist(pareto$renda, xlab = "Rendimento Mensal Efetivo (em R$)", main="",
     ylab="Densidade", freq = FALSE, xlim=c(0,10000),breaks = 150)
dev.copy(pdf,"graf6.pdf")
dev.off()


summary(pareto)




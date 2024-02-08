library(Rsolnp)
library(tidyverse)
library(future.apply)

# Ative o plano futuro para paralelismo
plan(multisession, workers = 6)

# Funções vetorizadas ajustadas
Lz <- function(B, t1, t2, w, L, a, w_lim) {
  ifelse(w <= w_lim, ((1 - a) * (B + (1 - t1) * w * L) / ((1 - t1) * w)), 
         ((1 - a) * (B + (1 - t2) * w * L) / ((1 - t2) * w)))
}

C <- function(B, t1, t2, w, L, a, w_lim) {
  ifelse(w <= w_lim, a * (B + (1 - t1) * w * L), 
         a * (B + (1 - t2) * w * L))
}

# A função U usa as novas definições de Lz e C

# Carregar os nomes dos arquivos de dados
tabelas <- list.files("data/Pareto", full.names = TRUE)

# Sequência de alphas para iterar
alpha_seq <- seq(0.30, 0.60, 0.01)

# Processamento dos dados e otimização para cada tabela e alpha em paralelo
results <- future_lapply(tabelas, function(tabela) {
  load(tabela) # Supondo que isso carregue 'pareto'
  w <- pareto$renda / 240
  n <- length(w)
  L <- rep(720, n)
  R <- 10 * n
  
  map_dfr(alpha_seq, function(alpha) {
    a <- rep(alpha, n)
    init <- c(100, 0.1, 0.2, median(w)) # Inicialização inclui w_lim
    
    eq <- function(x) {
      B <- x[1]
      t1 <- x[2]
      t2 <- x[3]
      w_lim <- x[4]
      R + sum(B - ifelse(w <= w_lim, t1 * w * (L - Lz(B, t1, t2, w, L, a, w_lim)), t2 * w * (L - Lz(B, t1, t2, w, L, a, w_lim))))
    }
    
    solucao <- solnp(pars = init, 
                     fun = function(x) -sum(U(x[1], x[2], x[3], w, L, a, x[4])),
                     eqfun = eq, eqB = 0, 
                     LB = c(0, 0, 0, min(w)), UB = c(Inf, 1, 1, max(w)), 
                     control = list(outer.iter = 200))
    
    tibble(Alpha = alpha, B = solucao$pars[1], t1 = solucao$pars[2], t2 = solucao$pars[3], w_lim = solucao$pars[4], Iteracoes = solucao$outer.iter, Lagrange = list(solucao$lagrange))
  }, .id = "ID")
}, future.seed = TRUE)

# Combinando todos os resultados em um único data frame
final_df <- bind_rows(results, .id = "Tabela")

# Exibindo os resultados
print(final_df)
save(final_df, file="data/final_df.RData")

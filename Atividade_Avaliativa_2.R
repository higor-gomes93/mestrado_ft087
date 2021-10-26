# Planejamento Fatorial 2^2

#--------------------------------------------------------------------------------#

### Construção do Experimento
# Níveis
levels <- c(-1, 1)

# Planejamento
plan <- expand.grid(levels, levels)
plan <- rbind(plan, plan, plan)

# Nomes das colunas
colnames(plan) <- c("C", "T")

# Respostas
y <- c(26.6, 40.9, 11.8, 34.0,
       22.0, 36.4, 15.9, 29.0,
       22.8, 36.7, 14.3, 33.6)

plan$y <- y
plan

#--------------------------------------------------------------------------------#

### Análise do Experimento
# Matriz do Planejamento
X <- model.matrix(~C*T, data = plan[,-3])

# Efeitos
efeitos <- crossprod(X, y)/(3*2^2/2)

# Coeficientes
coef <- efeitos/2

# Valores ajustados
fitted <- X%*%coef

# Resíduos
resi <- y - fitted

# Número de ensaios (N) e de termos no modelo (r)
N <- dim(X)[1]
r <- dim(X)[2]

## Soma dos quadrados
# SS dos resíduos
SSE <- sum(resi^2)

# SS total
SST <- sum(y^2) - sum(y)^2/N

## Graus de liberdade
# Dos erros
DFE <- N - r

# Total
DFT <- N - 1

## Média dos quadrados
# Dos erros
MSE <- SSE/DFE

# Total
MST <- SST/DFT

# t calculado
t0 <- coef/sqrt(MSE/N)
t0

# t crítico
t_critico <- qt(0.05, df = DFE, lower.tail = F)
t_critico

# pvalue
pvalue <- 2*pt(abs(t0), df = DFE, lower.tail = F)
pvalue

# Dataframe resumo do teste t
testet <- data.frame(coef, rep(sqrt(MSE/N), 4), t0, pvalue)
colnames(testet) <- c("Coef", "SE_coef", "t0", "p-value")

# Coef de determinação múltipla
R2 <- 1 - SSE/SST
R2_aj <- 1 - MSE/MST

#--------------------------------------------------------------------------------#

## Anova
# soma dos quadrados dos efeitos
SS_x <- crossprod(X[,-1], y)^2/N

# Média dos quadrados dos efeitos
MS_x <- SS_x/1

# F Calculado
F0 <- MS_x/MSE
F0

# pvalor
p <- pf(F0, 1, DFE, lower.tail = F)
p

# Tabela resumo ANOVA
Source <- c("C", "T", "CT", "Erro", "Total")
SS <- c(SS_x, SSE, SST)
DF <- c(rep(1, 3), DFE, DFT)
MS <- c(MS_x, MSE, MST)
F0 <- c(F0, NA, NA)
pvalor <- c(p, NA, NA)

ANOVA <- data.frame(SS, DF, MS, F0, pvalor)
rownames(ANOVA) <- Source
ANOVA

#--------------------------------------------------------------------------------#

## Mínimos quadrados matricial
# Multiplicando X transposta por X
t(X)%*%X

# Inversa do resultado anterior
solve(t(X)%*%X)

# Multiplicando X transposta por y
t(X)%*%y

# Obtendo os coeficientes
beta_mat <- solve(t(X)%*%X)%*%t(X)%*%y
beta_mat
  
# Previsão para todos os resultados experimentais
y_hay <- X%*%beta_mat

# Previsão para valores de C e T específicos
# C = 0, T = 0.5
x_esp <- matrix(c(1, 0, 0.5, 0), nrow = 4, ncol = 1)
y_esp_hat <- t(x_esp)%*%beta_mat
y_esp_hat

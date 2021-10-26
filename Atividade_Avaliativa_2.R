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
efeitos <- crossprod(X, y)/(2*2^2/2)

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





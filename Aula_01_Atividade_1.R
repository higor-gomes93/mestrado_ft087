# Criação do conjunto de dados

dados <- c(20.6, 21.1, 22.7, 22.2, 23.1, 20.9, 21.6, 21.8, 22.1, 22.9)

# Média Aritimética
media <- mean(dados)
media_exemplo <- 22.08

# Mediana
mediana <- median(dados)
mediana_exemplo <- 22.1
  
# Moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
  
moda <- getmode(dados)
moda_exemplo <- NULL
  
# Amplitude
amplitude <- max(dados) - min(dados)
amplitude_exemplo <- 2.2

# Variância
variancia <- var(dados)
variancia_exemplo <- 0.762

# Desvio Padrão
desvio_padrao <- sd(dados)
desvio_padrao_exemplo <- 0.873
  
# Erro Padrão
erro_padrao <- desvio_padrao/sqrt(length(dados))
erro_padrao_exemplo <- 0.39
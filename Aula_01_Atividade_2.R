# Análise de uma base de dados

# Importação das bibliotecas
library(tidyr)
library(ggplot2)
library(gridExtra)

# Importação do dataset
dataset_list <- read.csv(file = "c:/Users/Usuario/Documents/Estudos/Data Science e Programação/Spotify API/playlists_songs_clusters.csv")
dataset <- as.data.frame(dataset_list)

# Selecionando uma amostra aleatória de 50 observações
amostra = dataset[sample(nrow(dataset), 50), ]

# Eliminando o atributo categórico
amostra$genre <- NULL
amostra$X <- NULL
row.names(amostra) <- NULL

# Exportando o dataset como imagem
png("c:/Users/Usuario/Documents/Mestrado/Disciplinas/FT087 - Planejamento e Análise Experimental/Documentos/dataset.png", height = 23*nrow(amostra), width = 75*ncol(amostra))
grid.table(amostra)
dev.off()

# Construindo a função que irá realizar operações em cada coluna do dataset
funcao_geral <- function(operation){
  # Criando o vetor de armazenamento dos resultados
  auxiliar_vector <- c()
  for (element in colnames(amostra)) {
    # Selecionando a coluna de interesse
    coluna <- amostra[element]
    # Realizando a operação
    auxiliar_vector <- c(auxiliar_vector, operation(coluna[,]))
  }
  return(auxiliar_vector)
}

# Construindo a função de cálculo de moda
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Construindo a função de cálculo de amplitude
amplitude <- function(v) {
  max(v) - min(v)
}

# Construindo a função de cálculo do erro padrão
erro_padrao <- function(v) {
  sd(v)/sqrt(length(v))
}

# Construindo o dataset com as medidas
medidas <- c("Features", "Média", "Mediana", "Moda", "Desvio Padrão", "Variância", "Erro Padrão", "Amplitude")
features <- colnames(amostra)

media <- funcao_geral(mean)
mediana <- funcao_geral(median)
moda <- funcao_geral(moda)
desv_pad <- funcao_geral(sd)
variancia <- funcao_geral(var)
err_pad <- funcao_geral(erro_padrao)
amp <- funcao_geral(amplitude)

dataframe <- data.frame(linhas, media, mediana, moda, desv_pad, variancia, err_pad, amp)
colnames(dataframe) <- medidas

# Exportando o dataset como imagem
png("c:/Users/Usuario/Documents/Mestrado/Disciplinas/FT087 - Planejamento e Análise Experimental/Documentos/dataset_medidas.png", height = 25*nrow(dataframe), width = 100*ncol(dataframe))
grid.table(dataframe)
dev.off()

# Construindo o histograma
amostra %>% gather()
ggplot(gather(amostra), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

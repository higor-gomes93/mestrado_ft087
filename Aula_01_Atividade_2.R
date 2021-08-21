# Análise de uma base de dados

# Importação do dataset
dataset_list <- read.csv(file = "c:/Users/Usuario/Documents/Estudos/Data Science e Programação/Spotify API/playlists_songs_clusters.csv")
dataset <- as.data.frame(dataset_list)

# Selecionando uma amostra aleatória de 50 observações
amostra = dataset[sample(nrow(dataset), 50), ]

# Eliminando o atributo categórico
amostra$genre <- NULL

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
  desvio_padrao/sqrt(length(v))
}

# Construindo o dataset com as medidas
medidas <- c("Média", "Mediana", "Moda", "Desvio Padrão", "Variância", "Erro Padrão", "Amplitude")
linhas <- colnames(amostra)
media <- funcao_geral(mean)
mediana <- funcao_geral(median)

dataframe <- data.frame()






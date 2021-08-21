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

teste <- funcao_geral(mean)
teste
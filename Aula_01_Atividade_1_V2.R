#CRIANDO FUNÇÃO PARA MODA
getmoda <- function(ComprimentoRaiz){
  uniqComprimentoRaiz <- unique(ComprimentoRaiz)
  uniqComprimentoRaiz[which.max(tabulate(match(ComprimentoRaiz, uniqComprimentoRaiz)))]
}

#CRIANDO FUNÇÃO PARA MEDIANA
getmediana <- function(ComprimentoRaiz){
  qtdElem <- length(ComprimentoRaiz)
  ComprimentoRaiz <- sort(ComprimentoRaiz)
  if (qtdElem %% 2 == 0){
    med <- (ComprimentoRaiz[qtdElem/2]+ComprimentoRaiz[qtdElem/2+1])/2
  }else{
    med <- (ComprimentoRaiz[(n+1)/2])
  }
  return(med)
}

#CRINDO VETORES A SER MANIPULADO
ComprimentoRaiz <- c(20.6, 21.1, 22.7, 22.2, 23.1, 20.9, 21.6, 21.8, 22.1, 22.9)

#QUANTIDADE DE ELEMENTOS NO VETOR
qtdElem <- length(ComprimentoRaiz)

#ELEMENTOS DO VETOR (MAXIMA, MINIMA, RANGE, QUANTIDADE)
max(ComprimentoRaiz)
min(ComprimentoRaiz)
range(ComprimentoRaiz)
qtdElem

#MEDIA NATIVA
mean(ComprimentoRaiz)
media_exemplo <- 22.08

#MEDIANA NATIVA OU MEDIANA CRIADA 
median(ComprimentoRaiz)
getmediana(ComprimentoRaiz)
mediana_exemplo <- 22.1

#MODA CRIADA
moda <- getmoda(ComprimentoRaiz)
print(moda)
moda_exemplo <- NULL

#AMPLITUDE
amplitude <- max(ComprimentoRaiz) - min(ComprimentoRaiz)
print(amplitude)
amplitude_exemplo <- 2.2

#VARIANCIA
variancia <- var(ComprimentoRaiz)
print(variancia)
variancia_exemplo <- 0.762

#DESVIO PADRÃO
desvioPadrao <- sd(ComprimentoRaiz,na.rm = FALSE)
print(desvioPadrao)
desvio_padrao_exemplo <- 0.873

#ERRO PADRÃO
erroPadrao <- sd(ComprimentoRaiz)/sqrt(qtdElem)
print (erroPadrao)
erro_padrao_exemplo <- 0.39
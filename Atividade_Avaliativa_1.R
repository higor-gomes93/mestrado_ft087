# Importação das Bibliotecas
library(ggplot2)

# Criando o dataset_1
dataset_1 <- data.frame("Variedades" = c("A", "B", "C", "D"),
                        "R1" = c(25, 31, 22, 33),
                        "R2" = c(26, 25, 26, 29),
                        "R3" = c(20, 28, 28, 31),
                        "R4" = c(23, 27, 25, 34),
                        "R5" = c(21, 24, 29, 28))

dataset_2 <- as.data.frame(t(as.matrix(dataset_1[1:4, 2:6])))
colnames(dataset_2) <- c("A", "B", "C", "D")

# Preparando os dados para gerar o boxplot
r1_label <- replicate(4, "R1")
r2_label <- replicate(4, "R2")
r3_label <- replicate(4, "R3")
r4_label <- replicate(4, "R4")
r5_label <- replicate(4, "R5")
r1_data <- t(as.matrix(dataset_2[1,1:4]))
r2_data <- t(as.matrix(dataset_2[2,1:4]))
r3_data <- t(as.matrix(dataset_2[3,1:4]))
r4_data <- t(as.matrix(dataset_2[4,1:4]))
r5_data <- t(as.matrix(dataset_2[5,1:4]))
varied <- c(r1_data, r2_data, r3_data, r4_data, r5_data)

dataset_box_2 <- data.frame("Variedades" = c(r1_label, r2_label, r3_label, r4_label, r5_label), "Dados" = c(varied))

# Gerando o boxplot
ggplot(dataset_box_2, aes(x=Variedades, y=Dados)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Variedades")+
  ylab("Produção de Soja em kg/100m²")

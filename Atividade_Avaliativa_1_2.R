# Importação das Bibliotecas
library(ggplot2)

# Criando o dataset_1
dataset_1 <- data.frame("Técnicas" = c("E1", "E2", "E3", "E4", "E5"),
                        "A" = c(132, 129, 102, 116, 127),
                        "B" = c(142, 102, 91, 149, 113),
                        "C" = c(61, 90, 75, 84, 27),
                        "D" = c(38, 33, 21, 18, 22))

dataset_2 <- as.data.frame(t(as.matrix(dataset_1[1:5, 2:5])))
colnames(dataset_2) <- c("E1", "E2", "E3", "E4", "E5")

# Preparando os dados para gerar o boxplot
r1_label <- replicate(5, "A")
r2_label <- replicate(5, "B")
r3_label <- replicate(5, "C")
r4_label <- replicate(5, "D")
r1_data <- t(as.matrix(dataset_2[1,1:5]))
r2_data <- t(as.matrix(dataset_2[2,1:5]))
r3_data <- t(as.matrix(dataset_2[3,1:5]))
r4_data <- t(as.matrix(dataset_2[4,1:5]))
varied <- c(r1_data, r2_data, r3_data, r4_data)

dataset_box <- data.frame("Variedades" = c(r1_label, r2_label, r3_label, r4_label), "Dados" = c(varied))

# Gerando o boxplot
ggplot(dataset_box, aes(x=Variedades, y=Dados)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Técnicas")+
  ylab("Falhas")



# Preparando os dados para gerar o boxplot
r1_label_2 <- replicate(4, "E1")
r2_label_2 <- replicate(4, "E2")
r3_label_2 <- replicate(4, "E3")
r4_label_2 <- replicate(4, "E4")
r5_label_2 <- replicate(4, "E5")
r1_data_2 <- dataset_2[1:4,1]
r2_data_2 <- dataset_2[1:4,2]
r3_data_2 <- dataset_2[1:4,3]
r4_data_2 <- dataset_2[1:4,4]
r5_data_2 <- dataset_2[1:4,5]
varied_2 <- c(r1_data_2, r2_data_2, r3_data_2, r4_data_2, r5_data_2)

dataset_box_2 <- data.frame("Variedades" = c(r1_label_2, r2_label_2, r3_label_2, r4_label_2, r5_label_2), "Dados" = c(varied_2))

# Gerando o boxplot
ggplot(dataset_box_2, aes(x=Variedades, y=Dados)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Amostras")+
  ylab("Falhas")
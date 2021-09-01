# Boxplots da Atividade 2

# Importação das Bibliotecas
library(ggplot2)

# Criando o dataset_1
dataset_1 <- data.frame("Espécie Vegetal" = c("EV1", "EV2"),
                      "P1" = c(35, 31),
                      "P2" = c(19, 26),
                      "P3" = c(31, 39),
                      "P4" = c(15, 27),
                      "P5" = c(20, 20),
                      "P6" = c(30, 29),
                      "P7" = c(40, 45),
                      "P8" = c(35, 30),
                      "P9" = c(18, 28),
                      "P10" = c(33, 43))

# Preparando os dados para gerar o boxplot
ev1_label <- replicate(10, "EV1")
ev2_label <- replicate(10, "EV2")
ev1_data <- t(as.matrix(dataset_1[1,2:11]))
ev2_data <- t(as.matrix(dataset_1[2,2:11]))
ev <- c(ev1_data, ev2_data)

dataset_box_1 <- data.frame("Espécie" = c(ev1_label, ev2_label), "Dados1" = c(ev))

# Gerando o boxplot
ggplot(dataset_box_1, aes(x=Espécie, y=Dados1)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Espécies")


# Criando o dataset_2
dataset_2 <- data.frame("Máquina" = c("M1", "M2", "M3", "M4"),
                        "T1" = c(8.1, 8.4, 8.8, 8.3),
                        "T2" = c(8.3, 8.4, 8.9, 8.2),
                        "T3" = c(8.0, 8.3, 8.8, 8.2),
                        "T4" = c(8.1, 8.4, 8.8, 8.3),
                        "T5" = c(8.1, 8.3, 8.8, 8.3),
                        "T6" = c(8.2, 8.4, 8.8, 8.3))

# Preparando os dados para gerar o boxplot
m1_label <- replicate(6, "M1")
m2_label <- replicate(6, "M2")
m3_label <- replicate(6, "M3")
m4_label <- replicate(6, "M4")
m1_data <- t(as.matrix(dataset_2[1,2:7]))
m2_data <- t(as.matrix(dataset_2[2,2:7]))
m3_data <- t(as.matrix(dataset_2[3,2:7]))
m4_data <- t(as.matrix(dataset_2[4,2:7]))
maq <- c(m1_data, m2_data, m3_data, m4_data)

dataset_box_2 <- data.frame("Máquina" = c(m1_label, m2_label, m3_label, m4_label), "Dados2" = c(maq))

# Gerando o boxplot
ggplot(dataset_box_2, aes(x=Máquina, y=Dados2)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Máquinas")

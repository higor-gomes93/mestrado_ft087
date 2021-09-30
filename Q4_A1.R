# Primeiro Passo: Criação da base de dados
x<-data.frame("linha"=c("S1","S1","S1","S1","S1",
                        "S2","S2","S2","S2","S2",
                        "S3", "S3", "S3", "S3","S3",
                        "S4","S4","S4","S4", "S4",
                        "S5","S5","S5","S5", "S5"),
              "coluna"=c("W1", "W2", "W3", "W4","W5",
                         "W1", "W2", "W3", "W4","W5",
                         "W1", "W2", "W3", "W4","W5",
                         "W1", "W2", "W3", "W4","W5",
                         "W1", "W2", "W3", "W4","W5"),
              "tratamento"=c("A", "E", "D", "C", "B",
                             "B", "A", "E", "D", "C",
                             "C", "B", "A", "E", "D",
                             "D", "C", "B", "A", "E",
                             "E", "D", "C", "B", "A"),
              "residuos"=c(19,54,66,42,29,
                           21,18,68,41,28,
                           53,22,23,58,35,
                           51,42,29,17,61,
                         54,48,33,43,12))


# Segundo Passo: Organização dos dados
x$linha <- as.factor(x$linha) 
x$coluna <- as.factor(x$coluna) 
x$tratamento <- as.factor(x$tratamento)

# Terceiro Passo: análise de variância
analise <-lm(residuos ~ linha+coluna+tratamento, x)
anova(analise)

# Quarto Passo: Teste de Tukey
TukeyHSD(aov(analise))
plot(TukeyHSD(aov(analise)))

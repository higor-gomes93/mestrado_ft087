# Criando um data.frame
AtividadeF <- data.frame(Aditivo = c("A", "B","C","D","E"),
                 I = c(4, 5, 5, 4, 5),
                 II = c(4, 4, 4, 5, 3),
                 III = c(4, 4, 4, 4, 4),
                 IV = c(3, 3, 4, 5, 4),
                 V = c(4, 3, 2, 2, 4))

dataset <- c(AtividadeF$I, AtividadeF$II, AtividadeF$III, AtividadeF$IV, AtividadeF$V)
shapiro.test(dataset)
# Criando um data.frame elevado ao quadrado
Quadrado <- data.frame(I = AtividadeF$I^2,
                       II = AtividadeF$II^2,
                       III = AtividadeF$III^2,
                       IV = AtividadeF$IV^2,
                       V = AtividadeF$V^2)
# Inserindo a coluna Soma
AtividadeF$Soma <- 0
AtividadeF$Media <- 0

for (i in 1:5){
  for(j in 2:6)
    AtividadeF[i,7]<-AtividadeF[i,7]+AtividadeF[i,j]
    AtividadeF[i,8]<-AtividadeF[i,7]/5
}
SomaColuna <- sum(AtividadeF$Soma)

# Inserindo a coluna Media

Quadrado$Total <- sum(Quadrado)
MediaColuna <- round(sum(AtividadeF$Media/5), 2)
SomaValores <- round((SomaColuna^2)/(i*(j-1)), 2) 

SQTotal <- round((Quadrado$Total[1] - SomaValores),2)
SQT <- round(((AtividadeF$Soma[1]^2/(j-1) + 
                AtividadeF$Soma[2]^2/(j-1) + 
                AtividadeF$Soma[3]^2/(j-1) + 
                AtividadeF$Soma[4]^2/(j-1) + 
                AtividadeF$Soma[5]^2/(j-1)) - 
                SomaColuna^2/(i*(j-1))),3)
SQR <- round(SQTotal - SQT,2)
QMT <- round(SQT/(i-1),3)
QMR <- round(SQR/(i*(i-1)),3)
Fcal <- round(QMT/QMR,3)

#Fazendo o grafico por amostra
boxplot(AtividadeF$I, 
        AtividadeF$II, 
        AtividadeF$III, 
        AtividadeF$IV, 
        AtividadeF$V, 
        main ="Atividade Fisica", 
names = c("I", "II", "III", "IV", "V"), xlab = "Grupo por Idades", ylab = "Performace")

#Defindo matriz transposta
rownames(AtividadeF)<-AtividadeF$Aditivo
AtividadeF$Aditivo <- NULL
AtividadeF$Soma <- NULL
AtividadeF$Media <- NULL
AtividadeF_transpose <-as.data.frame(t(as.matrix(AtividadeF)))
boxplot(AtividadeF_transpose$A, 
        AtividadeF_transpose$B, 
        AtividadeF_transpose$C,
        AtividadeF_transpose$D,
        AtividadeF_transpose$E,
        main ="Atividade F?sica", 
        names = c("A","B","C","D","E"), 
        xlab = "Aditivo Vitaminico", 
        ylab = "Efeito")

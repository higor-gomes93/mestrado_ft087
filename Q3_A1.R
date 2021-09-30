# Primeiro Passo: Criação da base de dados
dad <-c(43, 63, 55,
        86, 69, 61,
        98, 79, 79,
        96, 81, 79,
        92, 98, 91)
dad

# Segundo Passo: Definição dos Blocos
bloc <- gl(5, 3, label = c(paste("T", 1:5))) #5 blocos (Trações)
bloc

# Terceiro Passo: Definição dos Tratamentos
trat <- rep(paste("S", 1:3), 5) #3 tratamentos (Organismos)
trat

# Quarto Passo: Criação da Tabela
tabela <- data.frame(blocos = bloc, tratamentos = factor(trat), dados = dad)
tabela

# Quinto Passo: Geração da Anova
resultado <- aov(dados ~ tratamentos + blocos, tabela)
resultado

# Sexto Passo: Aplicação de Tukey
anova(resultado)
TukeyHSD(resultado)
plot(TukeyHSD(resultado))
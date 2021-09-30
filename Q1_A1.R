# Primeiro Passo: criando as colunas
A <- c(117,120,114,119,115)
B <- c(115,110,116,115,114)
C <- c(118,123,119,122,118)
D <- c(125,121,123,118,118)

# Segundo Passo: realizando os testes de Shapiro-Wilk para cada coluna
shapiro.test(A)
shapiro.test(B)
shapiro.test(C)
shapiro.test(D)

# Terceiro Passo: criando uma matriz com todos os dados
Dados <- c(117,120,114,119,115,
           115,110,116,115,114,
           118,123,119,122,118,
           125,121,123,118,118)
Voltagem <- c(rep("A",5), rep("B",5), rep("C",5), rep("D",5))

# Quarto Passo: realizando o teste de Bartlett
bartlett.test(Dados~Voltagem)

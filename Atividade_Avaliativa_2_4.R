#--------------------------------------------------------------------------------#
## Iniciando o Experimento
# Carregando a biblioteca
library(FrF2)
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(xfun)
library(rsm)
theme_set(theme_sjplot())

# Criação do experimento
plan <- expand.grid(A = c(-1, 1),
                    B = c(-1, 1),
                    C = c(-1, 1),
                    D = c(-1, 1))

# Respostas
y <- c(45, 71, 48, 65, 68, 60, 80, 65, 43, 100, 45, 104, 75, 86, 70, 96)

# Adicionando a resposta ao planejamento
plan$y <- y

#--------------------------------------------------------------------------------#
## Calculando os efeitos
X <- model.matrix(~A*B*C*D, data = plan[, -5])

N <- dim(X)[1]

efeitos <- crossprod(X,y)/(N/2)

# Outra maneira de calcular os efeitos
library(unrepx)
efeitos2 <- yates(y)

# Half normal plot
hnplot(efeitos2, half = T, method = "Lenth", ID = ME(efeitos2))

# Pareto PSE plot
parplot(efeitos2, method = "Lenth")

# Análise de significância dos efeitos
# Teste t via pseudo erro padrão de Lenth
eff.test(efeitos2, method = "Lenth")

#--------------------------------------------------------------------------------#
# Cálculos para fatorial não replicado via método de Lenth
s0 <- 1.5*median(abs(efeitos[-1]))

# Efeitos desprezíveis
efeitos_2.5 <- abs(efeitos) < 2.5*s0
efeitos_2.5 <- efeitos[efeitos_2.5 == TRUE]

# Pseudo erro padrão de Lenth
PSE <- 1.5*median(efeitos_2.5)

# t calculado
t0 <- efeitos[-1]/PSE

# Erro marginal
m <- N-1
ME <- qt(0.025, df = m/3, lower.tail = F)*PSE

# Erro marginal simultâneo
g <- (1+.95^(1/m))/2
SME <- qt(g, df = m/2)*PSE

# Data Frame efeitos
efeitos_pad <- data.frame(names(efeitos2), abs(efeitos2))
colnames(efeitos_pad) <- c("Fonte", "Efeito")  

# Gráfico de efeitos padronizados
library(ggpubr)
ggbarplot(data = efeitos_pad,
          y = "Efeito",
          x = "Fonte",
          col = "grey50",
          fill = "lightgreen",
          rotate = T,
          sort.val = "asc") +
  theme_bw() +
  geom_hline(yintercept = c(ME, SME), col = "red", show.legend = T)
  
  
#--------------------------------------------------------------------------------#
## Fazendo os modelos  
#Modelo Linear Completo

lm.completo <- lm(y ~ A*B*C*D, data = plan)
summary(lm.completo)
teste <- summary(lm.completo)

# Modelo de terceira ordem
lm3 <- lm(y ~.^3, data = plan)
summary(lm3)

# Reduzindo modelo -> Maneira mais adequada e modelo mais preciso
lm3_red <- step(lm3, direction = "backward", trace = FALSE)
summary(lm3_red)

# Modelo de segunda ordem
lm2 <- lm(y ~.^2, data = plan)
summary(lm2)
  
# Anova
anova <- aov(lm3_red)
summary(anova)

# Criando uma variável
modelo <- lm.completo

# Gráfico de Interação
plot_model(modelo, type = "pred", terms = c("A", "B", "C", "D"))

# Superfície de Resposta e Gráfico de Controle
par(mfrow = c(4, 3))
contour(modelo, ~A + B, image = TRUE)
contour(modelo, ~A + C, image = TRUE)
contour(modelo, ~A + D, image = TRUE)
contour(modelo, ~B + C, image = TRUE)
contour(modelo, ~B + D, image = TRUE)
contour(modelo, ~C + D, image = TRUE)


persp(modelo, ~A + B, zlab = "Ra [um]", col = rainbow(50), contours = ("colors"))
persp(modelo, ~A + C, zlab = "Ra [um]", col = rainbow(50), contours = ("colors"))
persp(modelo, ~A + D, zlab = "Ra [um]", col = rainbow(50), contours = ("colors"))
persp(modelo, ~B + C, zlab = "Ra [um]", col = rainbow(50), contours = ("colors"))
persp(modelo, ~B + D, zlab = "Ra [um]", col = rainbow(50), contours = ("colors"))
persp(modelo, ~C + D, zlab = "Ra [um]", col = rainbow(50), contours = ("colors"))



# Importação das Bibliotecas
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(xfun)
theme_set(theme_sjplot())

# Criação do experimento
plan <- expand.grid(C = c(-1, 1),
                    P = c(-1, 1),
                    V = c(-1, 1))

plan <- rbind(plan, plan)

# Adicionando as respostas
d <- c(-3, 0, -1, 2, -1, 2, 1, 6,
       -1, 1, 0, 3, 0, 1, 1, 5)

plan$y <- d

# Análise

modelo <- lm(y ~ C*P*V, data = plan)
summary(modelo)

anova <- aov(modelo)
summary(anova)

# Gráfico de Interação
plot_model(modelo, type = "pred", terms = c("C", "P", "V"))

# Superfície de Resposta e Gráfico de Controle
par(mfrow = c(2, 3))
contour(modelo, ~C + P, image = TRUE)
contour(modelo, ~C + V, image = TRUE)
contour(modelo, ~P + V, image = TRUE)

persp(modelo, ~C + P, zlab = "Ra [um]", col = rainbow(50), contours = ("colors"))
persp(modelo, ~C + V, zlab = "Ra [um]", col = rainbow(50), contours = ("colors"))
persp(modelo, ~P + V, zlab = "Ra [um]", col = rainbow(50), contours = ("colors"))
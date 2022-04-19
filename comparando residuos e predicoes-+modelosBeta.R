
### a primeira###
#Q foi a apresentada aqui https://sites.google.com/view/gregoriosilva/rio-do-sul-alto-vale/comparando-predi%C3%A7%C3%B5es-e-res%C3%ADduos-ou-o-que-explica-melhor-o-antipetismo18
# baixar os bancos daqui https://github.com/gregorioCPcG/Residuos_EnsinoSuperior_Bolsonarismo_AltoVale/blob/main/Milton27.xlsx

# e daqui 

#https://github.com/gregorioCPcG/residuos_antipetismo2014_explicando_antipetismo2018/blob/main/RESIDUOS%20nEVES%20E%20bOLSONARO%20BANCO%20DE%20DADOS.xlsx

# daí
# primeiro rodar https://github.com/gregorioCPcG/residuos_antipetismo2014_explicando_antipetismo2018/blob/main/analise.R
# de lá manter só b6
rm(b5,base,fit,g,h,h1,superior.bic)
# depois rodar https://github.com/gregorioCPcG/Residuos_EnsinoSuperior_Bolsonarismo_AltoVale/blob/main/codigo.R
# de l´manter b5 só
rm(base,fit,g,Milton27,superior.bic)

# código de duas análises



# a primeira

# foi a apresentada aqui https://sites.google.com/view/gregoriosilva/rio-do-sul-alto-vale/comparando-predi%C3%A7%C3%B5es-e-res%C3%ADduos-ou-o-que-explica-melhor-o-antipetismo18

cor(b6$Bolsonaro2018, b5$Bolsonaro) #  verificar se tá igual, casoo valor do cor tem q ser 1
Bolsonaro_real <- b5$Bolsonaro
Cidade <- b5$Cidade
Previsto_porNeves14 <- b6$predicted
Residuos_porNeves14 <- b6$residuals
Previsto_porEscolaridade <- b5$predicted
Residuos_porEscolaridade <- b5$residuals
rm(b5,b6)
dados <- data.frame(Bolsonaro_real, Cidade, Previsto_porEscolaridade, Previsto_porNeves14,
                    Residuos_porEscolaridade, Residuos_porNeves14)
rm(Bolsonaro_real, Cidade, Previsto_porEscolaridade, Previsto_porNeves14,
   Residuos_porEscolaridade, Residuos_porNeves14)
num <-subset(dados,select=c(Bolsonaro_real, Previsto_porEscolaridade, Previsto_porNeves14))

# o que explica melhor por cidade (quanto menor o resíduo melhor)

library(tidyverse)
library(knitr)
library(kableExtra)

b5 <- dados %>% 
  dplyr::select(Cidade, Residuos_porNeves14, Residuos_porEscolaridade) %>% 
  arrange(Cidade)
b5 %>%
  kbl(caption = "Resíduos por Cidade") %>%
  kable_classic(full_width = F, html_font = "Garamond")


dados$`NevesExplicaMelhor?` <- c("sim","não","sim","não","sim","sim","sim", "não", "não","não", "sim",
                                 "sim","sim","sim","não","sim","sim", "não","não","sim","sim","não",
                                 "sim","sim","sim","sim","não")# continuar daqui 

table(dados$`NevesExplicaMelhor?`)

b5 <- dados %>% 
  dplyr::select(Cidade, `NevesExplicaMelhor?`, Residuos_porNeves14, Residuos_porEscolaridade,) %>% 
  arrange(desc(`NevesExplicaMelhor?`))
b5 %>%
  kbl(caption = "Resíduos por Cidade") %>%
  kable_classic(full_width = F, html_font = "Garamond")

# ggpairs e correlograma buni - previstos x real
library(GGally)
ggpairs(num, title="Comparando Previsões") 


# 
#A segunda  #################
# é a que consta nesse:
#https://sites.google.com/view/gregoriosilva/rio-do-sul-alto-vale/modelos-beta-ou-o-que-explica-melhor-o-antipetismo-de-2018
library(huxtable)
library(lm.beta)
library(readxl)
base4 <- read_excel("D:/ATUALIZA_PASTA_d/A Nova pasta/residuos_4textos/base4.xlsx")
#comparador de modelos
base4$Bolsonaro2018 <- 100*base4$Bolsonaro2018
base4$Neves14 <-100*base4$Neves14
fit1 <- lm(Bolsonaro2018 ~ Neves14,data=base4)
summary(fit1)
fit2 <- lm(Bolsonaro2018 ~superior_comp,data=base4)
summary(fit2)
fit3 <- lm(Bolsonaro2018 ~ Neves14+superior_comp,data=base4)
summary(fit3)
library(sjPlot)
tab_model(fit1, fit2, fit3, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")
library(coefplot)
coefplot(fit3, interactive=TRUE, intercept=FALSE)
coefplot(fit3, interactive=FALSE, intercept=FALSE)
summary(base4)
modelox <- lm.beta(fit3)
tab_model(modelox, show.ci = F, auto.label = F, show.se = F,
          collapse.se = F, wrap.labels = 100, p.style = "stars")

modelox$standardized.coefficients # é esse - só para corroborar




# rio do sul em tudo 


base5 <- read_excel("D:/ATUALIZA_PASTA_d/A Nova pasta/residuos_4textos/base5.xlsx")
base5$Neves14 <- 100*base5$Neves14
base5$Bolsonaro2018 <- 100*base5$Bolsonaro2018
fit1 <- lm(Bolsonaro2018 ~ Neves14,data=base5)
summary(fit1)
fit2 <- lm(Bolsonaro2018 ~superior_comp,data=base5)
summary(fit2)
fit3 <- lm(Bolsonaro2018 ~ Neves14+superior_comp,data=base5)
summary(fit3)
tab_model(fit1, fit2, fit3, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")
coefplot(fit3, interactive=TRUE, intercept=FALSE)
coefplot(fit3, interactive=FALSE, intercept=FALSE)
summary(base4)
modelox <- lm.beta(fit3)
tab_model(modelox, show.ci = F, auto.label = F, show.se = F,
          collapse.se = F, wrap.labels = 100, p.style = "stars")


# predicted x residuals em rio do sul
base <- base5

base$predicted <- predict(fit1)   # Save the predicted values
base$residuals <- residuals(fit1) # Save the residual values

h <- ggplot(base, aes(x = Neves14, y =Bolsonaro2018)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") + geom_text(label=base$Cidade)+       
  geom_segment(aes(xend = Neves14, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()


h


base$predicted2 <- predict(fit2)   # Save the predicted values
base$residuals2 <- residuals(fit2) # Save the residual values

g <- ggplot(base, aes(x = superior_comp, y =Bolsonaro2018)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") + geom_text(label=base$Cidade)+       
  geom_segment(aes(xend = superior_comp, yend = predicted2), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals2), size = abs(residuals2))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted2), shape = 1) +
  theme_bw()


g

base$previsto_porNeves14 <- base$predicted
base$previsto_porEscolaridade <- base$predicted2

b55 <- base  %>% 
  dplyr::select(Cidade, Bolsonaro2018, previsto_porNeves14, previsto_porEscolaridade) %>% 
  arrange(Cidade)
b55 %>%
  kbl(caption = "Resíduos por Cidade") %>%
  kable_classic(full_width = F, html_font = "Garamond")


# no caso de rio do sul a escolaridade preve melhor (já era bem antipetista em 2014)


### UMA ÚLTIMOA
# tudo desenhado tendo em base o modelo final


plot(fit3$fitted.values, base$Bolsonaro2018)
cor(fit1$fitted.values, base$Bolsonaro2018)#s[o neves
cor(fit2$fitted.values, base$Bolsonaro2018)#só escolaridade
cor(fit3$fitted.values, base$Bolsonaro2018)#modelo com as duas

modelo_completo <- fit3$fitted.values
numx <- data.frame(modelo_completo)
numx$Bolsonaro_real <- base$Bolsonaro2018
numx$modelo_Neves14 <- fit1$fitted.values
numx$modelo_escolaridade <- fit2$fitted.values

library(GGally)
ggpairs(numx, title="Comparando Previsões") 



# modelo com rural


base6 <- read_excel("D:/ATUALIZA_PASTA_d/A Nova pasta/residuos_4textos/base6.xlsx")
base6$Bolsonaro2018 <- 100*base6$Bolsonaro2018
base6$Neves14 <- 100*base6$Neves14
fit4 <- lm(Bolsonaro2018 ~ Neves14 + rural_2010 + superior_comp, data=base6)
summary(fit4)

modelos <- list(fit3, fit4)
huxreg(modelos)
coefplot(fit4, interactive=TRUE, intercept=FALSE)

# comparando modelos com e sem rural
Bolsonaro = base6$Bolsonaro2018
numt <- data.frame(Bolsonaro)
numt$Modelo1 <- fit3$fitted.values
numt$Modelo2 <- fit4$fitted.values
ggpairs(numt, title="Comparando Previsões") 


fit_final <- lm(Bolsonaro2018 ~ Neves14 + rural_2010 + superior_comp + log_pop_18+idh_mun+idosos, data=base6)
summary(fit_final)
modelos <- list(fit4, fit_final)
huxreg(modelos)


coefplot(fit_final, interactive=TRUE, intercept=FALSE)

# comparando modelos com e sem rural
Bolsonaro = base6$Bolsonaro2018
numy <- data.frame(Bolsonaro)
numy$Modelo2 <- fit4$fitted.values
numy$Completo <- fit_final$fitted.values
ggpairs(numy, title="Comparando Previsões") 

base <- base6
base$predicted <- predict(fit_final)   # Save the predicted values
base$residuals <- residuals(fit_final) # Save the residual values
base$fitted <- fit_final$fitted.values


base$tercil_Neves14 <- ntile(base$Neves14, 3)
base$tercil_Neves14 <- as.factor(base$tercil_Neves14)
levels(base$tercil_Neves14)


h <- ggplot(base, aes(x = fitted, y =Bolsonaro2018)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") + 
  geom_point(aes(color = Neves14, size = Neves14))


h

cor(base$Neves14, base$Bolsonaro2018)
cor(base$Neves14, fit_final$fitted.values)
cor(fit_final$fitted.values, base$Bolsonaro2018)
plot(base$Neves14, fit_final$fitted.values)
plot(base$Neves14, base$Bolsonaro2018)
plot(fit_final$fitted.values, base$Bolsonaro2018)

# rank

library('BurStMisc')
base$rank_Bolsonaro <- BurStMisc::ntile(base$Bolsonaro2018, result="factor", 28, reverse=TRUE)
base$rank_fitted <- BurStMisc::ntile(fit_final$fitted.values, result="factor", 28, reverse=TRUE)
base$rank_Neves14 <- BurStMisc::ntile(base$Neves14, result="factor", 28, reverse=TRUE)



b56 <- base %>% 
  dplyr::select(rank_Bolsonaro, Cidade, rank_fitted, rank_Neves14) %>% 
  arrange(desc(rank_Bolsonaro))
b56 %>%
  kbl(caption = "Comparando posição ordinal") %>%
  kable_classic(full_width = F, html_font = "Garamond")

numyx <-subset(base, select=c(rank_Bolsonaro, rank_Neves14, rank_fitted))
numyx$rank_Bolsonaro <- as.numeric(numyx$rank_Bolsonaro)
numyx$rank_Neves14 <- as.numeric(numyx$rank_Neves14)
numyx$rank_fitted <- as.numeric(numyx$rank_fitted)
ggpairs(numyx, title="Comparando Ordem")


# regressão ordinal
library(ordinal)
o.model <- clm(rank_Bolsonaro ~ Neves14+superior_comp, data = base)
summary (o.model)
huxreg(o.model)
tab_model(o.model)

coefplot(o.model, interactive = TRUE)
nominal_test(o.model)
scale_test(o.model)

slice.fm1 <- slice(o.model, lambda = 5)
par(mfrow = c(2, 3))
plot(slice.fm1)

base$rank <- as.numeric(base$rank_Bolsonaro)
model <- lm(rank ~ Neves14, data=base)
summary(model)
model2 <- lm(rank ~Neves14+Lula_2006+rural_2010+log_pop_18+idosos+superior_comp+idh_mun, data=base)
summary(model2)
coefplot(model2, interactive = TRUE)


model <- lm(Bolsonaro2018 ~  rural_2010, data=base)
summary(model)
coefplot(model, interactive =FALSE, intercept=FALSE)
plot(base$rural_2010, base$Bolsonaro2018)
cor.test(base$Bolsonaro2018, base$rural_2010)
cor.test(base$Neves14, base$rural_2010)
cor.test(base$Neves14, base$superior_comp)
cor.test(base$Neves14, base$log_pop_18)



model22 <- lm(Bolsonaro2018 ~ Neves14 + superior_comp +
              Neves14:superior_comp, data=base)
summary(model22)
coefplot(model22, interactive=TRUE, intercept=FALSE)
# usar termo regressão 
#https://acervolima.com/como-incluir-a-interacao-na-regressao-usando-a-programacao-r/
 #para model 22 

base$prop_formados <- ntile(base$superior_comp, 2)
base$prop_formados <- as.factor(base$prop_formados)
levels(base$prop_formados) <- c('Menor', 'Maior')

model22 <- lm(Bolsonaro2018 ~ Neves14 + prop_formados+ Neves14:prop_formados, data=base)
summary(model22)
coefplot(model22, interactive=TRUE, intercept=FALSE)
summary(base$superior_comp)
coef(model22)



plot(base$Neves14[base$superior_comp<8.015], 
     base$Bolsonaro2018[base$superior_comp<8.01], 
     col = "blue", 
     ylim = c(60, 90), xlim = c(40, 75), 
     xlab = "Neves14", ylab = "Bolsonaro2018", 
     main = "AltoVale: Bolsonaro vs. Neves14,prop. de formados")
points(base$Neves14[base$superior_comp >8.01], 
       base$Bolsonaro2018[base$superior_comp > 8.01],
       col = "red", pch = 16)
legend(60, 70, 
       legend = c("Menor prop. de formados", "Maior prop. de formados"), 
       col = c("blue", "red"), 
       pch = c(60, 70), bty = "n")

# prop menor 1 -> a= intercp , b = neves14
#propp maior a=interco+9.43, b = neves-interatcie)
abline(a = 45.300, b = 0.528,
       col = "blue", lwd = 1.7)
abline(a = 45.828, b = 0.618,
       col = "red", lwd = 1.7)

library(sjPlot)
tab_model(model22, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")

summary(base$log_pop_18)
summary(base$superior_comp)
sd(base$idh_mun)
sd(base$superior_comp)
cor(base$log_pop_18, base$rural_2010)
populacao <- base$log_pop_18 
prop.rural <- base$rural_2010

fit <- lm(populacao~prop.rural)
library(ggplot2)


base11 <- data.frame(populacao,prop.rural)
summary(fit)
plot(populacao,prop.rural)
base11$predicted <- predict(fit)   # Save the predicted values
base11$residuals <- residuals(fit) # Save the residual values

y1 <- ggplot(base11, aes(x = prop.rural, y =populacao)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_segment(aes(xend = prop.rural, yend = predicted), alpha = .5) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "black", high = "blue") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) + xlab("") +ylab("")+
  theme_classic()
y1


library(tidyverse)
library(knitr)
library(kableExtra)

b577 <- base %>% 
  dplyr::select(Cidade, prop_formados) %>% 
  arrange(prop_formados)
b577 %>%
  kbl(caption = "Prop formados por Cidade") %>%
  kable_classic(full_width = F, html_font = "Garamond")


MENOR <- subset(base, prop_formados=="Menor")
summary(MENOR)
modeloMENOR <- lm(Bolsonaro2018 ~ Neves14, data=MENOR)
MAIOR <- subset(base, prop_formados=="Maior")
summary(MAIOR)
modeloMAIOR <- lm(Bolsonaro2018 ~ Neves14, data=MAIOR)
tab_model(modeloMENOR, modeloMAIOR, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")


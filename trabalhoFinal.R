#Limpa memória
rm(list=ls())

########## IMPORTANTE ##########
# Instala os pacotes
install.packages('GGally')
install.packages('agricolae')
install.packages('corrgram')
install.packages('corrplot')
install.packages('gvlma')

#verificar se precisa
install.packages('ggcorr')

#Roda as bibliotecas
library(car)
library(tidyverse)
library(corrplot)
library(GGally)
library(agricolae)
library(corrgram)
library(scales)
library(gvlma)
library(ggplot2)

########## ----------- ##########
#Database
mtcars
attach(mtcars)

#Verifica se é dataframe
is.data.frame(mtcars)

#Exibe as variáveis/classes contida no conjunto de dados
names(mtcars)

#Exibe o conteúdo das variáveis
mtcars$mpg
mtcars$cyl
mtcars$disp
mtcars$hp
mtcars$drat
mtcars$wt
mtcars$qsec
mtcars$vs
mtcars$am
mtcars$gear
mtcars$carb

#Verifica se a variável é fator
is.factor(mtcars$mpg)
is.factor(mtcars$cyl)
is.factor(mtcars$disp)
is.factor(mtcars$hp)
is.factor(mtcars$drat)
is.factor(mtcars$wt)
is.factor(mtcars$qsec)
is.factor(mtcars$vs)
is.factor(mtcars$am)
is.factor(mtcars$gear)
is.factor(mtcars$carb)

#Verifica se a variável é numérica
is.numeric(mtcars$mpg)
is.numeric(mtcars$cyl)
is.numeric(mtcars$disp)
is.numeric(mtcars$hp)
is.numeric(mtcars$drat)
is.numeric(mtcars$wt)
is.numeric(mtcars$qsec)
is.numeric(mtcars$vs)
is.numeric(mtcars$am)
is.numeric(mtcars$gear)
is.numeric(mtcars$carb)

#Análise descritiva dos dados
summary(mtcars$mpg)
summary(mtcars$cyl)
summary(mtcars$disp)
summary(mtcars$hp)
summary(mtcars$drat)
summary(mtcars$wt)
summary(mtcars$qsec)
summary(mtcars$vs)
summary(mtcars$am)
summary(mtcars$gear)
summary(mtcars$carb)
mean(mtcars$mpg)
mean(mtcars$cyl)
mean(mtcars$disp)
mean(mtcars$hp)
mean(mtcars$drat)
mean(mtcars$wt)
mean(mtcars$qsec)
mean(mtcars$vs)
mean(mtcars$am)
mean(mtcars$gear)
mean(mtcars$carb)
sd(mtcars$mpg)
sd(mtcars$cyl)
sd(mtcars$disp)
sd(mtcars$hp)
sd(mtcars$drat)
sd(mtcars$wt)
sd(mtcars$qsec)
sd(mtcars$vs)
sd(mtcars$am)
sd(mtcars$gear)
sd(mtcars$carb)
var(mtcars$mpg)
var(mtcars$cyl)
var(mtcars$disp)
var(mtcars$hp)
var(mtcars$drat)
var(mtcars$wt)
var(mtcars$qsec)
var(mtcars$vs)
var(mtcars$am)
var(mtcars$gear)
var(mtcars$carb)

#Correlação
#Matriz de correlação
matriz_correlacao <- cor(mtcars) ; matriz_correlacao
dados_correlacao <- mtcars[ ,c('mpg','cyl','disp','hp', 'drat', 'wt', 'qsec', 'vs', 'am', 'gear',
                               'carb')]
#Matriz P-valor
dados <- correlation(mtcars[,1:11],method="pearson") ; dados

#Guarda só a tabela do p-valor
dadospvalor <- dados$pvalue ; dadospvalor

#Transforma Matriz P-valor em porcentagem
porcentagem <- percent (dadospvalor, accuracy = 0.000000000001, scale = 100) ;
porcentagem

#Regressão
#Regressão de todas as variáveis que possuem maior correlação em relação ao p-valor
(menor porcentagem na matriz P-valor)
mpg_wt<-lm(sqrt(mpg) ~ wt, data = mtcars); mpg_wt
gvlma(mpg_wt)
anova(mpg_wt)
summary(mpg_wt)

cyl_vs<-lm(sqrt(cyl) ~ vs, data = mtcars); cyl_vs
gvlma(cyl_vs)
anova(cyl_vs)
summary(cyl_vs)

disp_wt<-lm(sqrt(disp) ~ wt, data = mtcars); disp_wt
gvlma(disp_wt)
anova(disp_wt)
summary(disp_wt)

hp_carb<-lm(sqrt(hp) ~ carb, data = mtcars); hp_carb
gvlma(hp_carb)
anova(hp_carb)
summary(hp_carb)

drat_am<-lm(sqrt(drat) ~ am, data = mtcars); drat_am
gvlma(drat_am)
anova(drat_am)
summary(drat_am)

wt_am<-lm(sqrt(wt) ~ am, data = mtcars); wt_am
gvlma(wt_am)
anova(wt_am)
summary(wt_am)

qsec_carb<-lm(sqrt(qsec) ~ carb, data = mtcars); qsec_carb
gvlma(qsec_carb)
anova(qsec_carb)
summary(qsec_carb)

vs_carb<-lm(sqrt(vs) ~ carb, data = mtcars); vs_carb
gvlma(vs_carb)
anova(vs_carb)
summary(vs_carb)

am_gear<-lm(sqrt(am) ~ gear, data = mtcars); am_gear
gvlma(am_gear)
anova(am_gear)
summary(am_gear)

gear_carb<-lm(sqrt(gear) ~ carb, data = mtcars); gear_carb
gvlma(gear_carb)
anova(gear_carb)
summary(gear_carb)

#Gráficos
#Correlação
#Exibe o gráfico em cores (bolinhas) da matriz de correlação
corrplot(matriz_correlacao)

#Exibe o gráfico em pontos, elipses, sombreamento da matriz de correlação
corrgram(matriz_correlacao, lower.panel = panel.pts, upper.panel= panel.conf,
         diag.panel = panel.density)
corrgram(dados_correlacao, lower.panel = panel.pts, upper.panel= panel.conf,
         diag.panel = panel.density)

#Exibe o gráfico em cores com os valores da matriz de correlação
ggcorr(mtcars, label=T, method = c("pairwise", "pearson"), label_round = 3)

#Regressão
#Plot mpg_wt
plot(sqrt(mpg)~wt, pch=20, col="red",
     xlab="Peso em libras (wt)",
     ylab="Milhas por galão (mpg)",
     data=mtcars)

lines(mtcars$wt,predict(mpg_wt), type="l",
      col="blue", lwd=2)
text(4,5.7, expression(hat(y)==6.3553-0.5969*wt))
text(4,5.55, expression(R^2==0.7732))

#Plot cyl_vs
plot(sqrt(cyl)~vs, pch=20, col="red",
     xlab="Tipo de motor (Tipo V = 0, reto = 1) (vs)",
     ylab="Número de cilindros (cyl)",
     data=mtcars)

lines(mtcars$vs,predict(cyl_vs), type="l",
      col="blue", lwd=2, pch=18)
text(0.6,2.8, expression(hat(y)==2.7192-0.5908*vs))
text(0.6,2.74, expression(R^2==0.6384))

#Plot disp_wt
plot(sqrt(disp)~wt, pch=20, col="red",
     xlab="Peso em libras (wt)",
     ylab="Quantidade total de potência do motor (disp)",
     data=mtcars)

lines(mtcars$wt,predict(disp_wt), type="l",
      col="blue", lwd = 2, pch = 22)
text(3,21.5, expression(hat(y)== 2.507+3.769*wt))
text(3,20.55, expression(R^2== 0.7849))

#Plot hp_carb
plot(sqrt(hp)~carb, pch=20, col="red",
     xlab="Número de carburadores (carb)",
     ylab="Cavalos de potência (hp)",
     data=mtcars)

lines(mtcars$carb,predict(hp_carb), type="l",
      col="blue", lwd=2, pch=18)
text(2,17.5, expression(hat(y)==1.254+8.273*carb))
text(2,16.7, expression(R^2== 0.5161))

#Plot drat_am
plot(sqrt(drat)~am, pch=20, col="red",
     xlab="Tipo de transmissão (1 = automática, 0 = manual) (am)",
     ylab="Razão do eixo traseiro (drat)",
     data=mtcars)

lines(mtcars$am,predict(drat_am), type="l",
      col="blue", lwd=2, pch=18)
text(0.6,2.15, expression(hat(y)==1.8098+0.2008*am))
text(0.6,2.11, expression(R^2== 0.4923))

#Plot wt_am
plot(sqrt(wt)~am, pch=20, col="red",
     xlab="Tipo de transmissão (1 = automática, 0 = manual) (am)",
     ylab="Peso em libras (wt)",
     data=mtcars)

lines(mtcars$am,predict(wt_am), type="l",
      col="blue", lwd=2, pch=18)
text(0.6,2.23, expression(hat(y)==1.9323-0.3914*am))
text(0.6,2.15, expression(R^2== 0.4911))

#Plot qsec_carb
plot(sqrt(qsec)~carb, pch=20, col="red",
     xlab="Número de carburadores (carb)",
     ylab="Tempo para percorrer ¼ de milha, em segundos (qsec)",
     data=mtcars)

lines(mtcars$carb,predict(qsec_carb), type="l",
      col="blue", lwd=2, pch=22)
text(6,4.7, expression(hat(y)== 4.46415-0.08692*carb))
text(6,4.62, expression(R^2== 0.4264))

#Plot vs_carb
plot(sqrt(vs)~carb, pch=20, col="red",
     xlab="Número de carburadores (carb)",
     ylab="Tipo de motor (Tipo V = 0, reto = 1) (vs)",
     data=mtcars)

lines(mtcars$carb,predict (vs_carb), type="l",
      col="blue", lwd = 2, pch = 22)
text(5,0.9, expression(hat(y)== 0.9374-0.1777*carb))
text(5,0.83, expression(R^2== 0.3019))

#Plot am_gear
plot(sqrt(am)~gear, pch=20, col="red",
     xlab="Número de marchas (não incluída a marcha ré) (gear)",
     ylab="Tipo de transmissão (1 = automática, 0 = manual) (am)",
     data=mtcars)

lines(mtcars$gear,predict (am_gear), type="l",
      col="blue", lwd = 2, pch = 22)
text(3.5,0.8, expression(hat(y)== -1.574+0.537*gear))
text(3.5,0.73, expression(R^2== 0.6182))

#Plot gear_carb
plot(sqrt(gear)~carb, pch=20, col="red",
     xlab="Número de carburadores (carb)",
     ylab="Número de marchas (não incluída a marcha ré) (gear)",
     data=mtcars)

lines(mtcars$carb,predict (gear_carb), type="l",
      col="blue", lwd = 2, pch = 22)
text(5,2.2, expression(hat(y)== 1.82574+0.03042*carb))
text(5,2.16, expression(R^2== 0.03672))
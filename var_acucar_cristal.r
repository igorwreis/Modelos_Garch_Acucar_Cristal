#install.packages("gridExtra")

library(formattable)
library(rugarch)
library(ggplot2)
library(dplyr)
library(gghighlight)
library(ggpubr)
library(pastecs)
library(gridExtra)

options(vsc.dev.args = list(width = 800, height = 400))

diretorio = "C://Users//igorw//OneDrive//Documentos//Pos - FIA//_trabalho_final//Dados//codigos//"

precos = read.csv('C:/Users/igorw/OneDrive/Documentos/Pos - FIA/_trabalho_final/Dados/acucar_esalq_sp.csv', sep=";")
head(precos, 5)

precos$Data <- as.Date(precos$Data, format = "%d/%m/%Y")

precos$BRL <- sub(",",".",precos$BRL)
precos$BRL <- as.numeric(precos$BRL)

precos$USD <- sub(",",".",precos$USD)
precos$USD <- as.numeric(precos$USD)

retornos <- diff(log(precos$BRL))*100

par(new=TRUE)
par(mfrow = c(1, 2))
par(mar = c(5, 5, 4, 2))
acf(retornos, main = "FAC", cex.lab=1.5, cex.axis=1.5)
par(mar = c(5, 5, 4, 2))
pacf(retornos, main = "FACP", cex.lab=1.5, cex.axis=1.5)

precos$retornos <- c(NA, retornos)
head(precos)

precos_ = subset(precos, select=c("Data", "retornos"))
precos_ = subset(precos_, retornos!="NA")

precos_final = precos_ %>% tibble::rownames_to_column(var = "Index") %>% select(-Index)
head(precos_final)


#par(new=TRUE)
#par(mfrow = c(2, 2))
#par(mfrow = c(2, 2, 2, 2))

# plot(x=precos$Data, y=precos$BRL, pch=1, type="l", col="blue", bty="n",
#  main="Preço Açúcar Cristal (Saca 50kg)\n\n",
#  xlab="Tempo", ylab="Preço R$", cex.main=4, cex.lab=2, cex.axis=2, )

#plot(precos_final, pch=1, type="l", col="blue", bty="n",
#  main="Retornos Açúcar Cristal (Saca 50kg)",
#  xlab="Tempo", ylab="Variação %", cex.main=1.5)

options(vsc.dev.args = list(width = 1600, height = 1200))

#mtext("Small Margins", side = 3, line = 1, cex = 1.2)
#mtext("par(mar = c(2, 2, 2, 2))", side = 3)

# PREÇOS
ggplot(data = precos, aes(x=Data, y=BRL)) +
  geom_line(color = "blue", size=2) +
  labs(
    x="\nTempo", y="Preço R$\n"
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(2, 2, 2, 2, "cm"),
    plot.title = element_text(hjust = 0.5, size = 60),
    axis.text = element_text(size = 30, color = "black"),
    axis.title = element_text(size = 25, color = "black")
  ) + 
  ylim(40, 160) +
  ggtitle("Preço Açúcar Cristal (Saca 50kg)\n")

# RETORNOS
ggplot(data = precos_final, aes(x=Data, y=retornos)) +
  geom_line(color = "blue", size=1) +
  labs(
    x="\nTempo", y="Variação %\n"
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(2, 2, 2, 2, "cm"),
    plot.title = element_text(hjust = 0.5, size = 60),
    axis.text = element_text(size = 30, color = "black"),
    axis.title = element_text(size = 25, color = "black")
  ) + 
  ylim(-6, 4) +
  ggtitle("Retornos Açúcar Cristal\n")

#ggarrange(plot_precos, plot_retornos, 
#          labels = c("A", "B"),
#          ncol = 2, nrow = 1)

count(precos)
sd_ = sd(retornos)
sd_

head(precos_final)
head(retornos)
head(precos)

summary(retornos)

df_violacoes <- data.frame(
  Modelo = character(0), 
  Qt_Violacoes_95 = numeric(0), 
  Perc_Violacoes_95 = numeric(0), 
  Qt_Violacoes_99 = numeric(0), 
  Perc_Violacoes_99 = numeric(0)
)

#df_violacoes

#####################
####### GARCH ####### 
#####################
modelo_garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                                        mean.model=list(armaOrder=c(2,1)),
                                        distribution.model = "std")

ajustado <- ugarchfit(data = precos_$retornos, spec = modelo_garch)
ajustado

summary(ajustado) # ESTÁ CORRETO??? 
plot(ajustado, which="all") # gostei

sigma(ajustado)

estimado <- ajustado@fit$sigma
head(estimado)

sigma_ = data.frame(sigma = estimado)
head(sigma_)

df_completo = cbind(precos_final, sigma_)
head(df_completo)

# apenas para verificar nivel vol anualizada
plot(x=estimado * sqrt(252),xlab="",ylab="",main="Açúcar Cristal Annual Vol (GARCH[1,1])",
pch=1, type="l", col="blue", bty="n")

# sugestao Leo - verificar quadrado residuos vs variancia estimada
estimado_var <- ajustado@fit$var
residuos <- (ajustado@fit$residuals)^2
summary(estimado_var)
plot(residuos)
lines(residuos, col="red")
lines(estimado_var, col="green")

df_completo$var95 = qnorm(0.05) * df_completo$sigma
df_completo$var99 = qnorm(0.01) * df_completo$sigma
head(df_completo)


modelo_utilizado = "Arma(2,1)-Garch(1,1)"

#par(new=TRUE)
#par(mfrow = c(2, 2))

options(vsc.dev.args = list(width = 3200, height = 1200))

# VaR 95%
g1 = ggplot(data = df_completo, aes(x = Data)) +
  geom_line(aes(y = retornos, color = "retornos"), size = 0.5) +
  geom_line(aes(y = var95, color = "var95"), size = 0.8) +
  geom_point(data = subset(df_completo, retornos < var95),
            aes(y = retornos, color="retornos < var95"), size = 6) +
  labs(
    x = "\nTempo", y = "Variação%", colour=NULL, 
  title = paste("Realizado vs VaR 95% - ", modelo_utilizado, sep="")) +
  scale_color_manual(values=c(retornos = "blue",
    "retornos < var95" = "black", var95 = "red"),
    labels = c("Variacao%", "Violacoes", "VaR param")) +
  theme(
    plot.title = element_text(hjust = 0.5, size=65, , face="bold"),
  axis.title.x = element_text(size = 50), axis.title.y = element_text(size = 50),
  axis.text = element_text(size = 40), plot.margin = unit(c(1, 1, 1, 1), "cm"),
  axis.ticks = element_line(size = 2, color="black"),
  axis.ticks.length = unit(.5, "cm"),
  legend.text = element_text(size = 45), legend.title = element_text(size = 45),
  legend.position = c(0.9, 0.1))


###########
# VaR 99%
###########
g2 = ggplot(data = df_completo, aes(x = Data)) +
  geom_line(aes(y = retornos, color = "retornos"), size = 0.5) +
  geom_line(aes(y = var99, color = "var99"), size = 0.8) +
  geom_point(data = subset(df_completo, retornos < var99),
            aes(y = retornos, color="retornos < var99"), size = 6) +
  labs(
    x = "\nTempo", y = "Variação%", colour=NULL, 
  title = paste("Realizado vs VaR 99% - ", modelo_utilizado, sep="")) +
  scale_color_manual(values=c(retornos = "blue",
    "retornos < var99" = "black", var99 = "red"),
    labels = c("Variacao%", "Violacoes", "VaR param")) +
  theme(
    plot.title = element_text(hjust = 0.5, size=65, , face="bold"),
  axis.title.x = element_text(size = 50), axis.title.y = element_text(size = 50),
  axis.text = element_text(size = 40), plot.margin = unit(c(1, 1, 1, 1), "cm"),
  axis.ticks = element_line(size = 2, color="black"),
  axis.ticks.length = unit(.5, "cm"),
  legend.text = element_text(size = 45), legend.title = element_text(size = 45),
  legend.position = c(0.9, 0.1))


grid.arrange(g1, g2, ncol = 2, nrow = 1)

# computa total violacoes
violacoes95 = sum(df_completo$var95 > df_completo$retornos, na.rm=TRUE)
violacoes99 = sum(df_completo$var99 > df_completo$retornos, na.rm=TRUE)
totaldias = nrow(df_completo)

violacoes95_pc = violacoes95 / totaldias
violacoes99_pc = violacoes99 / totaldias

#df_violacoes[nrow(df_violacoes) + 1,] <- c(modelo_utilizado, violacoes95,
#    violacoes95_pc, violacoes99, violacoes99_pc)

nova_linha <- data.frame(
  Modelo = modelo_utilizado,
  Qt_Violacoes_95 = violacoes95,
  Perc_Violacoes_95 = violacoes95_pc,
  Qt_Violacoes_99 = violacoes99,
  Perc_Violacoes_99 = violacoes99_pc
)

#nova_linha

df_violacoes <- rbind(df_violacoes, nova_linha)
df_violacoes

#### excluir a linha abaixo para os outros casos
df_agregado = data.frame(df_completo)
################################################

names(df_agregado)[names(df_agregado) == "sigma"] = paste("sigma_",
    modelo_utilizado, sep="")
names(df_agregado)[names(df_agregado) == "var95"] = paste("var95_",
    modelo_utilizado, sep="")
names(df_agregado)[names(df_agregado) == "var99"] = paste("var99_",
    modelo_utilizado, sep="")
head(df_agregado)


####################
####### EWMA ####### 
####################
modelo_garch <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1, 1)),
    mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
    distribution.model = "norm", fixed.pars=list(omega=0, alpha1=(1-0.945)))

ajustado <- ugarchfit(data = precos_$retornos, spec = modelo_garch)
ajustado

summary(ajustado) # ESTÁ CORRETO??? 
plot(ajustado, which="all") # gostei

sigma(ajustado)

estimado <- ajustado@fit$sigma
head(estimado)

sigma_ = data.frame(sigma = estimado)
head(sigma_)

df_completo = cbind(precos_final, sigma_)
head(df_completo)

# apenas para verificar nivel vol anualizada
plot(x=estimado * sqrt(252),xlab="",ylab="",main="Açúcar Cristal Annual Vol (GARCH[1,1])",
pch=1, type="l", col="blue", bty="n")

# sugestao Leo - verificar quadrado residuos vs variancia estimada
estimado_var <- ajustado@fit$var
residuos <- (ajustado@fit$residuals)^2
summary(estimado_var)
plot(residuos)
lines(residuos, col="red")
lines(estimado_var, col="green")

df_completo$var95 = qnorm(0.05) * df_completo$sigma
df_completo$var99 = qnorm(0.01) * df_completo$sigma
head(df_completo)


modelo_utilizado = "Ewma - λ = 0,945"

# VaR 95%
g3 = ggplot(data = df_completo, aes(x = Data)) +
  geom_line(aes(y = retornos, color = "retornos"), size = 0.5) +
  geom_line(aes(y = var95, color = "var95"), size = 0.8) +
  geom_point(data = subset(df_completo, retornos < var95),
            aes(y = retornos, color="retornos < var95"), size = 6) +
  labs(
    x = "\nTempo", y = "Variação%", colour=NULL, 
  title = paste("Realizado vs VaR 95% - ", modelo_utilizado, sep="")) +
  scale_color_manual(values=c(retornos = "blue",
    "retornos < var95" = "black", var95 = "red"),
    labels = c("Variacao%", "Violacoes", "VaR param")) +
  theme(
    plot.title = element_text(hjust = 0.5, size=65, , face="bold"),
  axis.title.x = element_text(size = 50), axis.title.y = element_text(size = 50),
  axis.text = element_text(size = 40), plot.margin = unit(c(1, 1, 1, 1), "cm"),
  axis.ticks = element_line(size = 2, color="black"),
  axis.ticks.length = unit(.5, "cm"),
  legend.text = element_text(size = 45), legend.title = element_text(size = 45),
  legend.position = c(0.9, 0.1))


###########
# VaR 99%
###########
g4 = ggplot(data = df_completo, aes(x = Data)) +
  geom_line(aes(y = retornos, color = "retornos"), size = 0.5) +
  geom_line(aes(y = var99, color = "var99"), size = 0.8) +
  geom_point(data = subset(df_completo, retornos < var99),
            aes(y = retornos, color="retornos < var99"), size = 6) +
  labs(
    x = "\nTempo", y = "Variação%", colour=NULL, 
  title = paste("Realizado vs VaR 99% - ", modelo_utilizado, sep="")) +
  scale_color_manual(values=c(retornos = "blue",
    "retornos < var99" = "black", var99 = "red"),
    labels = c("Variacao%", "Violacoes", "VaR param")) +
  theme(
    plot.title = element_text(hjust = 0.5, size=65, , face="bold"),
  axis.title.x = element_text(size = 50), axis.title.y = element_text(size = 50),
  axis.text = element_text(size = 40), plot.margin = unit(c(1, 1, 1, 1), "cm"),
  axis.ticks = element_line(size = 2, color="black"),
  axis.ticks.length = unit(.5, "cm"),
  legend.text = element_text(size = 45), legend.title = element_text(size = 45),
  legend.position = c(0.9, 0.1))

grid.arrange(g3, g4, ncol = 2, nrow = 1)

# computa total violacoes
violacoes95 = sum(df_completo$var95 > df_completo$retornos, na.rm=TRUE)
violacoes99 = sum(df_completo$var99 > df_completo$retornos, na.rm=TRUE)
totaldias = nrow(df_completo)

violacoes95_pc = violacoes95 / totaldias
violacoes99_pc = violacoes99 / totaldias

nova_linha <- data.frame(
  Modelo = modelo_utilizado,
  Qt_Violacoes_95 = violacoes95,
  Perc_Violacoes_95 = violacoes95_pc,
  Qt_Violacoes_99 = violacoes99,
  Perc_Violacoes_99 = violacoes99_pc
)

df_violacoes <- rbind(df_violacoes, nova_linha)
df_violacoes

### AGERGA DF COMPLETO AO AGREGADO
### APENAS A PARTIR DO SEGUNDO MODELO!!
df_agregado = merge(x = df_agregado, y = df_completo[ ,
    c("Data", "sigma", "var95", "var99")], by = "Data", all.x=TRUE)

# RENOMEIA COLUNAS INCLUINDO NOME DO MODELO
names(df_agregado)[names(df_agregado) == "sigma"] = paste("sigma_",
    modelo_utilizado, sep="")
names(df_agregado)[names(df_agregado) == "var95"] = paste("var95_",
    modelo_utilizado, sep="")
names(df_agregado)[names(df_agregado) == "var99"] = paste("var99_",
    modelo_utilizado, sep="")
head(df_agregado)


######################
####### eGarch ####### 
######################
modelo_garch <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(2, 1)),
    mean.model=list(armaOrder=c(2,2), include.mean=TRUE),
    distribution.model = "std")

ajustado <- ugarchfit(data = precos_$retornos, spec = modelo_garch)
ajustado

summary(ajustado) # ESTÁ CORRETO??? 
plot(ajustado, which="all") # gostei

sigma(ajustado)

estimado <- ajustado@fit$sigma
head(estimado)

sigma_ = data.frame(sigma = estimado)
head(sigma_)

df_completo = cbind(precos_final, sigma_)
head(df_completo)

# apenas para verificar nivel vol anualizada
plot(x=estimado * sqrt(252),xlab="",ylab="",main="Açúcar Cristal Annual Vol (GARCH[1,1])",
pch=1, type="l", col="blue", bty="n")

# sugestao Leo - verificar quadrado residuos vs variancia estimada
estimado_var <- ajustado@fit$var
residuos <- (ajustado@fit$residuals)^2
summary(estimado_var)
plot(residuos)
lines(residuos, col="red")
lines(estimado_var, col="green")

df_completo$var95 = qnorm(0.05) * df_completo$sigma
df_completo$var99 = qnorm(0.01) * df_completo$sigma
head(df_completo)


modelo_utilizado ="Arma(2,2)-eGarch(2,1)"

# VaR 95%
g5 = ggplot(data = df_completo, aes(x = Data)) +
  geom_line(aes(y = retornos, color = "retornos"), size = 0.5) +
  geom_line(aes(y = var95, color = "var95"), size = 0.8) +
  geom_point(data = subset(df_completo, retornos < var95),
            aes(y = retornos, color="retornos < var95"), size = 6) +
  labs(
    x = "\nTempo", y = "Variação%", colour=NULL, 
  title = paste("Realizado vs VaR 95% - ", modelo_utilizado, sep="")) +
  scale_color_manual(values=c(retornos = "blue",
    "retornos < var95" = "black", var95 = "red"),
    labels = c("Variacao%", "Violacoes", "VaR param")) +
  theme(
    plot.title = element_text(hjust = 0.5, size=65, , face="bold"),
  axis.title.x = element_text(size = 50), axis.title.y = element_text(size = 50),
  axis.text = element_text(size = 40), plot.margin = unit(c(1, 1, 1, 1), "cm"),
  axis.ticks = element_line(size = 2, color="black"),
  axis.ticks.length = unit(.5, "cm"),
  legend.text = element_text(size = 45), legend.title = element_text(size = 45),
  legend.position = c(0.9, 0.1))


###########
# VaR 99%
###########
g6 = ggplot(data = df_completo, aes(x = Data)) +
  geom_line(aes(y = retornos, color = "retornos"), size = 0.5) +
  geom_line(aes(y = var99, color = "var99"), size = 0.8) +
  geom_point(data = subset(df_completo, retornos < var99),
            aes(y = retornos, color="retornos < var99"), size = 6) +
  labs(
    x = "\nTempo", y = "Variação%", colour=NULL, 
  title = paste("Realizado vs VaR 99% - ", modelo_utilizado, sep="")) +
  scale_color_manual(values=c(retornos = "blue",
    "retornos < var99" = "black", var99 = "red"),
    labels = c("Variacao%", "Violacoes", "VaR param")) +
  theme(
    plot.title = element_text(hjust = 0.5, size=65, , face="bold"),
  axis.title.x = element_text(size = 50), axis.title.y = element_text(size = 50),
  axis.text = element_text(size = 40), plot.margin = unit(c(1, 1, 1, 1), "cm"),
  axis.ticks = element_line(size = 2, color="black"),
  axis.ticks.length = unit(.5, "cm"),
  legend.text = element_text(size = 45), legend.title = element_text(size = 45),
  legend.position = c(0.9, 0.1))

grid.arrange(g5, g6, ncol = 2, nrow = 1)

# computa total violacoes
violacoes95 = sum(df_completo$var95 > df_completo$retornos, na.rm=TRUE)
violacoes99 = sum(df_completo$var99 > df_completo$retornos, na.rm=TRUE)
totaldias = nrow(df_completo)

violacoes95_pc = violacoes95 / totaldias
violacoes99_pc = violacoes99 / totaldias

violacoes95

nova_linha <- data.frame(
  Modelo = modelo_utilizado,
  Qt_Violacoes_95 = violacoes95,
  Perc_Violacoes_95 = violacoes95_pc,
  Qt_Violacoes_99 = violacoes99,
  Perc_Violacoes_99 = violacoes99_pc
)

df_violacoes <- rbind(df_violacoes, nova_linha)
df_violacoes

### AGERGA DF COMPLETO AO AGREGADO
### APENAS A PARTIR DO SEGUNDO MODELO!!
df_agregado = merge(x = df_agregado, y = df_completo[ ,
    c("Data", "sigma", "var95", "var99")], by = "Data", all.x=TRUE)

# RENOMEIA COLUNAS INCLUINDO NOME DO MODELO
names(df_agregado)[names(df_agregado) == "sigma"] = paste("sigma_",
    modelo_utilizado, sep="")
names(df_agregado)[names(df_agregado) == "var95"] = paste("var95_",
    modelo_utilizado, sep="")
names(df_agregado)[names(df_agregado) == "var99"] = paste("var99_",
    modelo_utilizado, sep="")
head(df_agregado)


######################
####### tGarch ####### 
######################
modelo_garch <- ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1),
    submodel="TGARCH"),
    mean.model=list(armaOrder=c(2,2), include.mean=TRUE),
    distribution.model = "std")

ajustado <- ugarchfit(data = precos_$retornos, spec = modelo_garch)
ajustado

summary(ajustado) # ESTÁ CORRETO??? 
plot(ajustado, which="all") # gostei

sigma(ajustado)

estimado <- ajustado@fit$sigma
head(estimado)

sigma_ = data.frame(sigma = estimado)
head(sigma_)

df_completo = cbind(precos_final, sigma_)
head(df_completo)

# apenas para verificar nivel vol anualizada
plot(x=estimado * sqrt(252),xlab="",ylab="",main="Açúcar Cristal Annual Vol (GARCH[1,1])",
pch=1, type="l", col="blue", bty="n")

# sugestao Leo - verificar quadrado residuos vs variancia estimada
estimado_var <- ajustado@fit$var
residuos <- (ajustado@fit$residuals)^2
summary(estimado_var)
plot(residuos)
lines(residuos, col="red")
lines(estimado_var, col="green")

df_completo$var95 = qnorm(0.05) * df_completo$sigma
df_completo$var99 = qnorm(0.01) * df_completo$sigma
head(df_completo)


modelo_utilizado ="Arma(2,2)-tGarch(1,1)"

# VaR 95%
g7 = ggplot(data = df_completo, aes(x = Data)) +
  geom_line(aes(y = retornos, color = "retornos"), size = 0.5) +
  geom_line(aes(y = var95, color = "var95"), size = 0.8) +
  geom_point(data = subset(df_completo, retornos < var95),
            aes(y = retornos, color="retornos < var95"), size = 6) +
  labs(
    x = "\nTempo", y = "Variação%", colour=NULL, 
  title = paste("Realizado vs VaR 95% - ", modelo_utilizado, sep="")) +
  scale_color_manual(values=c(retornos = "blue",
    "retornos < var95" = "black", var95 = "red"),
    labels = c("Variacao%", "Violacoes", "VaR param")) +
  theme(
    plot.title = element_text(hjust = 0.5, size=65, , face="bold"),
  axis.title.x = element_text(size = 50), axis.title.y = element_text(size = 50),
  axis.text = element_text(size = 40), plot.margin = unit(c(1, 1, 1, 1), "cm"),
  axis.ticks = element_line(size = 2, color="black"),
  axis.ticks.length = unit(.5, "cm"),
  legend.text = element_text(size = 45), legend.title = element_text(size = 45),
  legend.position = c(0.9, 0.1))


###########
# VaR 99%
###########
g8 = ggplot(data = df_completo, aes(x = Data)) +
  geom_line(aes(y = retornos, color = "retornos"), size = 0.5) +
  geom_line(aes(y = var99, color = "var99"), size = 0.8) +
  geom_point(data = subset(df_completo, retornos < var99),
            aes(y = retornos, color="retornos < var99"), size = 6) +
  labs(
    x = "\nTempo", y = "Variação%", colour=NULL, 
  title = paste("Realizado vs VaR 99% - ", modelo_utilizado, sep="")) +
  scale_color_manual(values=c(retornos = "blue",
    "retornos < var99" = "black", var99 = "red"),
    labels = c("Variacao%", "Violacoes", "VaR param")) +
  theme(
    plot.title = element_text(hjust = 0.5, size=65, , face="bold"),
  axis.title.x = element_text(size = 50), axis.title.y = element_text(size = 50),
  axis.text = element_text(size = 40), plot.margin = unit(c(1, 1, 1, 1), "cm"),
  axis.ticks = element_line(size = 2, color="black"),
  axis.ticks.length = unit(.5, "cm"),
  legend.text = element_text(size = 45), legend.title = element_text(size = 45),
  legend.position = c(0.9, 0.1))

grid.arrange(g7, g8, ncol = 2, nrow = 1)

# computa total violacoes
violacoes95 = sum(df_completo$var95 > df_completo$retornos, na.rm=TRUE)
violacoes99 = sum(df_completo$var99 > df_completo$retornos, na.rm=TRUE)
totaldias = nrow(df_completo)

violacoes95_pc = violacoes95 / totaldias
violacoes99_pc = violacoes99 / totaldias

nova_linha <- data.frame(
  Modelo = modelo_utilizado,
  Qt_Violacoes_95 = violacoes95,
  Perc_Violacoes_95 = violacoes95_pc,
  Qt_Violacoes_99 = violacoes99,
  Perc_Violacoes_99 = violacoes99_pc
)

df_violacoes <- rbind(df_violacoes, nova_linha)
df_violacoes

### AGERGA DF COMPLETO AO AGREGADO
### APENAS A PARTIR DO SEGUNDO MODELO!!
df_agregado = merge(x = df_agregado, y = df_completo[ ,
    c("Data", "sigma", "var95", "var99")], by = "Data", all.x=TRUE)

# RENOMEIA COLUNAS INCLUINDO NOME DO MODELO
names(df_agregado)[names(df_agregado) == "sigma"] = paste("sigma_",
    modelo_utilizado, sep="")
names(df_agregado)[names(df_agregado) == "var95"] = paste("var95_",
    modelo_utilizado, sep="")
names(df_agregado)[names(df_agregado) == "var99"] = paste("var99_",
    modelo_utilizado, sep="")
head(df_agregado)


df_violacoes[["Qt Violacoes 95%"]] <- as.numeric(df_violacoes[["Qt Violacoes 95%"]])
df_violacoes[["% Violacoes 95%"]] <- as.numeric(df_violacoes[["% Violacoes 95%"]])
df_violacoes[["Qt Violacoes 99%"]] <- as.numeric(df_violacoes[["Qt Violacoes 99%"]])
df_violacoes[["% Violacoes 99%"]] <- as.numeric(df_violacoes[["% Violacoes 99%"]])

df_violacoes

# SALVA ARQUIVO AGREGADO
write.csv2(df_agregado,
    paste(diretorio, "var_acucar_cristal.csv", sep=""),
    row.names=TRUE)

# SALVA CONTAGEM VIOLACOES
write.csv2(df_violacoes,
    paste(diretorio, "var_cont_violacoes.csv", sep=""),
    row.names=TRUE)

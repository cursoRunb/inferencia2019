dados <- read.csv("https://raw.githubusercontent.com/gustavopompeu/ENAP/master/Agua_Esgoto.csv")
head(dados)
subgrupo <- dados[dados$Ano.de.Referência==2012,]
popurb2012<- subgrupo$G06A...População.urbana.residente.do.s..município.s..com.abastecimento.de.água
popurb2012 <- popurb2012[!is.na(popurb2012)]
par(mfrow=c(1,2))
hist(popurb2012)
boxplot(popurb2012)
t.test(x = popurb2012,alternative = "less",mu = 122000)


s2 <- var(popurb2012,na.rm = T)
media <- mean(popurb2012)
n <- length(popurb2012)
tobs <- (media-122000)/sqrt(s2/n)
tobs
pt(tobs,(n-1))
xcrit <- qt(.05,(n-1))*sqrt(s2/n)+122000
xcrit

1- pt((xcrit-110000)/sqrt(s2/n),(n-1))

so2016 <- dados[(dados$Ano.de.Referência==2016) & (dados$Abrangência=="Local"),]
teste <- as.logical(so2016$G05B...Quantidade.total.de.municípios.atendidos.com.esgotamento.sanitário)
teste <- teste[!is.na(teste)]
summary(teste)
pichap <- mean(teste)
prop.test(sum(teste,na.rm=T),length(teste),p = .7,alternative = "greater")

t.test(teste,mu = .7,alternative = "greater")
(pichap-.7)/sqrt(pichap*(1-pichap)/1575)


parana <- dados[dados$Estado=="PR",]
paranapop <- parana$G12B...População.total.residente.do.s..município.s..com.esgotamento.sanitário..segundo.o.IBGE
paranapop <- paranapop[!is.na(paranapop)]
t.test(paranapop,alternative = "two.sided",mu = 493000)
tobs <- (mean(paranapop)-493000)/sqrt(var(paranapop)/length(paranapop))
tobs
2*(1-pt(tobs,length(paranapop)-1))

mtcars
par(mfrow=c(1,1))
boxplot(mtcars$hp~mtcars$am)

separados <- split(mtcars,mtcars$am)
separados

var.test(separados$`0`$hp,separados$`1`$hp)
s2a <- var(separados$`0`$hp)
s2b <- var(separados$`1`$hp)
s2a
s2b
s2a/s2b
2*pf(s2a/s2b,18,12)
sapply(separados,nrow)
t.test(x = separados$`0`$hp,y = separados$`1`$hp,alternative = "greater",
       paired =FALSE,var.equal = TRUE )

s2comb <- (18*var(separados$`0`$hp)+12*var(separados$`1`$hp))/30
(mean(separados$`0`$hp)-mean(separados$`1`$hp))/sqrt(s2comb/19+s2comb/13)

iris


t.test(iris$Sepal.Length,iris$Sepal.Width,alternative = "greater",paired = TRUE)
diferencas <- iris$Sepal.Length - iris$Sepal.Width
t.test(diferencas,alternative = "greater")
t.test(iris$Sepal.Length - iris$Sepal.Width,alternative = "greater")

boxplot(iris$Sepal.Length~iris$Species)

modelo <- aov(iris$Sepal.Length~iris$Species)
summary(modelo)
TukeyHSD(modelo)
shapiro.test(modelo$residuals)
ks.test(modelo$residuals,"pnorm",mean=0,sd=sd(modelo$residuals))

?pnorm
1 - pnorm(-.88)
pnorm(-.88,lower.tail = FALSE)
1 - pnorm(-.88)
pnorm(-.88,lower.tail = FALSE)
pnorm(.88)
pnorm(q = .73,mean = 0,sd = 1)
pnrom(1.5)-pnorm(-1.5)
pnorm(1.5)-pnorm(-1.5)
pnorm(q = 163,mean = 173,7.3)
(163-173)/7.3
pnorm(q = 163,mean = 173,sd = 7.3)
pnorm(q = (163-173)/7.3,mean = 0,sd = 1)
pnorm(180,173,7.3)
1-(pnorm(170,173,7.3))
pnrom(175,173,7.3) - pnorm(172,173,7.3)
pnorm(175,173,7.3) - pnorm(172,173,7.3)
x <- runif(10000,0,100)
hist(x)
x
mean(x)
populacao <- runif(10000,0,100)
hist(populacao)
populacao
mean(populacao)
medias <- c()
for(i in 1:1000){
  amostra <- sample(x = populacao,size = 10)
  medias[i] <- mean(amostra)
}
medias
hist(medias)
mean(populacao)
for(i in 1:1000){
  amostra <- sample(x = populacao,size = 30)
  medias[i] <- mean(amostra)
}
hist(medias)
medias <- c()
for(i in 1:1000){
  amostra <- sample(x = populacao,size = 100)
  medias[i] <- mean(amostra)
}
hist(medias)
populacao <- rexp(10000,1/5)
hist(populacao)
mean(populacao)
medias <- c()
for(i in 1:1000){
  amostra <- sample(x = populacao,size = 10)
  medias[i] <- mean(amostra)
}
medias
hist(medias)
for(i in 1:1000){
  amostra <- sample(x = populacao,size = 50)
  medias[i] <- mean(amostra)
}
hist(medias)
amostra <- sample(x = populacao,size = 100)
medias <- c()
for(i in 1:1000){
  amostra <- sample(x = populacao,size = 100)
  medias[i] <- mean(amostra)
}
hist(medias)
mean(populacao)
(pnorm((10-7.5)/2)-pnorm((5-7.5)/2))
(pnorm((10-7.5)/sqrt(2))-pnorm((5-7.5)/sqrt(2)))
1-(pnorm((10-7.5)/sqrt(2))-pnorm((5-7.5)/sqrt(2)))
1-(pnorm(10,7.5,sqrt(20/10))-pnorm(5,7.5,sqrt(20/10)))
pbinom(q = 70,size = 100,prob = .8)
pnorm((.7-.8)/sqrt(.8*.2/100))
pbinom(q = 700,size = 1000,prob = .8)
pnorm((.7-.8)/sqrt(.8*.2/1000))
qnorm(.05)
9-1.96*sqrt(4/50)
9+1.96*sqrt(4/50)
qnorm(.005)
9-2.57*sqrt(4/50)
9+2.57*sqrt(4/50)
qnorm(.05)
qnorm(.025)
qt(.025,49)
9-2*sqrt(5/50)
9+2*sqrt(5/50)
.25-1.96*sqrt(.25*.75/300)
.25+1.96*sqrt(.25*.75/300)
.25-1.96*sqrt(.25/300)
.25+1.96*sqrt(.25/300)

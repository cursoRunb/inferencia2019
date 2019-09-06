c(8-qnorm(.9)*sqrt(16/25),8+qnorm(.9)*sqrt(16/25))
c(8-qnorm(.925)*sqrt(16/25),8+qnorm(.925)*sqrt(16/25))
c(8-qnorm(.95)*sqrt(16/25),8+qnorm(.95)*sqrt(16/25))
c(8-qnorm(.975)*sqrt(16/25),8+qnorm(.975)*sqrt(16/25))

2*qnorm(.95)*sqrt(9^2/c(30,50,100))

c(9.3-qnorm(.97)*sqrt(2^2/40),9.3+qnorm(.97)*sqrt(2^2/40))
1.5/(2*sqrt(2^2/40))
pnorm(2.37)-pnorm(-2.37)

ocorrencias <- c(7, 11, 8, 9, 10,14, 6, 8, 8, 7, 8, 10, 10, 14, 12, 14, 12, 9, 11, 13, 13, 8, 6, 8, 13, 10, 14, 5, 14,10)
prop <- mean(ocorrencias>11)
c(prop-qnorm(.94)*sqrt(prop*(1-prop)/30),prop+qnorm(.94)*sqrt(prop*(1-prop)/30))
c(prop-qnorm(.94)*sqrt((1/4)/30),prop+qnorm(.94)*sqrt((1/4)/30))
prop.test(sum(ocorrencias>11),30,conf.level = .88)
c(360*prop-qnorm(.94)*sqrt(360*prop*(1-prop)/30),360*prop+qnorm(.94)*sqrt(360*prop*(1-prop)/30))

t.test(ocorrencias)
t.test(ocorrencias,conf.level = .99)


xcrit <- qnorm(.05)*sqrt(25/25)+50
xcrit
pnorm((47.3-50)/sqrt(25/25))

qt(.95,22)*sqrt(984^2/23)

(510-0)/sqrt(984^2/23)
1-pt(2.48,22)


erro2 <- function(n){
  pesocrit <- qt(.95,(n-1))*sqrt(984^2/n)    
  pnorm((pesocrit-200)/sqrt(984^2/n))
}
amostras <- 23:1000
erros <- erro2(amostras)
plot(amostras,erros,type="l")


erro2(50)

mtcars
t.test(mtcars$hp,alternative = "two.sided",mu = 122)







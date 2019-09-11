tarifas <- read.csv("https://raw.githubusercontent.com/gustavopompeu/ENAP/master/TarifaFornecimentoResidencial.csv")
head(tarifas)
summary(tarifas)
tarifas[,c("SigDistribuidora","VlrTRFBrancaPonta","VlrTRFBrancaIntermediaria")]
t.test(tarifas$VlrTRFBrancaPonta,tarifas$VlrTRFBrancaIntermediaria,
       alternative = "greater",paired = T,mu = .3)

t.test(tarifas$VlrTRFBrancaPonta-tarifas$VlrTRFBrancaIntermediaria,
       alternative = "greater",mu = .3)





t.test(tarifas$VlrTRFBrancaForaPonta,tarifas$VlrTRFBrancaIntermediaria,
       alternative = "less",paired = T,mu = .3)


var.test(tarifas$VlrTRFBrancaPonta[tarifas$nomConcessao=="Concessionária"],
         tarifas$VlrTRFBrancaPonta[tarifas$nomConcessao=="Permissionária"])

t.test(tarifas$VlrTRFBrancaPonta[tarifas$nomConcessao=="Concessionária"],
       tarifas$VlrTRFBrancaPonta[tarifas$nomConcessao=="Permissionária"],paired=FALSE)



for(i in 5:10){
  print(names(tarifas)[i])
  print(t.test(tarifas[tarifas$nomConcessao=="Concessionária",i],
               tarifas[tarifas$nomConcessao=="Permissionária",i]))
  print("________________________________________________")
}



mat1 <- matrix(c(56,71,12,
                 47,163,38,
                 14,42,85),ncol=3,byrow=T)
mat1

teste1 <- chisq.test(mat1)
names(teste1)
sum((teste1$observed-teste1$expected)^2/teste1$expected)
teste1$expected


wilcox.test(tarifas$VlrTUSDConvencional,mu = .5)

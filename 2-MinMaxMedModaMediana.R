#Esforço em Dias para a entrega de um projeto RPA
re <- fRPA$RealEffort

#Mínimo
minRPA <- min(re, na.rm = FALSE)
minRPA

#Primeiro quartil
fqRPA <- quantile(re, 0.25)
fqRPA

#Mediana
medianRPA <- median(re, na.rm = FALSE)
medianRPA

#Média
meanRPA <- mean(re, trim = 0, na.rm = FALSE)
meanRPA

#Moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
modeRPA <- getmode(re)
modeRPA

#Máximo
maxRPA <- max(re, na.rm = FALSE)
maxRPA

#Calcula a estatística descritiva de 1º quartil, média, mediana e máximo em dias dos projetos
summary(re)

#Gráfico
h <- hist(re, 
     breaks = 30,
     freq = NULL, 
     density = NULL, angle = 45, col = "orange", border = NULL,
     main = paste("RPAs entregues"),
     xlab = "Dias",
     ylab = "Quantidade")

text(h$mids,h$counts,labels=h$counts, col = "black", adj=c(0.5, -0.5), )



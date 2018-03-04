dataFrame <- read.table("/home/andrey/Downloads/bands.data", sep = ",")
res <- subset(dataFrame, V40 == "band" & V26 != '?', select = c("V26"))
sample <- as.numeric(as.character(res[,'V26']))
mean(sample)
minMean <- mean(sample)-1.96*sd(sample)/length(sample)^(0.5)
maxMean <- mean(sample)+1.96*sd(sample)/(length(sample))^(0.5)
minVar <- (length(sample)-1)*var(sample)/43.8
maxVar <- (length(sample)-1)*var(sample)/18.5

N <- 100
P <- 0.05
X <- rnorm(N, mean=4, sd=0.5)
Y <- rnorm(N, mean=4.2, sd=0.7) 
#with known var-s
expT <- (mean(X)-mean(Y))/((var(X)/length(X)+var(Y)/length(Y)))^(0.5)
F.border <- (1-2*P)/2
# if F(border)=F.border then border = 2.35, lborder = -2.35 (Laplas table for confInterval = 0.95)
lefCritT = -2.35


X1 <- rnorm(12, mean=4, sd=0.5)
Y1 <- rnorm(14, mean=4.1, sd=0.6)
DoF <- length(X1)+length(Y1)-2
expT1 <- ((mean(X1)-mean(Y1)) / (((length(X1)-1)*var(X1) + (length(Y1)-1)*var(Y1))^0.5))*(length(X1)*length(Y1)*DoF/(length(X1)+length(Y1)))
#from Student table: t(0.05, DoF)
critT1 <- 1.71

confInterval <- 0.95

var.test(X1, Y1, conf.level = confInterval)
t.test(X1, Y1, conf.level = confInterval)

?"rnorm"
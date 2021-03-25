table = "C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/Lookup Tables/SOIL-Pollination.csv"
dfTable = read.csv(file =table, header=TRUE)
par(mfrow = c(1, 3))
x = seq(0,100,0.1)

y = dfTable$Clay
n = exp(-(x-mean(y))*(x-mean(y))/(2*sd(y)*sd(y)))/(sd(y) * sqrt(2*pi))
plot(x,n/max(n),xlab="Clay (%)", ylab="Score")

y = dfTable$Sand
n = exp(-(x-mean(y))*(x-mean(y))/(2*sd(y)*sd(y)))/(sd(y) * sqrt(2*pi))
plot(x,n/max(n),xlab="Sand (%)", ylab="Score")

y = dfTable$Moisture
n = exp(-(x-mean(y))*(x-mean(y))/(2*sd(y)*sd(y)))/(sd(y) * sqrt(2*pi))
plot(x,n/max(n),xlab="Moisture (%)", ylab="Score")
library(kernlab)
library(ggplot2)
set.seed(6) #reproducibility


#рандомная выборка с нормальным распределением
#x <- rbind(matrix(rnorm(120), , 2), matrix(rnorm(120, mean = 3), , 2)) #vector with 120 rows and 2 columns
#y <- c(rep(1, 60), rep(-1, 60))

up <- 51
bot <- 150
x <- iris[up:bot, 3:4]
y <- c(rep(1,50), rep(-1, 50))
d <- data.frame(x=x,y=y)
names(d)<-c("x1", "x2", "y")
#qplot(x1, x2, data = d, color = factor(y)) + geom_point(shape = 1)
svp <- ksvm(y ~ x1 + x2, data = d, type = "C-svc", C = 1, kernel = "vanilladot", scaled = c())
plot(svp, data = d)
ymat <- ymatrix(svp)
#xlim = c(-5, 5), ylim = c(-3, 3),
#plot(c(min(x[,1]), max(x[,1])),c(min(x[,2]), max(x[,2])),type='n',xlab= "Petal.Length", ylab = "Petal.Width", main = "SVM. Линейно неразделимая выборка. Линейное ядро. С = 1")
#points(x[-SVindex(svp),1], x[-SVindex(svp),2], pch = ifelse(ymat[-SVindex(svp)] < 0, 2, 1))
#points(x[SVindex(svp),1], x[SVindex(svp),2], pch = ifelse(ymat[SVindex(svp)] < 0, 17, 16))

w <- colSums(coef(svp)[[1]] * x[SVindex(svp),])
b <- b(svp)
#w
# Draw the lines
#abline(b/w[2], -w[1]/w[2])
#abline((b+1)/w[2],-w[1]/w[2],lty=2)
#abline((b-1)/w[2],-w[1]/w[2],lty=2)



#typeof(w)
#as.double(d[1,1:2])
#ROC <- function(d, w) {
#  len <- dim(d)[1]
#  FRP <- vector()
#  TRP <- vector()
#  l_min <- 0
#  l_plu <- 0
#  func <- matrix(0,0,2)
#  for (i in 1:len) {
#    if (d[i,3] == -1) l_min <- l_min + 1
#    if (d[i,3] == 1) l_plu <- l_plu + 1
#    #d[i] <- c(d[i,], as.double(w %*% as.double(d[i, 1:2])))
#    func <- rbind(func, c(i, as.double(w %*% as.double(d[i, 1:2]))))
#  }
#  d <- cbind(d, func[,2])
#  d <- d[order(d[,4], decreasing = TRUE),]
#
#  FRP[1] <- 0
#  TRP[1] <- 0
#  AUC <- 0
#
#  for (i in 1:len) {
#    if (d[i, 3] ==  -1) {
#      FRP[i + 1] <- FRP[i] + 1 / l_min
#      TRP[i + 1] <- TRP[i]
#      AUC <- AUC + TRP[i + 1] / l_min
#    } else {
#      FRP[i + 1] <- FRP[i]
#      TRP[i + 1] <- TRP[i] + 1 / l_plu
#    }
#  }
#
#  plot(c(0, 1), c(0, 1), type = "n", xlab = "FRP", ylab = "TRP", main = "ROC-кривая")
#  points(FRP, TRP, type = "l")
#  legend("bottomright", paste("AUC = ", AUC))
#  abline(1, 0)
#
#}
#
#ROC(d, w)
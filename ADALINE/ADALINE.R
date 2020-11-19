ADALINE <- function (z, feature_matrix, labels, L, eps) {
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  w <- rep(0, n)
  #left <- min(iris$Petal.Length)
  #right <- max(iris$Petal.Length)
  left <- min(feature_matrix[,1])
  right <- max(feature_matrix[,1])
  coords <- matrix(0, 0, 2)
  tmp <- 1
  for (i in 1:n) {
    w[i] <- runif(1, -1 / (2 * tmp), 1 / (2 * tmp))
  }
  #print(w)
  Q <- 0
  for (i in 1:l) {
    Q <- Q + L((w %*% feature_matrix[i,]) * labels[i])
  }
  #print(Q)
  cnt <- 0
  lambda <- 1 / 200
  index <- 0
  while (Q > eps & cnt != 5) {
    index <- index + 1
    index <- runif(1, 1, l) %/% 1
    #print(index)
    etha <- 1 / feature_matrix[index,] %*% feature_matrix[index,]
    epsilon <- L((w %*% feature_matrix[index,]) * labels[index])
    #if (index %/% 10  ==  0) {print(epsilon)}
    if (epsilon < 1 /100) {next}
    w <- w - as.double(etha) * as.double(((w %*% feature_matrix[index,]) - labels[index])) * feature_matrix[index,]
    new_Q <- (1 - lambda) * Q + lambda * epsilon
    if (new_Q <= Q + 1e-6 & new_Q >= Q - 1e-6) {
      cnt <- cnt + 1
    }
    else {
      Q <- new_Q
      cnt <- 0
    }
    for (i in seq(left, right, 0.1)) {
      y <- (-w[1] * i) / w[2]
      coords <- rbind(coords, c(i, y))
    }
    if (Q >= eps) points(coords, type="l", col = "blue")
    #print (c(Q, cnt))
  }
  return(w)
}

L <- function(x) {
  return((x - 1) ^ 2)
}


labels <- rep(0, 100)
for (i in 1:50) {
  labels[i] <- -1
}
for (i in 51:100) {
  labels[i] <- 1
}

#w <- ADALINE(z, as.matrix(iris[1:100, 3:4]), labels, L, 5, 0.1, 0.1)

feature_matrix <- matrix(0,6  ,2)
feature_matrix[1,] <- c(0.8, 1)
feature_matrix[2,] <- c(0.65, 0.9)
feature_matrix[3,] <- c(0.6, 1.1)

feature_matrix[4,] <- c(1.2, 1)
feature_matrix[5,] <- c(1.4, 0.9)
feature_matrix[6,] <- c(1.3, 1.1)
#feature_matrix[4,] <- c(1, 1)
plot(feature_matrix)

labels <- rep(1, 6)
for (i in 1:3)  {
  labels[i] <- -1
}

colors <- c("-1" = "red", "1" = "green3")

plot(feature_matrix, pch = 21, col = colors[as.character(labels[])], bg = colors[as.character(labels[])])
w <- ADALINE(z, feature_matrix, labels, L, 5)
points(feature_matrix, pch = 21, col = colors[as.character(labels[])], bg = colors[as.character(labels[])])
#w
l <- min(feature_matrix[,1])
r <- max(feature_matrix[,1])
coords <- matrix(0, 0, 2)
for (i in seq(l, r, 0.1)) {
  y <- (-w[1] * i) / w[2]
  coords <- rbind(coords, c(i, y))
}
points(coords, type="l", col = "yellow")

#colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
#plot(iris[,3:4], pch = 21, col = colors[iris[,5]], bg = colors[iris[,5]], main = "Построение разделяющей гиперплоскости алгоритмом ADALINE")
#w <- ADALINE(z, as.matrix(iris[1:100, 3:4]), labels, L, 5)
#points(iris[,3:4], pch = 21, col = colors[iris[,5]], bg = colors[iris[,5]])



#l <- min(iris$Petal.Length)
#r <- max(iris$Petal.Length)
#coords <- matrix(0, 0, 2)
#for (i in seq(l, r, 0.1)) {
#  y <- (-w[1] * i) / w[2]
#  coords <- rbind(coords, c(i, y))
#}
#points(coords, type="l", col = "yellow")

#Q <- 0
#l <- 100
#feature_matrix <- as.matrix(iris[1:100, 3:4])
#for (i in 1:l) {
#    Q <- Q + L((w %*% feature_matrix[i,]) * labels[i])
#}
#Q

$\sigma_{11}x^2_1 + \sigma_{22}x^2_2 - x_1(2\mu_{r1}\sigma_{11} + \mu_{r2}\sigma_{21} + \mu_{r2}\sigma_{12}) - x_2(2\mu_{r2}\sigma_{22} +
\mu_{r1}\sigma_{21} + \mu_{r1}\sigma_{12}) + x_1x_2(\sigma_{12} + \sigma_{21}) + \mu^2_{r1}\sigma_{11} + \mu^2_{r2}\sigma_{22} + \mu_{r1}\mu_{r2}(\sigma_{12} +
\sigma_{21}) = $
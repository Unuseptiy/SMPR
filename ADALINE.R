ADALINE <- function (z, feature_matrix, labels, L, flag, eps) {
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  #print(n)
  #вектор весов
  w <- rep(0, n)
  #left <- min(iris$Petal.Length)
  #right <- max(iris$Petal.Length)
  left <- min(feature_matrix[,1])
  right <- max(feature_matrix[,1])
  coords <- matrix(0, 0, 2)

  #инициализация вектора весов
  tmp <- 1
  for (i in 1:n) {
    w[i] <- runif(1, -1 / (2 * tmp), 1 / (2 * tmp))
  }

  #подсчет ошибки
  Q <- 0
  for (i in 1:l) {
    Q <- Q + L((w %*% feature_matrix[i,]) * labels[i])
  }

  cnt <- 0
  #параметр сглаживания
  lambda <- 1 / 200
  while (Q > eps & cnt != 5) {
    index <- runif(1, 1, l) %/% 1
    #print(index)
    etha <- 1 / feature_matrix[index,] %*% feature_matrix[index,]
    epsilon <- L((w %*% feature_matrix[index,]) * labels[index])
    if (epsilon < 1 / 100) {next}
    if (flag == 1) w <- w - as.double(etha) * as.double(((w %*% feature_matrix[index,]) - labels[index])) * feature_matrix[index,]
    if (flag == 2) w <- w - as.double(etha) * feature_matrix[index,] * labels[index]
    new_Q <- (1 - lambda) * Q + lambda * epsilon
    if (new_Q <= Q + 1e-6 & new_Q >= Q - 1e-6) {
      cnt <- cnt + 1
    }
    else {
      Q <- new_Q
      cnt <- 0
    }
    for (i in seq(left, right, 0.1)) {
      y <- (-w[1] * i) / w[2] - w[3]
      coords <- rbind(coords, c(i, y))
    }
    if (Q >= eps) points(coords, type="l", col = "blue")

  }
  return(w)
}

L_ada <- function(x) {
  return((x - 1) ^ 2)
}

L_Hebb <- function(x){
  if (x < 1) return(-x)
  else return(0)
}


labels <- rep(0, 100)
for (i in 1:50) {
  labels[i] <- -1
}
for (i in 51:100) {
  labels[i] <- 1
}

#z <- c(3.0, 0.7)
#tmp <- matrix(0, 100, 1)
#for (i in 1:100) {
#  tmp[i] <- 1
#}
#feature_matrix <- cbind(as.matrix(iris[1:100, 3:4]), tmp)
##dim(feature_matrix)[2]
#w <- ADALINE(z, feature_matrix, labels, L, 5)

#работа на искусственной выборке
#feature_matrix <- matrix(0,6  ,2)
#feature_matrix[1,] <- c(0.8, 1)
#feature_matrix[2,] <- c(0.65, 0.9)
#feature_matrix[3,] <- c(0.6, 1.1)
#feature_matrix[4,] <- c(1.2, 1)
#feature_matrix[5,] <- c(1.4, 0.9)
#feature_matrix[6,] <- c(1.3, 1.1)
##feature_matrix[4,] <- c(1, 1)
#plot(feature_matrix)
#labels <- rep(1, 6)
#for (i in 1:3)  {
#  labels[i] <- -1
#}
#colors <- c("-1" = "red", "1" = "green3")
#plot(feature_matrix, pch = 21, col = colors[as.character(labels[])], bg = colors[as.character(labels[])])
#w <- ADALINE(z, feature_matrix, labels, L, 5)
#points(feature_matrix, pch = 21, col = colors[as.character(labels[])], bg = colors[as.character(labels[])])
##w
#l <- min(feature_matrix[,1])
#r <- max(feature_matrix[,1])
#coords <- matrix(0, 0, 2)
#for (i in seq(l, r, 0.1)) {
#  y <- (-w[1] * i) / w[2]
#  coords <- rbind(coords, c(i, y))
#}
#points(coords, type="l", col = "yellow")






#работа на ирисах
colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[,3:4], pch = 21, col = colors[iris[,5]], bg = colors[iris[,5]], main = "Построение разделяющей гиперплоскости алгоритмом ADALINE")
tmp <- matrix(0, 100, 1)
for (i in 1:100) {
  tmp[i] <- 1
}
feature_matrix <- cbind(as.matrix(iris[1:100, 3:4]), tmp)

weight <- ADALINE(z, feature_matrix, labels, L_Hebb, 2, 5)
points(iris[,3:4], pch = 21, col = colors[iris[,5]], bg = colors[iris[,5]])
weight
l <- min(iris$Petal.Length)
r <- max(iris$Petal.Length)
coords <- matrix(0, 0, 2)
for (x in seq(l, r, 0.1)) {
  y <- (-weight[1] * x) / weight[2] - weight[3]
  coords <- rbind(coords, c(x, y))
}
points(coords, type="l", col = "yellow")

Q <- 0
l <- 100

for (i in 1:l) {
    Q <- Q + L((weight %*% feature_matrix[i,]) * labels[i])
}
Q
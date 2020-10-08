#метрика
EM <- function (u, v) {
  return(sqrt(sum((u - v) ^ 2)))
}

epac <- function (x) {
  return (3/4 * (1 - x ^ 2) * (abs(x) <= 1))
}

quad <- function (x) {
  return (15/16 * (1 - x ^ 2) ^ 2 * (abs(x) <= 1))
}

tria <- function (x) {
  return ((1 - abs(x)) * (abs(x) <= 1))
}

gaus <- function (x) {
  return ((2 * pi) ^ (-1/2) * exp(1) ^ (-1/2 * x ^ 2))
}

rect <- function (x) {
  return (1/2 * (abs(x) <= 1))
}

test <- function(algo){
  x = -3
  deltaX = 0.1
  coords <- matrix(0, 0, 2)
  while(x <= 3) {
    coords <- rbind(coords, c(x, algo(x)))
    x <- x + deltaX
  }
  plot (coords, type = "l", xlab = "x", ylab = "y")
}


parzen <- function (feature_matrix, labels, z, h, core) {
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  cnt <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  for (i in 1:l) {
    cnt[labels[i]] <- cnt[labels[i]] + core(EM(feature_matrix[i,1:n], z) / h)
  }
  return(which.max(cnt))
}

#отрисовка ирисов Фишера
colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[,3:4], pch = 21, bg = colors[iris$Species],col = colors[iris$Species])

#задание 10 рандомных классифицируемых объектов и их отрисовка
for(i in 1:10){
  z <- c(runif(1, 0, 7), runif(1, 0, 2.5))
  #рисуем классифицируемый объект
  points(z[1], z[2], pch = 22, bg = colors[parzen(iris[,3:4], iris[,5], z, h, rect)], asp = 2)
}


#z <- c(runif(1, iris$Petal.Width), runif(1, iris$Petal.Length))
#h <- 2
#points(z[1], z[2], pch = 22, bg = colors[parzen(iris[,3:4], iris[,5], z, h, rect)], asp = 2)


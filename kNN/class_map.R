#метрика
EM <- function(u, v) {
  return(sqrt(sum((u - v) ^ 2)))
}

#линейка
ruler <- function(z, feature_matrix, metric_function = EM) {
  n <- dim(feature_matrix)[2]
  l <- dim(feature_matrix)[1]
  distances <- matrix(NA, l, 2)
  for(i in 1:l) {
    distances[i,] <- c(i, metric_function(feature_matrix[i,1:n], z))
  }
  distances <- distances[order(distances[,2]),]
  return(distances)
}


#принимает на вход обучающую выборку(features, labels), классифицируемый объект(z),
#возвращает класс (class), к которому алго относит классифицируемый объект,
# и k - количество соседей
#upd: фьючеры и лэйблы лучше передавать вместе, чтобы они сразу сортанулись или нет:)))
kNN <- function(feature_matrix, labels, z, k) {
  distances <- ruler(z, feature_matrix)
  cnt <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)

  for (i in 1:k) {
    class <- labels[distances[i,1]]
    cnt[class] <- cnt[class] + 1
  }
return(which.max(cnt))
}

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[,3:4], pch = 21, bg = colors[iris$Species],col = colors[iris$Species], xlab = "Petal.Length", ylab = "Petal.Width", main = "Карта классификации ирисов Фишера алгоритмом kNN, при k = 6")

#x <- max(iris$Petal.Length)
#y <- max(iris$Petal.Width)
#points(x, y, bg = colors[class], col = colors[class])


deltaX <- 0.1
deltaY <- 0.1
coords <- matrix(0, 0, 3)
index <- 1
x <- min(iris$Petal.Length)
y <- min(iris$Petal.Width)
while (x <= max(iris$Petal.Length)) {
  if (x == max(iris$Petal.Length)) print("+")
  y <- min(iris$Petal.Width)
  while (y <= max(iris$Petal.Width)) {
      class <- kNN(iris[, 3:4], iris[, 5], c(x, y), 6)
      points(x, y, bg = colors[class], col = colors[class])
    y <- y + deltaY
  }
  x <- x + deltaX
}

x <- max(iris$Petal.Length)
y <- min(iris$Petal.Width)
while (y <= max(iris$Petal.Width)) {
  class <- kNN(iris[, 3:4], iris[, 5], c(x, y), 6)
  points(x, y, bg = colors[class], col = colors[class])
  y <- y + deltaY
}

x <- min(iris$Petal.Lengt)
y <- max(iris$Petal.Width)
while (x <= max(iris$Petal.Length)) {
  class <- kNN(iris[, 3:4], iris[, 5], c(x, y), 6)
  points(x, y, bg = colors[class], col = colors[class])
  x <- x + deltaX
}

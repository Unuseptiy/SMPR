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

deltaX <- 0.25
deltaY <- 0.1
coords <- matrix(0, 0, 3)
index <- 1
x <- min(iris$Petal.Length)
y <- min(iris$Petal.Width)
#tmp <- matrix(0, 1, 3)
while (x <= max(iris$Petal.Length)) {
  y <- y <- min(iris$Petal.Width)
  while (y <= max(iris$Petal.Width)) {
    #tmp <- c(x, y, kNN(iris[, 3:4], iris[, 5], c(x, y), 6))
    coords <- rbind(coords, c(x, y, as.integer(kNN(iris[, 3:4], iris[, 5], c(x, y), 6))))
    y <- y + deltaY
  }
  x <- x + deltaX
}
#coords
colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(coords, pch = 21, bg = colors[coords[,3]], col = colors[coords[,3]], xlab = "Petal.Length", ylab = "Petal.Width")

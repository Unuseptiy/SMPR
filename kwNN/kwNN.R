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

kwNN <- function(feature_matrix, labels, z, k, q) {
  distances <- ruler(z, feature_matrix)
  cnt <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)

  for (i in 1:k) {
    class <- labels[distances[i,1]]
    cnt[class] <- cnt[class] + q ^ i
  }
return(which.max(cnt))
}

kwNN_LOO <- function(feature_matrix, labels, k_parametr_min_value, k_parametr_max_value, q_parametr_min_value, q_parametr_max_value, shedule_flag = FALSE){
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  delta <- 0.1
  rows <- k_parametr_max_value - k_parametr_min_value + 1
  cols <- (q_parametr_max_value - q_parametr_min_value) / delta + 1
  loo <- matrix(0, rows, cols)
  for(i in 1:l) {
    tmp_feature_matrix <- feature_matrix[-i, ]
    tmp_labels <- labels[-i]
    distances <- ruler(feature_matrix[i,1:n], tmp_feature_matrix)
    for (k in k_parametr_min_value:k_parametr_max_value){
      q <- q_parametr_min_value
      for (index in 1:cols) {
        cnt <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
        for (j in 1:k)  {
          cnt[tmp_labels[distances[j, 1]]] <- cnt[tmp_labels[distances[j, 1]]] + q ^ j
        }
        if (as.integer(which.max(cnt)) != as.integer(labels[i])){
            loo[k,index] <- loo[k,index] + 1
        }
        q <- q + delta
      }
    }
  }
  if (shedule_flag) {
    heatmap.2(loo, col = cm.colors(256), Rowv = NA, Colv = NA, main = "Тепловая карта зависимости ошибки от k и q"
    , xlab = "q", ylab = "k")
  }
  for (i in 1:rows) {
    for (j in 1:cols) {
      if (loo[which.min(loo)] == loo[i, j]){
        return(c(i, q_parametr_min_value + delta * (j - 1)))
      }
    }
  }
}

kwNN_LOO(iris[,3:4], iris[,5], 1, 149, 0.1, 1, TRUE)

#l <- dim(iris)[1]
#err <- 0
#for (i in 1:l) {
#  features <- iris[-i, 3:4]
#  labels <- iris[-i, 5]
#  if(as.integer(kwNN(features, labels, iris[i, 3:4], 3, 0.7)) != as.integer(iris[i, 5])) {
#    err <- err + 1
#  }
#}
#err


#colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
#plot(iris[,3:4], pch = 21, bg = colors[iris$Species],col = colors[iris$Species], xlab = "Petal.Length", ylab = "Petal.Width", main = "Карта классификации ирисов Фишера алгоритмом kwNN,\nпри k = 3, q = 0.6")
#
#deltaX <- 0.1
#deltaY <- 0.1
#coords <- matrix(0, 0, 3)
#index <- 1
#x <- min(iris$Petal.Length)
#y <- min(iris$Petal.Width)
#while (x <= max(iris$Petal.Length)) {
#  if (x == max(iris$Petal.Length)) print("+")
#  y <- min(iris$Petal.Width)
#  while (y <= max(iris$Petal.Width)) {
#      class <- kwNN(iris[, 3:4], iris[, 5], c(x, y), 3, 0.7)
#      points(x, y, bg = colors[class], col = colors[class])
#    y <- y + deltaY
#  }
#  x <- x + deltaX
#}
#
#x <- max(iris$Petal.Length)
#y <- min(iris$Petal.Width)
#while (y <= max(iris$Petal.Width)) {
#  class <- kwNN(iris[, 3:4], iris[, 5], c(x, y), 3, 0.7)
#  points(x, y, bg = colors[class], col = colors[class])
#  y <- y + deltaY
#}
#
#x <- min(iris$Petal.Lengt)
#y <- max(iris$Petal.Width)
#while (x <= max(iris$Petal.Length)) {
#  class <- kwNN(iris[, 3:4], iris[, 5], c(x, y), 3, 0.7)
#  points(x, y, bg = colors[class], col = colors[class])
#  x <- x + deltaX
#}
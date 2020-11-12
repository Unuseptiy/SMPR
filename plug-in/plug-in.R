#мат ожидание случайного вектора
mat_expect <- function(feature_matrix, probability) {
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  sum <- rep(0, n)
  for (i in 1:l) {
    sum <- sum + feature_matrix[i,] * probability[i]
  }
  sum <- as.numeric(sum)
  return(sum)
}

#подсчет матрицы ковариации
sigma_cnt <- function(feature_matrix, mu) {
  l <- dim(feature_matrix)[1]
  n <- length(mu)
  sum <- matrix(0, n, n)
  for (i in 1:l) {
    sum <- sum + (feature_matrix[i,] - mu) %*% t(feature_matrix[i, ] - mu)
  }
  sum <- sum / (l - 1)
  return(sum)
}

#плотность многомерного нормального распределения в точке
density <- function(x, M, Sigma) {
  first <- sqrt((2  * pi) ^ dim(Sigma)[1] * det(Sigma)) ^ (-1)
  second <- exp(t(x - M) %*% solve(Sigma) %*% (x - M) / -2)
  return(first * second)
}

#оптимальный байесовский классивикатор
OBC <- function (z, feature_matrix, labels, class_quan, lambda) {
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  p <- rep(0, class_quan)
  E <- matrix(0, 0, n)
  for (i in 1:class_quan){
    left <- (i - 1) * 50 + 1
    right <- (i - 1) * 50 + 50
    E <- rbind(E, mat_expect(feature_matrix[left:right,], rep(1/50, 50)))
  }
  for (i in 1:l) {
    if (as.integer(labels[i]) == 1) {
      p[1] <- p[1] + 1
    }
    if (as.integer(labels[i]) == 2) {
      p[2] <- p[2] + 1
    }
    if (as.integer(labels[i]) == 3) {
              p[3] <- p[3] + 1
    }
  }
  p <- p / l
  cnt <- c("setosa" =  0, "versicolor" = 0,  "virginca" = 0)
  for (i in 1:class_quan) {
    left <- (i - 1) * 50 + 1
    right <- (i - 1) * 50 + 50
    sigma <- sigma_cnt(feature_matrix[left:right, ],E[i, ])
    cnt[i] <- lambda[i] * p[i] * density(z, E[i,], sigma)
  }
  #print(cnt)
  return(which.max(cnt))
}

#отрисовка карты классификации
draw <- function() {
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  plot(iris[,3:4], pch = 21, col = colors[iris[,5]], bg = colors[iris[,5]], main = "Карта классификации ирисов Фишера plug-in алгортмом\nlambda_1 = lambda_2 = lambda_3 = 1")

  deltaX <- 0.1
  deltaY <- 0.1

  l <- min(iris$Petal.Length)
  r <- max(iris$Petal.Length)
  b <- min(iris$Petal.Width)
  t <- max(iris$Petal.Width)

  for (x in seq(l, r, deltaX)) {
    for (y in seq(b, t, deltaY)) {
      z <- c(x, y)
      class <- OBC(z, as.matrix(iris[,3:4]), iris[,5], 3, c(1, 1, 1))
      points(x, y, bg = colors[class], col = colors[class])
    }
  }
}

#попытка считать разделяющую плоскость
dividing_surface <- function(mu1, mu2, sigma) {
  sigma <- solve(sigma)
  coords <- matrix(0, 0, 2)
  left <- min(iris$Petal.Length)
  right <- max(iris$Petal.Length)
  for (x in seq(left, right, 0.1)){
    y <-
      (x * (2 * mu1[1] * sigma[1,1] + mu1[2] * (sigma[1,2] + sigma[2,1]) - 2 * mu2[1] * sigma[1,1] - mu2[2] * (sigma[1,2] + sigma[2,1])) +
      sigma[1,1] * (mu2[1]^2 + mu1[1]^2) + sigma[2,2]*(mu2[2]^2 + mu1[2]^2) + (sigma[1,2] + sigma[2,1]) * (mu2[1]*mu2[2] + mu1[1]*mu1[2]) ) /
      -(2 * mu1[2] * sigma[2,2] + mu1[1] * (sigma[1,2] + sigma[2,1]) - 2 * mu2[2] * sigma[2,2] - mu2[1] * (sigma[1,2] + sigma[2,1]))
    coords <- rbind(coords, c(x, y))
  }
  return(coords)
}
draw()
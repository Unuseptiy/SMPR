draw <- function(){
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  plot(iris[,3:4], pch = 21, col = colors[iris[,5]])

  for (i in 0:2) {
    l <- i * 50 + 1
    r <- i * 50 + 50
    E <- mat_expect(iris[l:r, 3:4], rep(1/50, 50))
    D <- dispersion(iris[l:r, 3:4], rep(1/50, 50))
    points(E, pch = 23, col = colors[iris[l,5]], bg = colors[iris[l,5]])
    tmp1 <- E[1] + sqrt(D[1])
    points(tmp1, E[2], pch = 22)
    tmp2 <- E[2] + sqrt(D[2])
    points(E[1], tmp2, pch = 22)
    tmp1 <- E[1] - sqrt(D[1])
    points(tmp1, E[2], pch = 22)
    tmp2 <- E[2] - sqrt(D[2])
    points(E[1], tmp2, pch = 22)
  }
}

#мат ожидание случайного вектора
mat_expect <- function(feature_matrix, probability) {
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  sum <- rep(0, n)
  for (i in 1:l) {
    sum <- sum + feature_matrix[i,] * probability[i]
  }
  return(sum)
}

dispersion <- function(feature_matrix, probability) {
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  E <- mat_expect(feature_matrix, probability)
  for_ME <- matrix(0,0, n)
  for (i in 1:l) {
    for_ME <- rbind(for_ME, ((feature_matrix[i,] - E) ^ 2))
  }
  return(mat_expect(for_ME, probability))
}

norm_dencity <- function(ksi, mu, sigma) {
  first <- 1 / (sigma * sqrt(2 * pi))
  second <- exp(-((ksi - mu) ^ 2) / (2 * sigma ^ 2))
  return(first * second)
}

NNBC <- function(z, feature_matrix, labels, class_quan, lambda){
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  p <- rep(0, class_quan)
  E <- matrix(0, 0, n)
  D <- matrix(0, 0, n)
  for (i in 1:class_quan){
    left <- (i - 1) * 50 + 1
    right <- (i - 1) * 50 + 50
    E <- rbind(E, mat_expect(feature_matrix[left:right,], rep(1/50, 50)))
    D <- rbind(D, dispersion(feature_matrix[left:right,], rep(1/50, 50)))
  }

  ######################################################################################
  #сделать так, чтобы априорные вероятности можно было ввести, а эту реализацию удалить
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
  ######################################################################################

  cnt <- c("setosa" =  0, "versicolor" = 0,  "virginca" = 0)
  for (i in 1:class_quan) {
    ln_pu <- 0
    for (j in 1:n) {
      ln_pu <- ln_pu + log(norm_dencity(z[j], E[i, j], sqrt(D[i, j])))
    }
    cnt[i] <- log(lambda[i]) + log(p[i]) + ln_pu
  }
  return(which.max(cnt))
}


  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  plot(iris[,3:4], pch = 21, col = colors[iris[,5]], bg = colors[iris[,5]], main = "Карта классификации ирисов Фишера алгоритмом NNBC\n(lambda_1 = 10, lambda_2 = lambda_3 = 1)")

  deltaX <- 0.1
  deltaY <- 0.1

l <- min(iris$Petal.Length)
r <- max(iris$Petal.Length)
b <- min(iris$Petal.Width)
t <- max(iris$Petal.Width)

for (x in seq(l, r, deltaX)) {
  for (y in seq(b, t, deltaY)) {
    z <- c(x, y)
    class <- NNBC(z, iris[,3:4], iris[,5], 3, c(10, 1, 1))
    points(x, y, bg = colors[class], col = colors[class])
  }
}
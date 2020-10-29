#метрика
EM <- function (u, v) {
  return(sqrt(sum((u - v) ^ 2)))
}

#функция, возвращающая отсортированный массив расстояний от классифицируемого объекта до элементов обучающей выборки
ruler <- function(z, feature_matrix, metric_function = EM) {
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  distances <- matrix(NA, length(feature_matrix[,1]), 2)
  for(i in 1:l) {
    distances[i,] <- c(i, metric_function(feature_matrix[i,1:n], z))
  }
  distances <- distances[order(distances[,2]),]
  return(distances)
}

epan <- function (x) {
  return (3/4 * (1 - x ^ 2) * (abs(x) <= 1))
}

quar <- function (x) {
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

poten <- function(feature_matrix, labels, z, core, h, gamma) {
  l <- dim(feature_matrix)[1]
  cnt <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  for (i in 1:l) {
    distance <- EM(z, feature_matrix[i,])
    if (distance <= h[i]) {
      cnt[labels[i]] <- cnt[labels[i]] + gamma[i] * core(distance / h[i])
    }
  }
  if(cnt["setosa"] != 0 | cnt["versicolor"] != 0 | cnt["virginica"] != 0) {
    return(which.max(cnt))
  } else {
    return(0)
  }
}


gamma_set <- function(feature_matrix, labels, core, eps) {
  l <- dim(feature_matrix)[1]
  gamma <- rep(0, l)
  h <- rep(0.4, l)
  loo <- eps + 1
  cnt <- 0
  prev_loo <- eps + 1
  while(loo >= eps & cnt < 5) {
    loo <- 0
    for (i in 1:l){
      pot <- poten(feature_matrix[-i,], labels[-i], feature_matrix[i,], core, h, gamma)
      if (as.integer(pot) != as.integer(labels[i])) {
        gamma[i] <- gamma[i] + 1
        loo <- loo + 1
      }
    }
    if (loo < prev_loo) {
      prev_loo <- loo
    } else {
      cnt <- cnt + 1
    }
  }
  return (gamma)
}

gamma <- gamma_set(iris[,3:4], iris[,5], gaus, 10)

#gamma <- read.csv("gamma.txt", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
#l <- length(gamma[,1])
#tmp <- vector()
#for (i in 1:l){
#  tmp[i] <- gamma[i, 1]
#}
#gamma <- tmp

#l <- dim(iris)[1]
#err <- 0
#h  <- rep(0, 150)
#for (i in 1:l) {
#  features <- iris[-i, 3:4]
#  labels <- iris[-i, 5]
#  if(as.integer(poten(features, labels, iris[i, 3:4], tria, h, gamma)) != as.integer(iris[i, 5])) {
#    err <- err + 1
#  }
#}
#err

tmp_colors <- vector()
l <- dim(iris)[1]
prev_Setosa <- 0
prev_Versicolor <- 0
prev_Virginica <- 0
for (i in 1:l) {
  if (i <= 50) {
    if(gamma[i] > prev_Setosa) {
      prev_Setosa <- gamma[i]
    }
  } else if (i <= 100) {
    if (gamma[i] > prev_Versicolor) {
      prev_Versicolor <- gamma[i]
    }
  } else {
    if (gamma[i] > prev_Virginica) {
      prev_Virginica <- gamma[i]
    }
  }
}

for (i in 1:l) {
  if (i <= 50) {
    alpha <- 255 %/% prev_Setosa * gamma[i]
    if (gamma[i] == 0) {
      alpha <- alpha + 25
    }
    tmp_colors[i] <- rgb(255, 0,0, alpha,,255)
  } else if (i <= 100) {
    alpha <- 255 %/% prev_Versicolor * gamma[i]
    if (gamma[i] == 0) {
      alpha <- alpha + 25
    }
    tmp_colors[i] <- rgb(0, 255,0, alpha,,255)
  } else {
    alpha <- 255 %/% prev_Virginica * gamma[i]
    if (gamma[i] == 0) {
      alpha <- alpha + 25
    }
    tmp_colors[i] <- rgb(0, 0,255, alpha,,255)
  }
}
plot(iris[,3:4], pch = 21, bg = tmp_colors, col = tmp_colors, xlab = "Petal.Length", ylab = "Petal.Width", main = "Распределение потенциалов по ирисам Фишера (треугольное ядро)")
#
#
#colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
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
#      class <- poten(iris[, 3:4], iris[, 5], c(x, y), epan, h, gamma)
#      points(x, y, bg = colors[class], col = colors[class])
#    y <- y + deltaY
#  }
#  x <- x + deltaX
#}
#
#x <- max(iris$Petal.Length)
#y <- min(iris$Petal.Width)
#while (y <= max(iris$Petal.Width)) {
#  class <- poten(iris[, 3:4], iris[, 5], c(x, y), epan, h, gamma)
#  points(x, y, bg = colors[class], col = colors[class])
#  y <- y + deltaY
#}
#
#x <- min(iris$Petal.Lengt)
#y <- max(iris$Petal.Width)
#while (x <= max(iris$Petal.Length)) {
#  class <- poten(iris[, 3:4], iris[, 5], c(x, y), epan, h, gamma)
#  points(x, y, bg = colors[class], col = colors[class])
#  x <- x + deltaX
#}
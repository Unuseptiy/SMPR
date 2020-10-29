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

kNN <- function(feature_matrix, labels, z, k, margin_flag = TRUE) {
  distances <- ruler(z, feature_matrix)
  cnt <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  for (i in 1:k) {
    class <- labels[distances[i,1]]
    cnt[class] <- cnt[class] + 1
  }
  return(cnt)
}

epan <- function (x) {
  return (3/4 * (1 - x ^ 2) * (abs(x) <= 1))
}

parzen <- function (feature_matrix, labels, z, h, core) {
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  cnt <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  for (i in 1:l) {
    cnt[labels[i]] <- cnt[labels[i]] + core(EM(feature_matrix[i,1:n], z) / h)
  }
  if(cnt["setosa"] != 0 | cnt["versicolor"] != 0 | cnt["virginica"] != 0){
    return(cnt)
  }
}

margin <- function(feature_matrix, labels, z, index) {
  cnt <- parzen(feature_matrix, labels, z, 0.4, epan)
  w1 <- cnt[iris[index, 5]]
  cnt[iris[index, 5]] <- 0
  w2 <- cnt[which.max(cnt)]
  M <- w1 - w2
  return(M)
}

margin_plot <- function(feature_matrix, labels) {
  l <- dim(feature_matrix)[1]
  y <- vector()
  for (i in 1:l)  {
    tmp_feature_matrix <- feature_matrix[-i,]
    tmp_labels <- labels[-i]
    y[i] <- margin(tmp_feature_matrix, tmp_labels, feature_matrix[i,], i)
  }
  y <- y[order(y)]
  #write.csv(y, "margin_plot_tmp.txt")
  plot(1:l, y, xlab = "i", ylab = "M", type = "l", main = "График отступов ирисов Фишера относительно\nалгоритма парзеновского окна (ядро Епанечникова)")
  points(1:l, y, pch = 21, cex = 0.5)
  lines(0:l, rep(0, l+1))
}

cool_descent <- function(M) {
  i <- 2
  prev_dif <- 0
  index <- 1
  while(M[i] <= 0) {
    dif <- M[i] - M[i - 1]
    if (dif > prev_dif) {
      prev_dif <- dif
      index <- i
    }
    i <- i + 1
  }
  for (i in 1:(index - 1)) {
    M <- M[-i]
  }
  return(M)
}

margin_plot(iris[,3:4], iris[,5])


#epan - 0.4
#quar - 0.4
#tria - 0.4
#gaus - 0.4
#rect - 0.4

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

parzen <- function (feature_matrix, labels, z, h, core) {
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  cnt <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  for (i in 1:l) {
    cnt[labels[i]] <- cnt[labels[i]] + core(EM(feature_matrix[i,1:n], z) / h)
  }
  if(cnt["setosa"] != 0 | cnt["versicolor"] != 0 | cnt["virginica"] != 0){
    return(which.max(cnt))
  }
}

parzen_LOO <- function(feature_matrix, labels, parametr_min_value, parametr_max_value, core, plot_flag = FALSE) {
  l <- dim(feature_matrix)[1]
  delta <- 0.1
  rows <- (parametr_max_value - parametr_min_value) / delta + 1
  loo <- matrix(0, ceiling(rows), 2)
  h <- parametr_min_value
  for (index in 1:rows) {
    loo[index, 1] <- h
    h <- h + delta
  }

  for (i in 1:l) {
    tmp_feature_matrix <- feature_matrix[-i,]
    tmp_labels <- labels[-i]
    h <- parametr_min_value
    distances <- ruler(feature_matrix[i,], tmp_feature_matrix)
    for (index in 1:rows) {
      cnt <- c("setosa" = 0, "versicolor" = 0, "virginca" = 0)
      j <- 1
      while(distances[j,2] <= h) {
        cnt[tmp_labels[distances[j,1]]] <- cnt[tmp_labels[distances[j,1]]] + core(distances[j, 2] / h)
        j <- j + 1
      }
      if (as.integer(which.max(cnt)) != as.integer(labels[i])){
            loo[index, 2] <- loo[index, 2] + 1
        }
      h <- h + delta
    }
  }

  if(plot_flag) {
    plot(loo[,1], loo[,2], xlab = "h", ylab = "loo", type = "l")
    points(which.min(loo), loo[which.min(loo)], pch = 20)
    location <- "topright"
    leg <- paste0("Риск минимален (количество\nошибок: ", as.character(loo[which.min(loo[,2]), 2]), ") при: h = ", as.character(loo[which.min(loo[,2]), 1]))
    legend(location, legend = leg)
  }
  return(loo[which.min(loo[,2]), 1])
}
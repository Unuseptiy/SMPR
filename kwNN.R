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

kwNN_LOO <- function(feature_matrix, labels, k, parametr_min_value, parametr_max_value, shedule_flag = FALSE){
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  delta <-  0.05
  rows <- (parametr_max_value - parametr_min_value) / delta + 1
  loo <- matrix(0, rows, 2)
  q <- parametr_min_value
  for (p in 1:rows) {
    loo[p, 1] <- q
    q <- q + delta
  }
  for(i in 1:l) {
    tmp_feature_matrix <- feature_matrix[-i, ]
    tmp_labels <- labels[-i]
    distances <- ruler(feature_matrix[i,1:n], tmp_feature_matrix)
    q <- parametr_min_value
    index <- 1
    while(q <= parametr_max_value) {
      cnt <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
      for (j in 1:k)  {
        cnt[tmp_labels[distances[j, 1]]] <- cnt[tmp_labels[distances[j, 1]]] + q ^ j
      }
      if (as.integer(which.max(cnt)) != as.integer(labels[i])){
          loo[index,2] <- loo[index,2] + 1
      }
      q <- q + delta
      index <- index + 1
    }
  }
  if (shedule_flag) {
    #plot(parametr_min_value:parametr_max_value, loo[parametr_min_value:parametr_max_value], xlab<-"k", ylab<-"loo", pch<-20)
    #lines(parametr_min_value:parametr_max_value, loo[parametr_min_value:parametr_max_value])
    plot(loo[,1], loo[,2], xlab = "q", ylab = "loo", type = "l")
  }
  print(loo)
  return(which.min(loo[,2]))
}

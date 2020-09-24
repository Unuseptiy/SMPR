#метрика
EM <- function(u, v) {
  return(sqrt(sum(u - v) ^ 2))
}

#линейка
ruler <- function(z, feature_matrix, metric_function = EM) {
  n <- dim(feature_matrix)[2]
  distances <- matrix(NA, length(feature_matrix[,1]), 2)
  for(i in 1:length(feature_matrix[,1])) {
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

#функция, выбрасывающая один элемент из матрицы признаков
throw_features <- function(feature_matrix, i, l) {
  if (i == 1) {
      out_feature_matrix <- feature_matrix[2:l,]
  } else if (i == l) {
      out_feature_matrix <- feature_matrix[1:(l - 1),]
  } else {
      out_feature_matrix <- rbind(feature_matrix[1:(i - 1),], feature_matrix[(i + 1):l,])
  }
  return(out_feature_matrix)
}

#функция, выбрасывающая один элемент из множества меток класса
throw_labels <- function(labels, i, l) {
  if (i == 1) {
      out_labels <- labels[2:l]
  } else if (i == l) {
      out_labels <- labels[1:(l - 1)]
  } else {
      out_labels <- c(labels[1:(i - 1)], labels[(i  + 1):l])
  }
  return(out_labels)
}

# LOO на вход передаем алгоритм, крание значения настраиваемого
# параметра, на выходе настроенный параметр, если флаг отрисовки графика
# установлен, то рисуем график, отображающий зависимость величины ошибки
# от значений настраиваемого параметра
kNN_LOO <- function(algorithm = kNN, feature_matrix, labels, parametr_min_value, parametr_max_value, shedule_flag = FALSE){
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  loo <- rep(0, parametr_max_value)
  #loo[1] <- 1000000
  for(i in 1:l) {
    tmp_feature_matrix <- throw_features(feature_matrix, i, l)
    tmp_labels <- throw_labels(labels, i, l)
    distances <- ruler(feature_matrix[i,1:n], tmp_feature_matrix)
    cnt <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
    for(tmp_parametr in parametr_min_value:parametr_max_value){
    #class <- algorithm(tmp_feature_matrix, tmp_labels, feature_matrix[i,1:n], tmp_parametr)
        class <- tmp_labels[distances[tmp_parametr,1]]
        cnt[class] <- cnt[class] + 1
      if (as.integer(which.max(cnt)) != as.integer(labels[i])) {
        loo[tmp_parametr] <- loo[tmp_parametr] + 1
      }
    }
  }
  if (shedule_flag) {
    plot(parametr_min_value:parametr_max_value, loo[parametr_min_value:parametr_max_value], xlab="k", ylab="loo", pch=20)
    lines(parametr_min_value:parametr_max_value, loo[parametr_min_value:parametr_max_value])
  }
  print(loo)
  return(which.min(loo))
}

z <- c(iris[1,1:2] - 1, iris[1,3:4] + 1)
#iris[1,1:4]
ruler(z, iris[,1:4])

#kNN_LOO(,iris[,1:4], iris[,5], 1, 149, TRUE)

##отрисовка ирисов Фишера
#colors <- c("1" = "red", "2" = "green3", "3" = "blue")
#plot(iris[,3:4], pch = 21, bg = colors[iris$Species],col = colors[iris$Species])
#
##k <- kNN_LOO(,iris[,3:4], iris[,5], 1, 15, TRUE)
#k <- 8
##задание 10 рандомных классифицируемых объектов и их отрисовка
#for(i in 1:10){
#  z <- c(runif(1, 0, 7), runif(1, 0, 2.5))
#  #рисуем классифицируемый объект
#  points(z[1], z[2], pch = 22, bg = colors[kNN(iris[,3:4], iris[,5], z, k)], asp = 1)
#}

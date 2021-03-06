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


# LOO на вход передаем алгоритм, крание значения настраиваемого
# параметра, на выходе настроенный параметр, если флаг отрисовки графика
# установлен, то рисуем график, отображающий зависимость величины ошибки
# от значений настраиваемого параметра
kNN_LOO <- function(feature_matrix, labels, parametr_min_value, parametr_max_value, plot_flag = FALSE){
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  loo <- rep(0, parametr_max_value)
  for(i in 1:l) {
    tmp_feature_matrix <- feature_matrix[-i, ]
    tmp_labels <- labels[-i]
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
  if (plot_flag) {
    plot(parametr_min_value:parametr_max_value, loo[parametr_min_value:parametr_max_value], xlab = "k", ylab = "loo", type = "l", main = "График зависимости ошибки от k")
    points(which.min(loo), loo[which.min(loo)], pch = 20)
    location <- "topleft"
    leg <- paste0("Функционал эмпирического риска \nминимален (количество ошибок: ", as.character(loo[which.min(loo)]), ") при: k = ", as.character(which.min(loo)))
    legend(location, legend = leg)
  }
  #print(length(parametr_min_value:parametr_max_value))
  #print(length(loo[parametr_min_value:parametr_max_value]))
  return(which.min(loo))
}

#kNN_LOO(iris[,3:4], iris[,5], 1, 149, TRUE)

##отрисовка ирисов Фишера
#colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
#plot(iris[,3:4], pch = 21, bg = colors[iris$Species],col = colors[iris$Species], main = "knn-классификация 10 рандомных объектов")
#
##задание 10 рандомных классифицируемых объектов и их отрисовка
#for(i in 1:10){
#  z <- c(runif(1, min(iris$Petal.Length), max(iris$Petal.Length)), runif(1, min(iris$Petal.Width), max(iris$Petal.Width)))
#  #рисуем классифицируемый объект
#  points(z[1], z[2], pch = 22, bg = colors[kNN(iris[,3:4], iris[,5], z, 6)])
#}



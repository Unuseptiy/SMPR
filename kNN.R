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
    class <-labels[distances[i,1]]
    #print(class)
    #print(cnt[class])
    cnt[class] <- cnt[class] + 1
  }
  #print(as.integer(labels[1]))
return(which.max(cnt))
}

#z <- c(4, 0.9)
#class <- kNN(iris[,3:4], iris[,5], z, 6)
#
##iris[1,5]
#if (as.integer(class) == as.integer(iris[1,5])) {
#  print(":)")
#} else {
#  print(":(")
#}


# LOO на вход передаем алгоритм, крание значения настраиваемого
# параметра, на выходе настроенный параметр, если флаг отрисовки графика
# установлен, то рисуем график, отображающий зависимость величины ошибки
# от значений настраиваемого параметра

kNN_LOO <- function(algorithm = kNN, feature_matrix, labels, parametr_min_value, parametr_max_value, shedule_flag = FALSE){
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  loo <- vector()
  for(tmp_parametr in parametr_min_value:parametr_max_value){
    loo <- c(loo, 0)
    for (i in 1:l) {
      if (i == 1) {
        tmp_feature_matrix <- feature_matrix[2:l,]
        tmp_labels <- labels[2:l]
      } else if (i == l) {
        tmp_feature_matrix <- feature_matrix[1:(l - 1),]
        tmp_labels <- labels[1:(l - 1)]
      } else {
        tmp_feature_matrix <- rbind(feature_matrix[1:(i - 1),], feature_matrix[(i + 1):l,])
        tmp_labels <- c(labels[1:(i - 1)], labels[(i  + 1):l])
      }
      class <- algorithm(tmp_feature_matrix, tmp_labels, feature_matrix[i,1:n], tmp_parametr)
      if (as.integer(class) != as.integer(labels[i])) {
        loo[tmp_parametr] <- loo[tmp_parametr] + 1
      }
    }
  }
  if (shedule_flag) {
    plot(1:parametr_max_value, loo[1:parametr_max_value], pch=20)
    lines(1:parametr_max_value, loo[1:parametr_max_value])
  }
  return(which.min(loo))
}

#tmp <- rep(0, 10)
#tmp

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


#str(iris[,5])

#suma <- function(a, b) {
#  return(a + b)
#}
#optima <- function(algo) {
#  prev_value <- -1000
#  for(i in 1:10) {
#    tmp_param <- i
#    value <- algo(1, tmp_param)
#    if(value > prev_value) {
#      prev_value <- value
#      param <- tmp_param
#    }
#  }
#  return(param)
#}
#
#optima(suma)


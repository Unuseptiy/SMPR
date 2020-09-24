#метрика
EM <- function(u, v) {
  return(sqrt(sum(u - v) ^ 2))
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

xy <- matrix(NA, 10, 2)
#for (i in 1:10) {
#  xy[i,] <- c(0, 1 / i)
#}
xy[1,] <- c(1, 1)
xy[2,] <- c(1, 2)

ruler(c(2, 1), xy)

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
kNN_LOO <- function(algorithm = kNN, feature_matrix, labels, parametr_min_value, parametr_max_value, shedule_flag = FALSE){
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  loo <- rep(0, parametr_max_value)
  for(i in 1:l) {
    #tmp_feature_matrix <- throw_features(feature_matrix, i, l)
    #tmp_labels <- throw_labels(labels, i, l)
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
  #loo <- loo / 150
  if (shedule_flag) {
    #plot(parametr_min_value:parametr_max_value, loo[parametr_min_value:parametr_max_value], xlab="k", ylab="loo", pch=20)
    #lines(parametr_min_value:parametr_max_value, loo[parametr_min_value:parametr_max_value])
    plot(parametr_min_value:parametr_max_value, loo[parametr_min_value:parametr_max_value], xlab = "k", ylab = "loo", type = "l")
  }
  #print(loo)
  return(which.min(loo))
}

colors <- c("setosa" = "red", "virginica" = "green3", "versicolor" = "blue")
plot(iris[,3:4], pch = 21, bg = colors[iris$Species],col = colors[iris$Species])

k <- 6
kef6 <- 0
cnt <- 0
for (i in 1:length(iris[,1])) {
  feature <- iris[-i, 3:4]
  label <- iris[-i, 5]
  class <- kNN(feature, label, iris[i, 3:4], k)
  if(as.integer(class) != as.integer(iris[i, 5])) {
    kef6 <- kef6 + 1

    if (cnt == 10) {
      points(iris[i, 3:4], pch=22, bg = colors[class])
      distances <- ruler(iris[i, 3:4], feature)
      for (j in 1:k) {
      #j<-4
        points(iris[distances[j,1],3:4], pch = 24, bg = colors[iris[distances[j,1],5]])
        #print(distances[j,1])
      }
      #print(iris[distances[3, 1],])
      #print(iris[distances[4, 1],])
      #print(c(i, names(class), as.character(iris[i,5])))
    }
    cnt <- cnt + 1
    #print(c(i, class, as.character(iris[i,5])))
  }
}
#kef6
#1:6
#k <- 8
#kef8 <- 0
#for (i in 1:length(iris[,1])) {
#  feature <- iris[-i, 3:4]
#  label <- iris[-i, 5]
#  if(kNN(feature, label, iris[i,3:4], k) != iris[i, 5]) {
#    kef8 <- kef8 + 1
#  }
#}
#kef6
#kef8

#kNN_LOO(,iris[,3:4], iris[,5], 1, 149, TRUE)

##отрисовка ирисов Фишера
#colors <- c("1" = "red", "2" = "green3", "3" = "blue")
#plot(iris[,3:4], pch = 21, bg = colors[iris$Species],col = colors[iris$Species])



##задание 10 рандомных классифицируемых объектов и их отрисовка
#for(i in 1:10){
#  #z <- c(runif(1, 0, 7), runif(1, 0, 2.5))
#  #рисуем классифицируемый объект
#  points(z[1], z[2], pch = 22, bg = colors[kNN(iris[,3:4], iris[,5], z, k)], asp = 1)
#}


#k <- 8
#x <- min(iris$Petal.Length)
#y <- min(iris$Petal.Width)
#while (x <= max(iris$Petal.Length)) {
#  y <- min(iris$Petal.Width)
#  while (y <= max(iris$Petal.Width)) {
#    class <- kNN(iris[,3:4], iris[,5], c(x, y), k)
#    #coords[]
#    points(x, y, pch = 22, bg = colors[class], col = colors[class], asp = 1)
#    y <- y + 0.2
#  }
#  x <- x + 0.2
#}

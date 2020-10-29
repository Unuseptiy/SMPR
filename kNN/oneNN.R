#евклидова метрика
EM <- function(u, v) {
  return(sqrt(sum((u - v) ^ 2)));
}

#функция, возвращающая отсортированный массив расстояний от классифицируемого объекта до элементов обучающей выборки
ruler <- function(z, feature_matrix, metric_function = EM) {
  n <- dim(feature_matrix)[2]
  distances <- matrix(NA, length(feature_matrix[,1]), 2)
  for(i in 1:length(feature_matrix[,1])) {
    distances[i,] <- c(i, metric_function(feature_matrix[i,1:n], z))
  }
  distances <- distances[order(distances[,2]),]
  return(distances)
}

#1nn алгоритм
oneNN <- function(z, feature_matrix) {
  distance <- ruler(z, feature_matrix, )
  class <- iris[distance[1,1], 5] #сортируем массив расстояний и получаем класс классифицируемого объекта
  return(class)
}

#отрисовка ирисов Фишера
colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[,3:4], pch = 21, bg = colors[iris$Species],col = colors[iris$Species], main = "1nn-классификация 10 рандомных объектов")

#задание 10 рандомных классифицируемых объектов и их отрисовка
for(i in 1:10){
  z <- c(runif(1, min(iris$Petal.Length), max(iris$Petal.Length)), runif(1, min(iris$Petal.Width), max(iris$Petal.Width)))
  #рисуем классифицируемый объект
  points(z[1], z[2], pch = 22, bg = colors[oneNN(z, iris[,3:4])], asp = 2)
}
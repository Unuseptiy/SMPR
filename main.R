#функцция метрики
EM <- function(u, v) {
  return(sqrt(sum((u - v) ^ 2)));
}

#функция, возвращающая массив расстояний от классифицируемого объекта до элементов обучающей выборки
dist <- function(z, xl, metricFunction = EM) {
  distances <- matrix(NA, length(xl[,1]), 2)
  for (i in 1:length(xl[,1])) {
    distances[i,] <- c(i, metricFunction(xl[i,1:2], z))
  }
  return(distances)
}

oneNN <- function(z, xl) {
  distance <- dist(z, xl, )
  class <- iris[order(distance[,2])[1], 5] #сортируем массив расстояний и получаем класс классифицируемого объекта
  return(class)
}

#отрисовка ирисов Фишера
colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[,3:4], pch = 21, bg = colors[iris$Species],col = colors[iris$Species])

#задание 10 рандомных классифицируемых объектов и их отрисовка
for(i in 1:10){
  z <- c(runif(1, 0, 7), runif(1, 0, 2.5))
  #рисуем классифицируемый объект
  points(z[1], z[2], pch = 22, bg = colors[oneNN(z, iris[,3:4])], asp = 2)
}

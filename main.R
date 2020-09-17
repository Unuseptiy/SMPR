# Title     : TODO
# Objective : TODO
# Created by: elenakozenko
# Created on: 16.09.2020


EM <- function(u, v) {
  return(sqrt(sum((u - v) ^ 2)));
}


dist <- function(z, metricFunction = EM) {
  distances <- matrix(NA, length(iris[,1]), 2)
  for (i in 1:length(iris[,1])) {
    distances[i,] <- c(i, metricFunction(iris[i,3:4], z))
  }
  return(distances)
}

#Работа алгоритма 1нн
z <- c(2.5, 0.7);
distance <- dist(z,)
class <- iris[order(distance[,2])[1], 5]
class;

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[,3:4], pch = 21, bg = colors[iris$Species],col = colors[iris$Species])

points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)

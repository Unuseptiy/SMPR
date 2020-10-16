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
  cnt <- c("a" = 0, "b" = 0)

  for (i in 1:k) {
    class <- labels[distances[i,1]]
    cnt[class] <- cnt[class] + q ^ i
  }
return(which.max(cnt))
}

kNN <- function(feature_matrix, labels, z, k) {
  distances <- ruler(z, feature_matrix)
  cnt <- c("a" = 0, "b" = 0)

  for (i in 1:k) {
    class <- labels[distances[i,1]]
    cnt[class] <- cnt[class] + 1
  }
return(which.max(cnt))
}

a <- vector()
b <- vector()
a[1] <- 1
a[2] <- 1
a[3] <- 4
a[4] <- 5
a[5] <- 4

b[1] <- 1
b[2] <- 3
b[3] <- 1
b[4] <- 2
b[5] <- 3

class <- vector()
class[1] <- "a"
class[2] <- "a"
class[3] <- "b"
class[4] <- "b"
class[5] <- "b"

feature_matrix <- cbind(a, b)
#feature_matrix

colors <- c("a" = "red", "b" = "blue")
plot(a, b, pch = 21, col = colors[class], bg = colors[class], cex = 3, main = "Пример kNN-классифифкации ")

#z <- c(2, 2)
x <- 2
y <- 2
cl1 <- kNN(feature_matrix, class, c(x, y), 5)
cl2 <- kwNN(feature_matrix, class, c(x, y), 5, 0.6)
points(x, y, pch = 22, col = colors[cl2], bg = colors[cl1], cex = 3)
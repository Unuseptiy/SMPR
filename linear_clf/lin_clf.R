#SGD <- function (f, f1, eps) {
#  w <- runif (1, -50, 50)
#  better_w <- 0
#  F <- f(w)
#  prevF <- F + 2 * eps
#  minF <- 1e6
#  cnt <- 0
#  min_cnt <- 0
#  #abs(F - prevF) > eps
#  while (min_cnt < 50) {
#    cnt <- cnt + 1
#    etha <- 1 / cnt
#    F1 <- f1(w)
#    w <- w - etha * F1
#    prevF <- F
#    F <- f(w)
#    print(c(minF, F, w))
#    if (F < minF) {
#      minF <- F
#      min_cnt <- 0
#      better_w <- w
#    }
#    else {
#      min_cnt <- min_cnt + 1
#    }
#  }
#  return(better_w)
#}
#
#f <- function (x) {
#  (x - 51) ^ 2
#}
#
#f1 <- function (x) {
#  2 * (x - 51)
#}
#
#SGD(f, f1, 0.01)

# добавим функцию подсчета ошибки не через сглаживающий параметр
SGD <- function (feature_matrix, labels, L, L1, flag = 0, eps) {
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  err <- matrix(0, l, 2)
  #вектор весов
  w <- rep(0, n)

  # вспомогательный вектор весов, хранящий веса, с которыми
  # ошибка минимальна (используется когда стабилизируется функционал ошибки)
  better_w <- rep(0, n)

  #инициализация вектора весов слцчайными небольшими значениями
  tmp <- 1
  for (i in 1:n) {
    w[i] <- runif(1, -1 / (2 * tmp), 1 / (2 * tmp))
  }

  #подсчет ошибки
  Q <- 0
  for (i in 1:l) {
    Q <- Q + L(M(feature_matrix[i,], labels[i], w))
    err[i,] <- c(i, L(M(feature_matrix[i,], labels[i], w)))
  }
  err <- err[order(err[,2], decreasing = TRUE),]
  Q_arr <- vector()
  Q_arr <- c(Q_arr, Q)

  #параметр сглаживания
  #lambda <- 1 / l

  #счетчик кол-ва шагов
  check <- 0

  #счетчик кол-ва шагов, на которых min(Q) не меняется
  min_cnt <- 0
  min_Q <- 1e6

  # выходим из массива, если:
  # ошибка Q меньше некоторой заданной;
  # количество шагов, на которых Q не уменьшалось больше некоторого заданного.
  while (Q > eps & min_cnt < 100) {
    check <- check + 1
    #print(c(check, w))
    #if (check %/% 10 == 0) {
      #выбираем элт на котором большая ошибка
      if (flag == 1) {
        index <- err[1,1]
      } else {
        index <- runif(1, 1, l) %/% 1
      }
    #} else {
    #  # выбираем случайный элемент

    #}

    #if (check == 1 | check == 2 | check == 3) print(index)

    # считаем ошибку на нем
    epsilon <- L(M(feature_matrix[index,], labels[index], w))

    #etha <- 1 / check
    etha <- 1 / as.double(feature_matrix[index,] %*% feature_matrix[index,])
    w <- w - etha * L1(feature_matrix[index,], labels[index], w)

    #Q <- (1 - lambda) * Q + lambda * epsilon
    Q <- 0
    for (i in 1:l) {
      Q <- Q + L(M(feature_matrix[i,], labels[i], w))
      err[i,] <- c(i, L(M(feature_matrix[i,], labels[i], w)))
    }
    err <- err[order(err[,2], decreasing = TRUE),]
    print(c(check, epsilon, Q, min_Q, min_cnt))

    # если Q не стало меньше минимального, полученного на предыдущих шагах, то не меняем его,
    # иначе -- меняем и обновляем счетчик шагов без изменения минимума и вектор лучших весов.
    if (Q >= min_Q) {
      min_cnt <- min_cnt + 1
    } else {
      min_Q <- Q
      min_cnt <- 0
      better_w <- w
    }
    Q_arr <- c(Q_arr, Q)
    abline(a = -w[3] / w[2], b = -w[1] / w[2], col = "blue")
  }

  #return(better_w)
  return (Q_arr)
}

M <- function (x, y, w) {
  as.double(w %*% x) * y
}

L_adaline <- function(x) {
  return((x - 1) ^ 2)
}

L1_adaline <- function(x ,y, w) {
  return(as.double(w %*% x - y) * x)
}

L_hebb <- function(x){
  if (x < 0) return(-x)
  else return(0)
}

L1_hebb <- function(x, y, w) {
  return(- x * y)
}

L_logistic <- function(x) {
  return(log(1 + exp(-x),2))
}

L1_logistic <- function(x, y, w) {
  return(-x * y * sigmoid(-as.double(w %*% x) * y))
}

ADALINE <- function(feature_matrix, labels) {
  weight <- SGD(feature_matrix, labels, L_adaline, L1_adaline, ,5)
  return(weight)
}

Hebbs_rule <- function(feature_matrix, labels) {
  weight <- SGD(feature_matrix, labels, L_hebb, L1_hebb, 1, 0)
  return(weight)
}

Logistic_regression <- function(feature_matrix, labels) {
  weight <- SGD(feature_matrix, labels, L_logistic, L1_logistic, ,1)
  return(weight)
}

sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
}

u <- 1
b <- 100
left <- 3
right <- 4
tmp <- matrix(1, 100, 1)

# формируем обучающую выборку
feature_matrix <- cbind(as.matrix(iris[u:b, left:right]), tmp)

max_x <- max(feature_matrix[,1])
min_x <- min(feature_matrix[,1])
delta_x <- max_x - min_x
max_y <- max(feature_matrix[,2])
min_y <- min(feature_matrix[,2])
delta_y <- max_y - min_y
# нормализация
for (i in 1:100) {
  feature_matrix[i, 1] <- feature_matrix[i, 1] / delta_x
  feature_matrix[i, 2] <- feature_matrix[i, 2] / delta_y
}
labels <- rep(1, 100)
for (i in 1:50) {
  labels[i] <- -1
}

#colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
#plot(iris[u:b,left:right], xlim = c(min(iris[u:b,3]), max(iris[u:b,3])), ylim = c(min(iris[u:b,4]), max(iris[u:b,4])), pch = 21, col = colors[iris[u:b,5]], bg = colors[iris[u:b,5]], main = "Разделяющие плоскости линейных классификаторов")
colors <- c("-1" = "red", "1" = "green3")
plot(feature_matrix[,1:2], pch = 21, col = colors[as.character(labels)], bg = colors[as.character(labels)])
weight <- ADALINE(feature_matrix, labels)
points(feature_matrix[,1:2], pch = 21, bg = colors[as.character(labels)])
abline(a = -weight[3] / weight[2], b = -weight[1] / weight[2], col = "yellow")
points(c(0, 0), c(min(iris[u:b,4]) - 5, max(iris[u:b,4]) + 5), type = "l")
points(c(min(iris[u:b,3]) - 5, max(iris[u:b,3]) + 5), c(0, 0), type = "l")

print("===================")

# отрисовка графиков для Q
Q <- Hebbs_rule(feature_matrix, labels)
plot(Q, xlab = "step", type="l", main = "Hebb's rule Q")
points(c(0, length(Q)), c(0, 0), type = "l")
# все, что тут используется как АДАЛАЙН переделать на СГД
# еще надо будет добавить передачу градиента, чтобы убрать реализацию через флаги
SGD <- function (feature_matrix, labels, L, flag, eps) {
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  #Q_arr <- vector()
  #вектор весов
  w <- rep(0, n)

  # вспомогательный вектор весов, хранящий веса, с которыми
  # ошибка минимальна (используется когда стабилизируется функционал ошибки)
  better_w <- rep(0, n)

  #параметры для отрисовки линии
  #left <- min(feature_matrix[,1])
  #right <- max(feature_matrix[,1])
  #coords <- matrix(0, 0, 2)

  #инициализация вектора весов слцчайнфми небольшими значениями
  tmp <- 1
  for (i in 1:n) {
    w[i] <- runif(1, -1 / (2 * tmp), 1 / (2 * tmp))
  }

  #подсчет ошибки
  Q <- 0
  for (i in 1:l) {
    Q <- Q + L((w %*% feature_matrix[i,]) * labels[i])
  }
  #Q_arr <- c(Q_arr, Q)
  # счетчик кол-ва итераций на которых Q менялся незначительно
  # в пределах [Q - 1e-2; Q + 1e-2]
  cnt <- 0

  #параметр сглаживания
  lambda <- 1 / l

  #счетчик кол-ва шагов
  check <- 0

  #счетчик кол-ва шагов, на которых min(Q) не меняется
  min_cnt <- 0
  min_Q <- 1e6

  # выходим из массива, если:
  # ошибка Q меньше некоторой заданной;
  # количество шагов, на которых Q меняется незначительно больше некоторого заданного;
  # количество шагов, на которых Q не уменьшалось больше некоторого заданного.
  # если условие на небольшое отклонение не будет использоваться -- удалить cnt
  while (Q > eps & cnt < 10 & min_cnt < 500) {
    check <- check + 1

    # выбираем случайный элемент
    index <- runif(1, 1, l) %/% 1

    # считаем ошибку на нем
    epsilon <- L((w %*% feature_matrix[index,]) * labels[index])

    # измение Q и w, в зависимости от выбранного лин клф:
    # ADALINE
    if (flag == 1) {
      if (epsilon < 1e-2) {next}
      etha <- 1 / feature_matrix[index,] %*% feature_matrix[index,]
      w <- w - as.double(etha) * as.double((w %*% feature_matrix[index,]) - labels[index]) * feature_matrix[index,]
    }

    # Hebbs rule
    if (flag == 2) {
      etha <- 1 / check
      if (epsilon > 0) w <- w + etha * feature_matrix[index,] * labels[index]
    }

    # Logistic regression
    if (flag == 3) {
      etha <- 1 / check
      w <- w + etha * feature_matrix[index,] * labels[index] * sigmoid(as.double(-w %*% feature_matrix[index,]) * labels[index])
    }

    # SVM
    if (flag == 4) {
      etha <- 1 / check
      w <- w - etha * feature_matrix[index,] * labels[index]
    }

    #etha <- 1 / check
    #w <- w - as.double(etha) * L1(feature_matrix[index,], labels[index], w)

    ######################################################################################################
    # эта запись для второго критерия выхода из массива
    #new_Q <- (1 - lambda) * Q + lambda * epsilon
    #print(c(epsilon, Q, new_Q, cnt))
    ## сравниваем новое Q с Q, полученным на предыдущих шагах и, если оно отличается
    ## меньше, чем на 1е-6, то Q не меняем, иначе -- меняем
    # это условие не надо в связи с тем, что условие на изменение минимума сильнее
    # (а если минимум меняется, но не сильно? тогда это уловие может сыграть свою роль)
    #if (new_Q >= Q - 1e-2 & new_Q <= Q + 1e-2) {
    #  cnt <- cnt + 1
    #}
    #else {
    #  Q <- new_Q
    #  cnt <- 0
    #}
    ######################################################################################################

    Q <- (1 - lambda) * Q + lambda * epsilon
    print(c(epsilon, Q, min_Q, min_cnt))
    # если Q не стало меньше минимального, полученного на предыдущих шагах, то не меняем его,
    # иначе -- меняем и обновляем счетчик шагов без изменения минимума и вектор лучших весов.
    if (Q >= min_Q) {
      min_cnt <- min_cnt + 1
    } else {
      min_Q <- Q
      min_cnt <- 0
      better_w <- w
    }
    #Q_arr <- c(Q_arr, Q)

    # незатратная отрисовка
    #if (Q >= eps) abline(a = -w[3] / w[2], b = -w[1] / w[2], col = "blue")
  }

  return(better_w)
  # для построения графиков для Q
  #return(Q_arr)
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
  return(x * y)
}

L_logistic <- function(x) {
  return(log(1 + exp(-x),2))
}

L1_logistic <- function(x, y, w) {
  return(x * y * sigmoid(as.double(-w %*% x) * y))
}

L_SVM <- function(x) {
  if (1 - x > 0) return(1 - x)
  else return(0)
}

ADALINE <- function(feature_matrix, labels) {
  weight <- SGD(feature_matrix, labels, L_adaline, 1, 5)
  return(weight)
#  L1_adaline
}

Hebbs_rule <- function(feature_matrix, labels) {
  weight <- SGD(feature_matrix, labels, L_hebb, 2, 5)
  return(weight)
  #L1_hebb
}

Logistic_regression <- function(feature_matrix, labels) {
  weight <- SGD(feature_matrix, labels, L_logistic, 3, 5)
  return(weight)
  #L1_logistic
}

SVM <- function(feature_matrix, labels) {
  weight <- SGD(feature_matrix, labels, L_SVM, 4, 5)
  return(weight)
}

sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
}

#работа на ирисах
u <- 1
b <- 100
left <- 3
right <- 4
colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[u:b,left:right], pch = 21, col = colors[iris[u:b,5]], bg = colors[iris[u:b,5]], main = "Разделяющие плоскости линейных классификаторов")
tmp <- matrix(0, 100, 1)
for (i in 1:100) {
  tmp[i] <- 1
}

# формируем обучающую выборку
feature_matrix <- cbind(as.matrix(iris[u:b, left:right]), tmp)
labels <- rep(1, 100)
for (i in 1:50) {
  labels[i] <- -1
}

# отрисовка графиков для Q
#Q <- Logistic_regression(feature_matrix, labels)
#plot(Q, type="l", main = "Logistic regression Q")

weight1 <- Logistic_regression(feature_matrix, labels)
weight2 <- Hebbs_rule(feature_matrix, labels)
weight3 <- Logistic_regression(feature_matrix, labels)

points(iris[u:b,left:right], pch = 21, col = colors[iris[u:b,5]], bg = colors[iris[u:b,5]])


#coords
# легкая отрисовка посчитанной гиперплоскости
abline(a = -weight1[3] / weight1[2], b = -weight1[1] / weight1[2], col = "black")
abline(a = -weight2[3] / weight2[2], b = -weight2[1] / weight2[2], col = "brown")
abline(a = -weight3[3] / weight3[2], b = -weight3[1] / weight3[2], col = "yellow")
legend('bottomright', c("ADALINE", "Hebbs rule", "Logistic regression"), lty=1, col=c('black', 'brown', 'yellow'), bty='n', cex=1)

# считаем эмпирическую оценку
#Q <- 0
#length <- 100
#L <- L_SVM
#for (i in 1:length) {
#    Q <- Q + L((weight %*% feature_matrix[i,]) * labels[i])
#}
#Q
#print("====================================================================")




# СЧИТАЕМ ЦВЕТА ДЛЯ ЛОГИСТИЧЕСКОЙ
#weight <- Logistic_regression(feature_matrix, labels)
##функция считает апостериорную вероятность
#aposterior <- function(w, x, y) {
#  return(sigmoid((w %*% x) * y))
#}
#
##считаем апостериорную вероятность эл-тов обучающей выборки
#PYX <- rep(0, b - u + 1)
#for (i in u:b) {
#  PYX[i - u + 1] <- aposterior(weight, feature_matrix[i - u + 1,], labels[i - u + 1])
#}
#PYX
#max_ver <- max(PYX)
#max_ver
#tmp_colors <- rep(0, 100)
##подбор интенсичности цветов
#for (i in u:b) {
#  alpha <- 255 %/% max_ver * PYX[i - u + 1]
#  if (i - u + 1 <= 50) {
#  tmp_colors[i - u + 1] <- rgb(255, 0,0, alpha,,255)
#  } else {
#    tmp_colors[i - u + 1] <- rgb(0, 255,0, alpha,,255)
#  }
#
#}
#plot(iris[u:b,left:right], pch = 21, bg = tmp_colors, col = tmp_colors, main = "Апостериорные вероятности для логистической регрессии")
#abline(a = -weight[3] / weight[2], b = -weight[1] / weight[2], col = "black")

#генерация выборки
#fx <- runif(50,-1,-0.1)
#fy <- runif(50,0,3)
#
#sx <- runif(50, 0.1,1)
#sy <- runif(50, 0,3)
#
#fclass <- cbind.data.frame(fx, fy, 1)
#sclass <- cbind.data.frame(sx, sy, -1)
#
#tip_name <- c("one", "two", "thri")
#
#names(fclass) <- tip_name
#names(sclass) <- tip_name
#
#for_ADALINE <- rbind.data.frame(fclass, sclass)
#labels <- for_ADALINE[,3]
#for_ADALINE[,3] <- rep(1, dim(for_ADALINE)[1])
#colors <- c("1"="red", "-1"="green")
#plot(for_ADALINE[,1:2], pch = 21, col=colors[as.character(labels)], bg = colors[as.character(labels)])
##for_ADALINE$thri <- as.factor(for_ADALINE$thri)
#
#weight <- ADALINE(z, as.matrix(for_ADALINE), labels, L_adaline, 1, 20)
#points(for_ADALINE[,1:2], pch = 21, col=colors[as.character(labels)], bg = colors[as.character(labels)])
#print("Weight")
#weight
#l <- min(for_ADALINE$one)
#r <- max(for_ADALINE$one)
#coords <- matrix(0, 0, 2)
#for (x in seq(l, r, 0.1)) {
#  y <- (-weight[1] * x - weight[3]) / weight[2]
#  coords <- rbind(coords, c(x, y))
#}
#points(coords, type="l", col = "yellow")
#
#Q <- 0
#l <- 100
#L <- L_adaline
##labels
#for (i in 1:l) {
#    Q <- Q + L((weight %*% as.double(for_ADALINE[i,])) * labels[i])
#}
#print("Q")
#Q

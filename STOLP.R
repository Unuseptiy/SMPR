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

epan <- function (x) {
  return (3/4 * (1 - x ^ 2) * (abs(x) <= 1))
}

parzen <- function (feature_matrix, labels, z, h, core) {
  l <- dim(feature_matrix)[1]
  n <- dim(feature_matrix)[2]
  cnt <- c("setosa" = 0, "versicolor" = 0, "virginica" = 0)
  for (i in 1:l) {
    cnt[labels[i]] <- cnt[labels[i]] + core(EM(feature_matrix[i,1:n], z) / h)
  }
    return(cnt)
}

#считает все отступы множества для классификации относительно обучающего множества
margin <- function(feature_matrix, labels, classification_set, classification_set_labels) {
  l <- dim(classification_set)[1]
  M <- matrix(0, 0, 4)
  for (i in 1:l) {
    z <- classification_set[i,]
    cnt <- parzen(feature_matrix, labels, z, 0.4, epan)
    w1 <- cnt[classification_set_labels[i]]
    cnt[classification_set_labels[i]] <- 0
    w2 <- cnt[which.max(cnt)]
    m <- w1 - w2
    M <- rbind(M, c(classification_set[i,1], classification_set[i,2], as.integer(classification_set_labels[i]), m))
  }
  M <- M[order(M[,4]),]
  return(M)
}

#считает все отступы айрисов отнсительно сотавшейся обучающей выборки
err <- function (feature_matrix, labels) {
  l <- dim(feature_matrix)[1]
  M <- matrix(0,0,4)
  for (i in 1:l) {
    tmp_feature_matrix <- feature_matrix[-i]
    tmp_labels <- labels[-i]
    cnt <- parzen(tmp_feature_matrix, tmp_labels, feature_matrix[i,], 0.4, epan)
    w1 <- cnt[labels[i]]
    cnt[labels[i]] <- 0
    w2 <- cnt[which.max(cnt)]
    m <- w1 - w2
    M <- rbind(M, c(feature_matrix[i,1], feature_matrix[i,2], as.integer(labels[i]), m))
  }
  M <- M[order(M[,4]),]
  return(M)
}

STOLP <- function (feature_matrix, labels, l0) {
  #M <- err(feature_matrix, labels)
  ############################################################################
  M <- read.csv("margin_STOLP_tmp.txt")
  l <- dim(M)[1]
  n <- dim(M)[2]
  tmp <- matrix(0, l, (n - 1))
  for (i in 1:l){
    tmp[i,] <- c(M[i, 2], M[i, 3], M[i, 4], M[i, 5])
  }
  M <- tmp
  ############################################################################
  i <- 1
  while (M[i, 4] <= 0) {
    M <- M[-i,]
  }

  l <- dim(M)[1]
  omega <- matrix(0,0,3)
  index <- vector()
  for (i in 1:3) {
    index[i] <- 0
  }
  for (i in l:1) {
    if (M[i, 3] == 1 & index[1] == 0) {
      omega <- rbind(omega, M[i, 1:3])
      index[1] <- i
    }
    if (M[i, 3] == 2 & index[2] == 0) {
      omega <- rbind(omega, M[i, 1:3])
      index[2] <- i
    }
    if (M[i, 3] == 3 & index[3] == 0) {
      omega <- rbind(omega, M[i, 1:3])
      index[3] <- i
    }
    if (index[1] != 0 & index[2] != 0 & index[3] != 0) {
      break
    }
  }
  index <- index[order(index)]
  for (i in 3:1) {
    M <- M[-index[i],]
  }
  l <- dim(M)[1]

  # тут надо бы добавить проверку множеств Ω и X на совпадение

  erro <- l
  while (erro > l0) {
    erro <- 0
    i <- 1
    tmpM <- margin(omega[,1:2], omega[,3], M[,1:2], M[,3])
    while(tmpM[i, 4] <= 0) {
      erro <- erro + 1
      i <- i + 1
    }
    omega <- rbind(omega, tmpM[1,1:3])
    M <- tmpM[-1,]
  }
  print(omega)
  return(omega)
}
omega <- STOLP(iris[,3:4], iris[,5], 10)

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[,3:4], pch = 21, col = colors[iris$Species], main = "STOLP-алгоритм. Черные точки - эталоны (15 эталонов).\nПрозрачные - обучающая выборка.\nЗолотые - ошибки на эталонах (6 ошибок).")
points(omega[,1:2], pch = 21, col = colors[omega[,3]], bg = "black")

er <- 0
l <- dim(iris)[1]
for (i in 1:l) {
  cnt <- parzen(omega[,1:2], omega[,3], iris[i, 3:4], 0.4, epan)
  if (cnt["setosa"] == 0 & cnt["versicolor"] == 0 & cnt["virginica"] == 0) {
    class <- 0
    points(iris[i,3:4], pch = 21, bg = "gold2")
    er <- er + 1
  } else {
    class <- which.max(cnt)
    if (as.integer(class) != as.integer(iris[i, 5])) {
      points(iris[i,3:4], pch = 21, bg = colors[class])
      er <- er + 1
    }
  }

}
#er
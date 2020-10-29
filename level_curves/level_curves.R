#Считает плотность нормального многомерного распределения с матожиданием М и
#матрицей ковариации Sigma
density <- function(x, M, Sigma) {
  first <- sqrt((2  * pi) ^ dim(Sigma)[1] * det(Sigma))
  second <- exp(t(x - M) %*% ginv(Sigma) %*% (x - M) / -2)
  return(second / first)
}

#считает значения плотности распределения с матожиданием М и матрицей
#ковариации Sigma и рисует линии уровня этой плотности
plotin <- function(M, Sigma) {
  d <- data.frame()
  left <- M[1] - (Sigma[1,1] + 1)
  right <- M[1] + (Sigma[1,1] + 1)
  bottom <- M[2] - (Sigma [2,2] + 1)
  top <- M[2] + (Sigma [2,2] + 1)

  for (i in seq(left, right, 0.1)) {
    for (j in seq(bottom, top, 0.1)){
      x <- c(i, j)
      ro <- density(x, M, Sigma)
      d <- rbind(d, c(i, j, ro))
    }
  }

names(d) <- c("x", "y", "z")
contourplot(z ~ x * y, d, main = "Линии уровня плотности нормального распределения\nдвумерной корелированной случайной величины\nс разными дисперсиями")
}

n <- 2
M <- matrix(c(3, 3), nrow = n, ncol = 1)
Sigma <- matrix(c(5, 2, 3, 5), nrow = n, ncol = n)
plotin(M, Sigma)
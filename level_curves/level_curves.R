#Считает плотность нормального многомерного распределения с матожиданием М и
#матрицей ковариации Sigma
density <- function(x, M, Sigma) {
  first <- sqrt((2  * pi) ^ dim(Sigma)[1] * det(Sigma)) ^ (-1)
  second <- exp(t(x - M) %*% ginv(Sigma) %*% (x - M) / -2)
  return(first * second)
}

#считает значения плотности распределения с матожиданием М и матрицей
#ковариации Sigma и рисует линии уровня этой плотности
plotin <- function(M, Sigma) {
  d <- data.frame()
  m  <- matrix(0,0,3)
  left <- M[1] - (Sigma[1,1] + 1)
  right <- M[1] + (Sigma[1,1] + 1)
  bottom <- M[2] - (Sigma[2,2] + 1)
  top <- M[2] + (Sigma[2,2] + 1)

  z <- matrix(0, 0, (top - bottom) / 0.1 + 1)
  for (i in seq(left, right, 0.1)) {
    tmp <- vector()
    for (j in seq(bottom, top, 0.1)){
      x <- c(i, j)
      ro <- density(x, M, Sigma)
      d <- rbind(d, c(i, j, ro))
      m <- rbind(m, c(i, j, ro))
      tmp <- c(tmp, ro)
    }
    z <- rbind(z, tmp)
  }


  #x <- seq(left, right, 0.1)
  #y <- seq(bottom, top, 0.1)
  #filled.contour(x, y, z, color=terrain.colors, xlab = "x", ylab = "y")
  names(d) <- c("x", "y", "z")
  contourplot(z ~ x * y, d, main = "Линии уровня плотности нормального распределения\nдвумерной корелированной случайной величины\nс разными дисперсиями")
}


M <- matrix(c(3, 3), nrow = 2, ncol = 1)
Sigma <- matrix(c(5, 0, 0, 1), nrow = 2, ncol = 2)
plotin(M, Sigma)
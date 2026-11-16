simpson38_comp <- function(f, a, b, n) {
  if (n %% 3 != 0) {
    stop("n harus kelipatan 3.")
  }
h <- (b - a) / n
x <- seq(a, b, length.out = n + 1)
y <- f(x)

idx3 <- seq(2, n, by = 3)
idx2 <- seq(3, n, by = 3)

3*h/8 * (y[1] + 3*sum(y[idx3]) +
           2*sum(y[idx2]) + y[n+1])
}

f <- function(x) x^3
simpson38_comp(f, 0, 3, 3)


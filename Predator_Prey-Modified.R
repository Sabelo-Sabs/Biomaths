Nx <- 100
Ny <- 100
xgrid <- setup.grid.1D (x.up = 0, x.down = 1, N = Nx)
ygrid <- setup.grid.1D (x.up = 0, x.down = 1, N = Ny)
x <- xgrid$x.mid
y <- ygrid$x.mid
laplace <- function(t, U, parms) {
  w <- matrix(nrow = Nx, ncol = Ny, data = U)
  dw <- tran.2D(C = w, C.x.up = 0, C.x.down = 0,
                flux.y.up = 0,
                flux.y.down = -1 * sin(pi*x)*pi*sinh(pi),
                D.x = 1, D.y = 1,
                dx = xgrid, dy = ygrid)$dC
  list(dw)
}
print(system.time(
  out <- steady.2D(y = runif(Nx*Ny), func = laplace,
                   parms = NULL, nspec = 1,
                   dimens = c(Nx, Ny), lrw = 1e7)
))

w <- matrix(nrow = Nx, ncol = Ny, data = out$y)
analytic <- function (x, y) sin(pi*x) * cosh(pi*y)
OutAna <- outer(x, y, FUN = analytic)
max(abs(w - OutAna))

image(out, grid = list(x, y), main = "elliptic Laplace",
      add.contour = TRUE)
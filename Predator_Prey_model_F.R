#Library for solving the ode's
library(deSolve)

#The function that takes the ode's, time and parameters
LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}
#Setting parameter values.
Pars <- c(alpha = 2, beta = .5, gamma = .2, delta = .6)

#Initial Conditions of the model
State <- c(x = 10, y = 10)
# Total time to run the model.
Time <- seq(0, 100, by = 1)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))
matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)

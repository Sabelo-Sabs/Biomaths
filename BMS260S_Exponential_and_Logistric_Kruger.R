#The Kruger National Park springbok population in 1910 and 1960 was 5.3 and
#23.1 hundred thoushand respectively. Predict its population in 2010 and in 2060 using the
#exponential model of population growth. Then considering that the population in 2010 was
#actually 76 thoushand, correct your prediction for 2060 using the logistic model of population
#growth (help: with this data, the growth rate is r = 0.031476 in the logistic model).

# Exponential Growth Model
dNt <- function(r, N) r * N 

# iterate growth through time
Nt <- function(r, N, t) {
  for (i in 1:(t - 1)) {
    # population at next time step is population at current time + pop growth
    N[i + 1] <- N[i] + dNt(r, N[i])
  }
  N
}

t <- 500# The total time to run our model.
r <-0.029443# The growth rate given in the example.
# lets consider 4 different starting initial population values (i.e., N(t=0) values)
Nt0 = c(5.3, 23.1, 35, 50)

par(mfrow=c(2,2))
for (i in seq_along(Nt0)) {
  plot(1:t, Nt(r,Nt0[i], t), type = 'l', xlab = 'time', ylab = 'Population size',
       main = paste('N(t=0) =', Nt0[i]), xlim=c(0,500),ylim =c(0,1000),lwd=2,col="blue")
  abline(h = 440, lty = 2, col='red',lwd=2)
}


# logistic growth model
dNt <- function(r, N) r * N * (1 - N/189.4)

# iterate growth through time
Nt <- function(r, N, t) {
  for (i in 1:(t - 1)) {
    # population at next time step is population at current time + pop growth
    N[i + 1] <- N[i] + dNt(r, N[i])
  }
  N
}

t <- 500# The total time to run our model.
r <- 0.031476# The growth rate given in the example.
# lets consider 4 different starting abundances (i.e., N(t=0) values)
Nt0 = c(1, 23.1, 76, 110)

par(mfrow=c(2,2))
for (i in seq_along(Nt0)) {
  plot(1:t, Nt(r,Nt0[i], t), type = 'l', xlab = 'time', ylab = 'Population size',
       main = paste('N(t=0) =', Nt0[i]), xlim=c(0,500),ylim =c(0, 200),lwd=2,col="blue")
  abline(h = 189.4, lty = 2, col='red',lwd=2)
}
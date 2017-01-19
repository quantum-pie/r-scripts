# function to simulate random walk of particle on euclidian surface
# radius - when particle is outside the circle with center in (0, 0) ind given radius, simulation is over
# n_max - maximum number of walk iterations
# p - probability of particle absorbtion
simulate_walk <- function(radius = 6, n_max = 100, p = 1e-2) {
  current_pos <- c(0, 0)
  for (i in 1:n_max) {
    # result is binomial sample of one experiment
    is_absorbed <- rbinom(1, 1, p)
    if (is_absorbed) return(1)
    
    # randow walk on the surface
    current_pos <- current_pos + rnorm(2)
    
    # current distance from the (0, 0)
    distance <- norm(current_pos, type = "2")
    if (distance > radius) return(2)
  }
  return(3)
}

# number of walk simulations
repetitions <- 1e5
res <- replicate(repetitions, simulate_walk(), simplify = T)

# probability of getting out of circle while simulation
prob <- 100 * length(res[res == 2]) / repetitions

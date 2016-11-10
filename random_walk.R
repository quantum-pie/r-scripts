simulate_walk <- function(radius = 6, n_max = 100, p = 1e-2) {
  current_pos <- c(0, 0)
  for (i in 1:n_max) {
    is_absorbed <- rbinom(1, 1, p)
    if (is_absorbed) return(1)
    current_pos <- current_pos + rnorm(2)
    distance <- norm(current_pos, type = "2")
    if (distance > radius) return(2)
  }
  return(3)
}

repetitions <- 1e5
res <- replicate(repetitions, simulate_walk(), simplify = T)
prob <- 100 * length(res[res == 2]) / repetitions

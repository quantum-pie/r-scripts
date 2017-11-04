# task1 - create vectors
vec1 <- c(1:100, 99:1)
vec2 <- seq(from = 0, to = 1, by = 0.01)
vec3 <- rep(1:3, 100)

# task2 - create vector
my_vector <- paste0("element", 100:1)

# task3 - create vector
arg <- 1:100
my_vector <- arg * arg * cos(arg)

# task4 = create vector and get positions of its first 10 maximum values
arg <- 1:100
my_vector <- arg^rev(arg)
max_positions <- order(my_vector, decreasing = T)[1:10]

# task5 - calculate sum
arg <- 1:200
my_sum <- sum(arg^2 + 2*sqrt(arg))

# task6 - create function to replace last matrix column with products of values in other columns
mutate_matrix <- function(m) {
  m[,ncol(m)] <- apply(m[,-ncol(m)], 1, prod)
  return(m)
}

mm <- matrix(1:6, nrow = 2)
mm2 <- mutate_matrix(mm)

# task7 - create matrix with ones on the diagonals and zeros at other places
decorate_matrix <- function(n) {
  m <- matrix(0, nrow = n, ncol = n)
  diag(m) <- rep(1, n)
  # trick is to get diagonal of upside down matrix
  diag(m[n:1,]) <- rep(1, n)
  return(m)
}

mm <- decorate_matrix(5)

# task8 - get index of column with maximum mean
max_mean_col <- function(m) {
  which(colMeans(m) == max(colMeans(m)))
}

# task9 - get elements of v1 than are the most frequent in v2
get_max_element <- function(v1, v2) {
  # frequency table of v2 with respect to v1 values
  times <- table(v2)[v1]
  # self-documentary
  as.integer(names(which(times == max(times))))
}

# task10 - cut string on adjacent characters pairs
cut_string <- function(x) {
  parts <- nchar(x) - 1
  pos <- 1:parts
  # trick is to replicate string pairs time and get substrings
  substr(rep(x, parts), pos, pos + 1)
}

# task11 - dplyr: group df by V3 and V4 and for each pair of those get two rows with minimum V1 value
library("dplyr")
library("tidyr")
d <- read.csv("https://stepik.org/media/attachments/course/724/data.csv")

res <- 
  d %>%
    group_by(V3, V4) %>%
    filter(V1 == sort(V1)[1] | V1 == sort(V1)[2])

# task12 - dplyr: calculate main statistics of df
res <- 
  d %>%
    group_by(V3, V4) %>%
    summarise(mean = mean(V1), sd = sd(V1), max = max(V1), min = min(V1))

# task13 - create snake matrix
snake_matrix <- function(n){
  # not snaked almost
  m <- 1:n^2
  dim(m) <- c(n, n)
  # reverse even columns, now snaked
  sapply((1:(n%/%2))*2, function(x) m[, x] <<- rev(m[, x]))
  return(m)
}

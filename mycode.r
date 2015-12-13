myfunction <- function() {
  x <- rnorm(100)
}

second <- function(x) {
  x + rnorm(length(x))
}
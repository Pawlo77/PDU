set.seed(123)

x <- round(rnorm(20, 0, 1), 2)

# 1.1.1
x[(x >= -2 & x <= -1) | (x >= 1 & x <= 2)]

# 1.1.2
f <- x[x >= 0]
length(f)
length(f) / length(x)
# alternatywnie
sum(x >= 0) / length(x)
#
mean(x >= 0)

# 1.1.3
mean(abs(x))

# 1.1.4
x[abs(x) == min(abs(x))]
x[abs(x) == max(abs(x))]
# alternatywnie
x[which.min((abs(x)))]
x[which.max((abs(x)))]

# 1.1.5
x[abs(x - 2) == min(abs(x - 2))]
x[abs(x - 2) == max(abs(x - 2))]
# alternatywnie
x[which.min((abs(x - 2)))]
x[which.max((abs(x - 2)))]

# 1.1.6
(x - min(x)) / (max(x) - min(x))

# 1.1.7
y <- c()
for (i in x) {
  if (i >= 0) {
    y <- c(y, "nieujemna")
  } else {
    y <- c(y, "ujemna")
  }
}
y
# alternatywnie
ifelse(x >= 0, "nieujemna", "ujemna")
#
y <- character(length(x))
y[x < 0] <- "ujemna"
y[x >= 0] <- "nieujemna"
y
#
c("nieujemna", "ujemna")[(x < 0) + 1]

# 1.1.8
y <- c()
for (i in x) {
  if (i < -1) {
    y <- c(y, "mały")
  } else if (i <= 1) {
    y <- c(y, "średni")
  } else {
    y <- c(y, "duży")
  }
}
y
# alternatywnie
ifelse(x <= -1, "mały", ifelse(x >= 1, "duzy", "sredni"))

# 1.1.9
y <- c()
for (i in x) {
  k <- floor(i)
  y <- c(y, k + 1 / 2)
}
y
# alternatywnie
floor(x) + 0.5


# 1.2
pearson <- function(x, y) {
  eps <- sum((x - mean(x)) / sd(x) * (y - mean(y)) / sd(y))
  return(1 / (length(x) - 1) * eps)
}

# a
x <- rnorm(20, 0, 1)
y <- 10 * x + 2
pearson(x, y)
# b
x <- rnorm(20, 0, 1)
y <- -4 * x + 1
pearson(x, y)
# c
x <- rnorm(2000, 0, 1)
y <- rnorm(2000, 5, 2)
pearson(x, y)


# 1.3
kor <- function(x, y) {
  d <- rank(x) - rank(y)
  n <- length(x)
  return(1 - (6 * sum(d^2)) / (n * (n^2 - 1)))
}
kor(x, y)


# 1.4
win <- function(x, k) {
  n <- length(x)
  stopifnot("Błędne k" = (k <= (n - 1) / 2))
  x <- sort(x)
  x[1:k] <- x[k + 1]
  x[n - k:n] <- x[n - k - 1]
  return(mean(x))
}
win(rnorm(20), 5)


# 1.5
factorial2 <- function(n) {
  stopifnot("Błędne n" = (n %% 1 == 0 & n > 0))
}
factorial_stirling <- function(n) {
  stopifnot("Błędne n" = (n %% 1 == 0 & n > 0))
  return((n / exp(1))^n * sqrt(2 * pi * n))
}
for (n in seq(5, by = 5, length.out = 3)) {
  factorial(n)
  factorial_stirling(n)
}


# 1.6
arcs <- function(x, m) {
  stopifnot("Błędne x" = (-1 <= x & x <= 1), "Błędne m" = (m %% 1 == 0 & m > 0))
  n <- 0:m
  return(sum((factorial(2 * n) * x^(2 * n + 1)) / (4^n * factorial(n)^2 * (2 * n + 1))))
}
x <- 0.75
arcs(1, 10)
for (n in 10^(1:3)) {
  print(abs(asin(x) - arcs(x, n)))
}


# 1.7
top <- c("_", " ", "_", "_", " ", "_", "_", "_", "_", "_")
mid <- c("| |", " |", " _|", " _|", "|_|", "|_ ", "|_ ", " |", "|_|", "|_|")
bot <- c("|_|", " |", "|_ ", " _|", " |", " _|", "|_|", " |", "|_|", " _|")

print_kalk <- function(x) {
  stopifnot("Błędne m" = (x %% 1 == 0 & n > 0))
  print(paste(toString(StrAlign(gettextf("%3s", top[x + 1]), sep = "\\c"))))
  print(paste(toString(mid[x + 1])))
  print(paste(toString(bot[x + 1])))
}
x <- 1:4
print_kalk(x)
?sprintf

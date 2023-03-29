# 1
?rle
licznosc <- rle(c(rep(1, times = 10), 1:10))
licznosc$lengths
licznosc$values

# 2
calkaMonteCarlo <- function(f, a, b, n = 1000) {
    stopifnot(
        "f nie jest funkcja" = is.function(f),
        "a, b numeryczne 1 el" = (is.numeric(a) && is.numeric(b) && length(a) == 1 && length(b) == 1),
        "NOT a < b" = (a < b),
        "n ma byc z N" = (is.numeric(n) && n > 0 && n == floor(n)),
        "f(a), f(b) - nieujemne" = (f(c(a, b)) >= 0)
    )

    f_max <- max(f(c(a, b)))
    f_min <- min(f(c(a, b)))

    x <- runif(n, a, b)
    y <- runif(n, f_min, f_max)

    choosen <- mean(y <= f(x))
    p <- (b - a) * (f_max - f_min)

    return(p * choosen + (b - a) * f_min)
}

set.seed(123)
a <- 0
b <- 1
n <- 10000

f <- function(x) sin(x)
calkaMonteCarlo(f, a, b, n)
f <- function(x) x + 1
calkaMonteCarlo(f, a, b, n)
f <- function(x) x^2 + 2
calkaMonteCarlo(f, a, b, n)
f <- function(x) x
calkaMonteCarlo(f, a, b, n)

# 3
x <- c("a", "b", "c")
k <- 5
sample(x, k, replace = TRUE) # replace - ze zwracaniem

# 4
x <- c(1, 2, 1, 4, 3, 4, 1)

# 5
zad_5 <- function(x, y) {
    stopifnot(
        "len(x) <= len(y)"=(length(x) <= length(y),
        "x, y - el dodatnie cał"
    )
}
x <- 1:10
y <- 5:15
intersect(x, y)
tabulate(x)
tabulate(y)

# 7
merge_string_list <- function(x, sep = "") {
    stopifnot("x ma byc lista" = (is.list(x)))
    paste(unlist(x), collapse = sep)
}
l <- list(c("Ala", "ma", "kota"), c("Jan", "ma", "psa"))
merge_string_list(l, sep = "-")

# 9
approxinvert <- function(f, y, c, d, k = 100) {
    stopifnot(
        "f nie jest funkcja" = is.function(f),
        "k calkowita > 2" = (is.numeric(k) && k == floor(k) && k >= 2),
        "c, d rzeczywiste, c < d" = (length(c) == 1 && length(d) == 1 && is.numeric(c(c, d)) && c < d),
        "y liczbowy zawarty w <f(c), f(d)>" = (is.numeric(y) && min(y) >= f(c) && max(y) <= f(d))
    )

    x_f <- seq(c, d, length.out = k)
    y_f <- f(x_f)
    a_f <- approxfun(x_f, y_f)
    a_f(y)
}
y <- 1:10
f <- function(x) x
approxinvert(f, y, 1, 10)

# 10
wystarczy <- function(w, r, R, fun) {
    stopifnot(
        is.function(fun),
        is.list(w),
        is.numeric(unlist(w)),
        is.numeric(r),
        length(r) == 1,
        is.numeric(R),
        length(R) == 1
    )
    temp_fun <- function(w) r <= fun(w) && fun(w) <= R

    take <- unlist(lapply(w, temp_fun))
    return(w[take])
}
wystarczy(list(c(6, 3, 10), c(5, 10, 15), 4), 20, 30, sum)

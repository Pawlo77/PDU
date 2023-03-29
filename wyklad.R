is.vector(5)

numeric()
integer()
double()
complex()
character()
logical()

typeof(TRUE)
typeof(1)
typeof(1L)
typeof(1.9)
typeof(7 + 9i)
typeof("a")
typeof(NA) # logical

sum(c(1:3, NA), na.rm = TRUE)
max(c(1:3, NA), na.rm = TRUE)

is.nan(log(-10))
is.finite(c(1, NA, NaN, Inf, -Inf, NULL))

"%-_-%" <- function(x, y) {
    return(x + y + 2)
}
3 %-_-% 6

rep(1:3, times = 4)
rep(1:3, each = 2)
rep(1:3, each = 2, times = 3)
rep(1:3, 1:3)

seq(0, 1, by = 0.1) # both ends inclusive
seq(0, 1, length.out = 9)

x <- 1:3
x[5] <- 5 # x[4] zostanie ustawione jako NA
x

x <- 1:10
x[x < 5] <- 0
x
x[-(1:3)] # wszystkie prócz 1:3

x <- 1:10
x[rep(c(TRUE, FALSE), each = 5)]
x[c(TRUE, FALSE)]

library(MASS) # import
anorexia

x1 <- anorexia$Postwt
x2 <- anorexia$Treat
typeof(x2)
x2 <- as.character(x2)

mean(x1[x2 == "CBT"])
anorexia[anorexia$Prewt < anorexia$Postwt, ]

l <- list(1:4, TRUE, letters, sum)
l
str(l)

is.vector(l)
is.atomic(l)
is.atomic(1:30)

l[1] # dostaniemy liste zapierajaca 1:4
l[[1]] # dostaniemy dokladnie 1:4
l[100]
l[[100]] # to juz da error
l[1] * c(10, 100) # da error
l[[1]] * c(10, 100)
l[FALSE]
l[[FALSE]] # nie mozemy wybrac zera el w ten sposob
l[[c(1, 3)]]
l[[c(3, 4)]]
str(l)

l[c(1, 3)] <- list("a", c(TRUE, FALSE, TRUE))
str(l)

l[1] <- mean # da error
l[[1]] <- mean
str(l)

l[1] <- NULL # jak del
l[[1]] <- NULL # to tez
l[1] <- list(NULL)

l[[1]] <- 1:10
l[[c(1, 3)]] <- "a"
l[[c(1, 3)]] <- NULL # to da error
str(l)

?optim

fr <- function(x) {
    x1 <- x[1]
    x2 <- x[2]
    100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}
t <- optim(c(-1.2, 1), fr)
str(t)

x <- as.list(1:10)
exp(x) # na listach sie nie da

?apply
lapply(x, exp)
exp(unlist(x))
sapply(x, typeof)

do.call("paste", list(1:4, "%", sep = "**"))

{
    cat("a")
    5
} -> r
r # wynik ostatniego wyrazenia jest zwracany

t <- print(10000) # print zostanie zawolane rowniez przy przypisaniu
t
invisible(t)

ls() # wyswietla wszystkie zmienne
rm(list = ls()) # usuwa wskazane zmienne
ls()

u <- scan(what = double())
u

potega <- function(x, y) x^y
?plot
plot(1:10, potega(1:10, 3), type = "b", ylab = "p-ta potega", xlab = "p", main = "wykres funkcji potega()")

x <- 1:3
y <- c("a", "b", "c")
paste(x) # zmiana na character()
paste(x, y) # element wise
paste(x, y, collapse = "|") # łączy element wise, potem w całość odzielając |
paste(x, y, sep = "_") # łączy element wise z sep _
paste(x, y, sep = "_", collapse = "|")

# Sprawdzenie ile elementów wektora spełnia pewien warunek
sum(x >= 0)
# Sprawdzenie jaka frakcja wektora spełnia pewien warunek
mean(x >= 0)

x <- c(1, NA, 4, 5.5)
y <- na.omit(x) # x bez obserwacji brakujących
na.action(y) # zawiera pozycje na ktorych wystąpila wartosc NA

class(iris)
is.data.frame(iris)
mode(iris)
attributes(iris)
str(iris)

# tworzenie ramek danych
df <- data.frame(
    x = rnorm(5),
    y = sample(c(TRUE, FALSE), 5, replace = TRUE),
    z = letters[1:5]
)
df
names(df)
rownames(df)
rownames(df) <- letters[1:5] # muszą byc unikatowe
unclass(df)

lista <- list(a = 1:10, b = rnorm(10))
as.data.frame(lista)

as.data.frame(simplify2array(lapply(iris[, 1:4], summary)))

# zapis / wczytywanie
write.table(df, "ramka_danych.csv") # row.names=FALSE
read.table("ramka_danych.csv")

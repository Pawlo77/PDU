a <- 1:3
b <- 1:4

rbind(a, b) # warning, jednak zapetli ktorszy
cbind(a, b) # tak samo
matrix(1:9, nrow=3) # ncol zgadnie na podstawie nrow
matrix(1:7, nrow=3) # ncol zgadnie, brakujace dopelni z warningiem
matrix(1:4, nrow=3, ncol=3) # zapetli aby zapelnic
matrix(1:9, nrow=3, byrow=TRUE) # wklada by_row nie by_col
# musza byc podane oba
a <- matrix(1:9, nrow=3, dimnames=list(c("x", "y", "z"), c("A", "B", "C"))) 
colnames(a)
rownames(a)
colnames(a) <- c("C1", "C2", "C3")

a <- 1:9
dim(a) <- c(3, 3)
class(a) # zmieni sie na matrix

a[1, 2]
a[c(1, 3), c(2, 3)] # wiersze 1, 3 kolumny 2, 3
a[c(1, 2), ] # caly 1 i 2 wiersz
a[, c(1)] # cala pierwsza kolumna
a[,] # cale a
a[-1, ] # wszystko procz pierwszego wiersza
class(a[-1, ]) # matrix 
class(a[1, ]) # vector if we take only part of one row / col
class(a[1, , drop=FALSE]) # now it is still matrix
# jednym wektorem stakujemy kolumny jedna na drugiej i bierzemy z tego
a[4:5]
a[a > 3]
# oczywiscie mozna indeksowac rownames i colnames
a[1, 1] <- 10; a
a[a < 4] <- -1; a
rowSums(a) + a[, 1, drop=FALSE] # pozostanie macierza
c(1, 2) + a[, 1, drop=FALSE] # warning, ale zapetli

t(a) # matrix transpose
a <- a[, -1] # remove first row
dim(a) <- c(1, 6)

replicate(100, {
  x <- runif(10);
  mean(x) # to zostanie zapisane do wektora wynikowego
})
a <- replicate(100, {
  x <- runif(10)
  c(mean(x), sd(x))
})
dim(a)
typeof(a)
class(a)

x <- 1:2
if (TRUE) {
  cat("Im mad")
} else if (FALSE) {
  cat("I will never go alive")
} else {
  cat("Neighter do I")
}
if (length(x) > 1)
  cat(":)") else cat("No")
if (length(x) > 1) cat(":)") else cat("No")
{
  if (length(x) > 1)
      cat(":)")
  else 
    cat("No")
}
{
  if (length(x) > 1)
    cat(":)")
  else cat("No")
}
u <- if (0.9) cat("D") # cokolwiek int nie 0 trigeruje
u # if nic nie zwrocil

for (i in 1:10) {
  y <- i^2
} -> u # ostatnie i^2
u <- for (i in 1:10) {
  y <- i^2
} # tu u bedzie juz NULL

i <- 0
repeat {
  cat(i)
  i <- i + 1
  if (i > 10) break
}

warning("Uwaga")
message("Cokolwiek co chce ci powiedziec") # bedzie na czerwono
stop("Why not") # Error
tryCatch(
  expr = rnorm("a"),
  error = function(e) {
    print(e)
    print(":(")
  },
  finally = {
    print("Udalo sie")
  }
)

x <- matrix(1:6, nrow=2)
y <- seq(10L, 60L, 10L)
sapply(list(x, y), max)

x <- list(1:4, 9:12, c("A", "B"))[c(1, 2)]
x[[c(1, 3)]]
class(x)
dim(x) <- c(1, 2)
class(x)
class(drop(x))

attributes(iris)
unclass(iris)
iris$Petal.Length
iris[1, ]
typeof(iris[1, ]) # zwroci liste
class(iris[1, ])  # ta lista pozostanie data.frame
typeof(iris[, 1]) # zwroci wektor
class(iris[, 1]) # numeryczny w tym przypadku
iris[, 1, drop=FALSE]
head(iris)
dim(iris)
iris[sample(1:nrow(iris), 20), ]
row.names(iris)[2] <- "jakis_row"
head(iris)
tail(iris, 4)
names(iris)

res <- aggregate(iris$Sepal.Length, by=iris["Species"], FUN=function(x) c(min(x), max(x), sd(x)))
str(res)
attributes(res)
res$x
res$Species

by(iris[, c("Sepal.Width")], iris["Species"], mean)

order(c(4, 2, 3, 4, 1), decreasing =TRUE)
table(rep(letters[1:3], each=3))
?sample
rep(1:2, each=3, times=2)
?cumsum
cumsum(1:10)

a <- matrix(1:9, 3)
a[rbind(c(1, 2), c(2, 3))]
colSums(a)

?by

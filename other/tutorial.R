?source # execute commands from file

a <- 10
b <- "Hello"
c <- FALSE
objects() # prints names of all objects
rm(a, c) # removes object a, c

# equivalent
a <- c(1.1, 2.2, 3.3, 4.4)
assign("c", c(1.1, 2.2, 3.3, 4.4))
d = c(1.1, 2.2, 3.3, 4.4) # not always same as assign and <-, not recomennded

x <- c(1, 2, 3, 4)
y <- c(x, 0, x)
z <- 2 * x + y + 3 # krótszy wektor x zostanie cyklicznie użyty

max(x, y, z) # traktuje jako max(c(x, y, z))
pmax(x, x+1) # traktuje jako (max(x[[0], (x+1)[0]), ...)

range(z) # zwraca wektor postaci (min(z), max(z))
length(z)
sum(z)
prod(z)
mean(z) # mean -  sum(x)/length(x)
var(z) # variance - sum((x-mean(x))^2)/(length(x)-1)

sort(z)
?order # for more advanced uses

1:30
2*1:15 # : ma większy piorytet niz dzialania itp

?seq # to more advanced 
seq(1, 5, 0.4) # od 1 do 5 co 0.4
seq(1, by=0.4, length.out=20) # dlugosc 20, step 0.4 od 1
seq(1, by=0.1, along.with=z) # dlugosc jak z, step 0.1 od 1

?rep
rep(x, times=3)
rep(x, each=2)

z = c(1:3, NA, NaN)
is.na(z)

?Quotes
a <- c("Paweł", "Dominik")
labs <- paste(a, 1:3, sep=" - id: ") # tu krotsze a rowniez jest cyklicznie

z[!is.na(z)]
(z + 1)[(!is.na(z)) & z > 1] 

c("x", "y")[rep(c(1, 2, 2, 1), times=3)] # indeksowanie od 1
c(1:10)[-(1:3)] # exclude 1:3 positions

price <- c(1:4)
names(price) <- c("a", "b", "c", "d")
price[c("c", "b")]

z[is.na(z)] <- 0 # tu może byc tez operacja np -z[idx]

a <- character(0) # empty character vector
b <- numeric(0)

mode(a) # object type
length(a) # object len

a <- 1:10
a <- as.character(a)
a <- as.integer(a)

a <- numeric()
a[3] <- 17 # now a is NA NA 17

a <- 1:10
length(a) <- 3 # keep only first 3 
a = a[1:3] # same
length(a) <- 10 # entend with NA

attr(a, "my_attr") <- c(10, 10) # custom attribute
attributes(a)

a = data.frame()
unclass(a)

state <- c("tas", "sa",  "qld", "nsw", "nsw", "nt",  "wa",  "wa",
           "qld", "vic", "nsw", "vic", "qld", "qld", "sa",  "tas",
           "sa",  "nt",  "wa",  "vic", "qld", "nsw", "nsw", "wa",
           "sa",  "act", "nsw", "vic", "vic", "act")
statef <- factor(state)
levels(statef) # vector of unique var in state
tapply(statef, statef, length) # number of each states
incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56,
             61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46,
             59, 46, 58, 43)
tapply(incomes, statef, mean) # income means

z <- 1:1500
dim(z) <- c(3, 5, 100) # now z is 3 x 5 x 100 array
# !first index is moving the fastest
z[,,0] # whitespace means take all range

z <- array(1:20, dim=c(4,5))
z
i <- array(c(1:3, 3:1), dim=c(3, 2)) # index array
i
z[i]
z[i] <- 0
z

z <- array(0, c(3, 4, 2))

df <- data.frame(player = c('AJ', 'Bob', 'Chad', 'Dan', 'Eric', 'Frank'),
                 position = c('A', 'B', 'B', 'B', 'B', 'A'),
                 points = c(1, 2, 2, 1, 0, 0))
table(df$position) # frequency table

x <- 1:10
y <- 1:10
x %o% y # outer product

?aperm

a <- 1:10
diag(a) # zwraca macierz diagonalna o glownej przekatnej a 
diag(diag(a)) # zwraca a
diag(10) # jednostkowa 10 na 10

A <- diag(a)
B <- matrix(1:100, c(10, 10))

# matrix multiplication element-wise - res[i, j] = A[i, j] * B[i, j], 
# tylko jak obie kwadratowe tego samego wymiaru
A*B 

A %*% B # mnozenie macierzy
crossprod(A, B) # to samo ale często szybsze

A <- rbind(c(1, 2, 3), c(1, 2, 4), c(1, 2, 5)) # cbind for columns

A <- matrix(1:4, c(2,2))
x <- c(1, 2)
B <- crossprod(A, x)

?solve
solve(A, B)
solve(A) # inverse of A







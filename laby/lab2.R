x <- seq(1, 3, 1)
y <- c("X", "O", "V")

rep(y, times = 3)
rep(c(x, y), length.out = 10)

paste("a", c("b", "c"), sep = "X", collapse = "-")
-2:3
seq(-1, 1, 1)
1:4
(1:4)[5]
typeof(c((1:4), NA) + (1:2))
typeof(41:13)

?cat
?lapply

l <- list(1:4, 3:4)
l[c(1, 3)]

typeof(paste(c(4, 5), c(1), sep = "X"))
sum(1, 1:3)

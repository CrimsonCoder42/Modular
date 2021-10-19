#gcdcalc.R

N <- 100
DF <- data.frame(a=numeric(N),b=numeric(N),r=numeric(N),q=numeric(N),
                 m=numeric(N),n=numeric(N))
DF

gcd.euclid <- function(x,y,details = FALSE) {
  #Make a data frame with more rows than we will ever need
  N <- 100
  DF <- data.frame(a=numeric(N),b=numeric(N),r=numeric(N),q=numeric(N),
                   m=numeric(N),n=numeric(N))
  i <- 1
  DF[i,] <- c(max(x,y),min(x,y),max(x,y)%%min(x,y),max(x,y)%/%min(x,y),0,0)
  while (DF[i,3] > 0){
    DF[i+1,1] <- DF[i,2]
    DF[i+1,2] <- DF[i,3]
    DF[i+1,4] <- DF[i,2]%/%DF[i,3]
    DF[i+1,3] <- DF[i,2]%%DF[i,3]
    i <- i+1
  }
  #In row i the remainder was 0 and the smaller number b is the gcd.
  gcd <- DF[i,2]
  #If details are not requested we have the answer.
  if(details == FALSE) return(gcd)
  j <- i-1
  #In row j the remainder r2 is the gcd and a2 = b2q2+r2.
  #So r2 = (1)(a2)-(q2)(b2)
  #Therefore m2 = 1 and n2 = -q2
  DF[j,5] <- 1
  DF[j,6] <- -DF[j,4]
  #In row j-1 a1 = b1q1+r1. The remainder r1 becomes b2 and b1 becomes a2.
  #So a1 = a2q1+b2   and b2 = a1-q1a2
  #The gcd is m2a2+n2b2=m2a2+n2a1-n2q1a2=n2a1+(m2-n2q1)a2
  #Therefore m1 = n2 and n1 = m2-n2q1
  while (j > 1){
    DF[j-1,5] <- DF[j,6]
    DF[j-1,6] <- DF[j,5]-DF[j,6]*DF[j-1,4]
    j <- j-1
  }

  m <- ifelse(x > y, DF[1,5], DF[1,6])
  n <- ifelse(x > y, DF[1,6], DF[1,5])
  stopifnot(gcd ==m*x+n*y)
  return(list(gcd=gcd,m=m,n=n,df=head(DF,i)))
}
# gcd.euclid(30,37)
# gcd.euclid(30,37,details=TRUE)$gcd
# gcd.euclid(30,37,details=TRUE)$m
# gcd.euclid(30,37,details=TRUE)$n

gcd.GCD <- function(n, m) {
  stopifnot(is.numeric(n), is.numeric(m))
  if (length(n) != 1 || floor(n) != ceiling(n) ||
      length(m) != 1 || floor(m) != ceiling(m))
    stop("Arguments 'n', 'm' must be integer scalars.")
  if (n == 0 && m == 0) return(0)
  
  n <- abs(n); m <- abs(m)
  if (m > n) {
    t <- n; n <- m; m <- t
  }
  while (m > 0) {
    t <- n
    n <- m
    m <- t %% m
  }
  return(n)
}

c=gcd.GCD(8,1);c

gcd.multiplicativeGroup <- function(n) {
  i <- n-1
  v <- c(1:i)
  x <- c()
  
  for (i in v) {
    val <- gcd.GCD(i, n)
    if(val == 1) {
      x <- append(x, i)
    }
  }
  return(x)
}
vec <- gcd.multiplicativeGroup(20);vec
vec[which.max(vec)]

gcd.orders <- function(group, power) {
  order <- c()
  for(num in group) {
    # print(num)
    exp <- 1
    n <- (num^exp)%%power
    while(n != 1) {
      n <- (num^exp)%%power
      exp <- exp + 1
    }
    order <- append(order, exp-1)
  }
  # DF <- data.frame(group, order)
  DF <- cbind(group, order)
  return(DF)
}
gcd.orders(vec, 20)

data <- data.frame(vec)
data
for(i in 1:4) {                                   # Head of for-loop
  new <- rep(i, ncol(data))                       # Create new row
  data[nrow(data) + 1, ] <- new                   # Append new row
}
data

(6)%%6

# i <- 1
# while (i != 6) {
#   print(i)
#   i = i+1
# }

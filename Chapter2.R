#Chapter 2
#apply, sapply, lapply, tapply
a <- c(2,4)
b <- c(-2.5,1.5)
-a
1+b
a+b
a-1
2*3
10%%3
2^a
(a+1)^(-2)

#   - Looping over row or columns of matrices with 'apply()':
m <- matrix(1:12, 3)
m
#     * Looping over rows::
apply(m, 1, min)   # Return a vector with minima of the rows.
#                   ^^^ the "min()" function, applied to the 1st dimension.
#     * Looping over columns:
apply(m, 2, mean)  # Return a vector with the means of the columns.
#     * Looping over columns:
apply(m, 1, mean)  # Return a vector with the means of the rows.
#     * 'apply()' can also be used with functions that return vectors, 
#       such as 'range()' and 'sort()':
apply(m, 2, range)
#     There are complications with "apply(m, 1, range)":
#     the result is 2xnrow(m), not nrow(m)x2, as one would expect.
#     This requires transposition, 't()', to get the expected matrix:
t(apply(m, 1, range))
#   - Looping over elements of a list:
lis <- list(numvec=1:10, charvec=letters,
            logicvec=rep(c(T,F),3), mat=cbind(1:3,1))
lis
# returns a list with one number per entry: the length
lapply(lis, length)           
unlist(lapply(lis, length))   # same but 'flattened' to a vector
sapply(lis, length)           # same but Simpler ('Simple apply')
lapply(lis, dim)              # returns a list with the dim attribute for each entry
lapply(lis, sort)             # returns a list of sorted vectors

sapply(lis, mean)

#Finding Pythagorian Triples
s <- 2
t <- 1
a <- s^2 - t^2
b <- 2 * s * t
c <- s^2 + t^2
a
b
c

#Generate Pythagorean triples
pythag <- function(x) {
  s <- x[1]
  t <- x[2]
  a <- t^2 - s^2
  b <- 2 * s * t
  c <- s^2 + t^2
  cat("The Pythagorean triple is: ", a,b,c,"\n")
}
input <- scan()
pythag(input)

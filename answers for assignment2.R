# another solution to 7(a)
set.seed(75)
aMat <- matrix(sample(10, size = 60, replace = T), nr = 6)
aMat > 4
tmp <- (aMat > 4)*1
tmp
rowSums(tmp)
# another solution to 7(b)
tmp <- (aMat == 7)*1
tmp <- rowSums(tmp)
tmp

# 8(c)
# you need a 10x10 denom matrix and a length = 10 numerator
denom <- outer(1:10, 1:10) + 3
denom
numer <- (1:10)^4
numer
# ok now form a 10x10 matrix of terms
tmp <- numer/denom
tmp
# now you know intuitively that you're going to sum either the upper or lower triangle
# keep it simple
sum(lower.tri(tmp), dia = TRUE)*tmp


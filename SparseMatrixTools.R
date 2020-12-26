## Declarations -----------------------------------------------------------------------------------------

A <- Matrix(1:9,nrow = 3)
B <- Matrix(1:6,nrow = 3, byrow = TRUE)

A <- Matrix(round(rnorm(12,5,2),1), nrow = 3, byrow = TRUE)
B <- Matrix(round(rnorm(6,10,3),1), nrow = 3, byrow = TRUE)

## vector with ones -------------------------------------------------------------------------------------
ones.A <- matrix(1, nrow = nrow(A), ncol = 1)
ones.B <- matrix(1, nrow = nrow(B), ncol = 1)

## row means (column vector) ----------------------------------------------------------------------------
A %*% matrix(1/ncol(A), nrow = ncol(A), ncol = 1)
B %*% matrix(1/ncol(B), nrow = ncol(B), ncol = 1)

sparse.rowMeans <- function(M){M %*% matrix(1/ncol(M), nrow = ncol(M), ncol = 1)}

## col means (column vector) ----------------------------------------------------------------------------
t(A) %*% matrix(1/nrow(A), nrow = nrow(A), ncol = 1)
t(B) %*% matrix(1/nrow(B), nrow = nrow(B), ncol = 1)

sparse.colMeans <- function(M){t(M) %*% matrix(1/nrow(M), nrow = nrow(M), ncol = 1)}

## col sums (column vector) -----------------------------------------------------------------------------
t(A) %*% matrix(1, nrow = nrow(A), ncol = 1)
t(B) %*% matrix(1, nrow = nrow(B), ncol = 1)

sparse.colSums <- function(M){t(M) %*% matrix(1, nrow = nrow(M), ncol = 1)}

## normalized (sweep out each column mean) --------------------------------------------------------------
norm.A <- t(t(A) - as.numeric(t(A) %*% matrix(1/nrow(A), nrow = nrow(A), ncol = 1)))
norm.B <- t(t(B) - as.numeric(t(B) %*% matrix(1/nrow(B), nrow = nrow(B), ncol = 1)))
norm.A
norm.B

sparse.colCenter <- function(M){t(t(M) - as.numeric(sparse.colMeans(M)))}

## colSds -----------------------------------------------------------------------------------------------
# check if colSums() is faster, if not just use code above
sd.A <- sqrt(colSums(norm.A^2)/(nrow(A)-1))
sd.B <- sqrt(colSums(norm.B^2)/(nrow(B)-1))

sparse.colSds <- function(M){sqrt(sparse.colSums(sparse.colCenter(M)^2)/(nrow(M)-1))}

## correlation of 1 column from A and B ------------------------------------------------------------------
# with regular matrices
vec.A <- A[,1] # class: numeric
vec.B <- B[,1] # class: numeric
cor(vec.A,vec.B)

# direct calculation
sum(norm.A[,1] * norm.B[,1]) / sqrt(sum(norm.A[,1]^2) * sum(norm.B[,1]^2))

# calculations using sds
sum(norm.A[,1] * norm.B[,1]) / (sd.A[1] * sd.B[1]) / sqrt((nrow(A)-1) * (nrow(B)-1)) # class: numeric
t(norm.A[,1]/sd.A[1]) %*% norm.B[,1]/sd.B[1] / sqrt((nrow(A)-1) * (nrow(B)-1)) # class: matrix

## correlation between columns of A and columns of B -----------------------------------------------------
# with regular matrices
cor(as.matrix(A),as.matrix(B))

# with sparse matrices and colSums() function
t(norm.A) %*% norm.B / 
  sqrt(t(t(colSums(norm.A^2))) %*% # colSums require this 'dual transposal' to match class
       colSums(norm.B^2))

# with sparse matrices and sum of columns calculation
t(norm.A) %*% norm.B / 
  sqrt(t(norm.A^2) %*% matrix(1, nrow = nrow(A), ncol = 1) %*% 
         t(t(norm.B^2) %*% matrix(1, nrow = nrow(B), ncol = 1)))

# definition of sparse.correlation function
sparse.correlation <- function(M,N){

  # deviations from mean
  norm.M = sparse.colCenter(M)
  norm.N = sparse.colCenter(N)
  
  # sum of squared errors
  sse.M = norm.M^2 %>% sparse.colSums
  sse.N = norm.N^2 %>% sparse.colSums
  
  # correlation matrix
  (t(norm.M) %*% norm.N) / sqrt(sse.M %*% t(sse.N))
}

## correlation between columns of A and columns of B calculated at common rows for each column ---------

# definition of sparse.correlationCommonRows function
sparse.correlationCommonRows <- function(M,N){
  
  # deviations from mean
  norm.M = sparse.colCenter(M)
  norm.N = sparse.colCenter(N)
  
  # sum of squared errors for each pair of columns from first and second matrices
  sse.M = t(norm.M^2) %*% !near(N,0) # sse of each column in M matched with non-zero entries of each column of N
  sse.N = t(norm.N^2) %*% !near(M,0) # sse of each column in N matched with non-zero entries of each column of M
  
  # correlation matrix
  (t(norm.M) %*% norm.N) / sqrt(sse.M * t(sse.N))
}















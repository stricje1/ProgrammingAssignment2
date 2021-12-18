### Jeffrey Strickland ##
## This script is comprise of two functions related to computing matrix inverses.
## The first function, makeCasheMatrix(matrix), computes the inverse of a square matrix of full rank. If
## the matrix is not invertible (i.e., singular or computationally singilar),
## makeCasheMatrix will return "Error in solve.default(data, ...) :" with a 
## corresponding reason.

## The second function, cacheSolve(matrix), retrieves a matrix from the cashe. If 
## the matrix is an inverse of a matrix computed by makeCasheMatrix, it retrieves
## the inverse matrix. Otherwise it retrieve the cashed matrix and computes its
## inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  message("matrix inverse in cache")
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## CasheSolve computes the inverse of a matrix. 
## (The matrix must be square and invertible, i.e., full rank.)
## If the inverse has already been calculated, then he cacheSolve will retrieve
## the inverse from the cache 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("retrieving cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

################################### TEST #####################################
library(mgcv)

# A full rank matix is invertible, i.e., Rank(3x3 matrix) = 3 if full
A <- matrix(c(1,2,0,0,1,0,5,0,1),3,3,3)
A
Rrank(A) # Returns the rank of an upper triangular matrix

Ai <- makeCacheMatrix(A)
cacheSolve(Ai) #inverse returned after computation

cacheSolve(Ai) #inverse returned from cache


## The inverse of the Identity matrix is itself
Identity <- matrix(c(1,0,0,0,1,0,0,0,1),3,3,3)
Identity

Id <- makeCacheMatrix(Identity)
cacheSolve(Id) #inverse cached after computation

cacheSolve(Id) #inverse returned from cache

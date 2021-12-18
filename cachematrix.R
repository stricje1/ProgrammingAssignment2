################################################################################
###                      R Programming Assignment #2                         ###
###                          Jeffrey Strickland                              ###
################################################################################
## This script is comprise of two functions related to computing matrix inverses.
## The first function, makeCasheMatrix(matrix), computes the inverse of a square 
## matrix of full rank. If the matrix is not invertible (i.e., singular or 
## computationally singilar), makeCasheMatrix will return "Error in 
## solve.default(data, ...) :" with a corresponding reason.

## The second function, cacheSolve(matrix), retrieves a matrix from the cashe. If 
## the matrix is an inverse of a matrix computed by makeCasheMatrix, it retrieves
## the inverse matrix. Otherwise it retrieve the cashed matrix and computes its
## inverse.

## This function computes a matrix inverse and pushes it to cache
## Comprised of sub-functions that computate inverse and caches

makeCacheMatrix <- function(X = matrix()) {
  i <- NULL
  set <- function(y) {  # defines elements of the sub-function
    X <<- y
    i <<- NULL
  }
  get <- function() X # returns matrix X as argument to the function
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  message("matrix inverse in cache")
  list(get = get,
       set = set,
       getinverse = getinverse,
       setinverse = setinverse)
}


## CasheSolve computes the inverse of a matrix. 
## If the inverse has already been calculated, then he cacheSolve will retrieve
## the inverse from the cache. 

cacheSolve <- function(X, ...) {
  # X is a square matrix & should have full rank
  i <- X$getinverse()
  if (!is.null(i)) {
    message("retrieving cached data")
    return(i)
  }
  data <- X$get() # gets matrix elements for solve to invert
  i <- solve(data, ...) # computes the matrix inverse
  X$setinverse(i)
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

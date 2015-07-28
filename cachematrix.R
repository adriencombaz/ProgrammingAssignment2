rm(list = ls())
library(MASS)

## ASSIGNMENT 2: CACHING THE INVERSE OF A MATRIX

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y){
    x   <<- y
    iv  <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) iv <<- inverse
  getinverse <- function() iv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  

}


## This second function caclulates the inverse of the special "matrix" create with th above function
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value 
# of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iv <- x$getinverse()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  inv <- ginv(data, ...)
  x$setinverse(inv)
  inv
}

## Testing
n = 1000
mat <- matrix( rnorm(n*n,mean=0,sd=1), n, n) 
tt <- ginv(mat)

mat2 <- makeCacheMatrix(mat)
tt2 <- cacheSolve(mat2)
tt3 <- cacheSolve(mat2)

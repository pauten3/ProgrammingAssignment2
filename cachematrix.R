## Put comments here that give an overall description of what your
## functions do

## we are going to create a pair of functions
## the first is going to create a special "matrix" that can then 
## cache its inverse

## the second computes the inverse of the matrix created in the first 
## function

## Write a short comment describing this function
## this is the function that creates the matrix, and can store
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get the matrix
  get <- function() x
  ## set the inverse
  setInverse <- function(inverse) inv <<- inverse
  ## get the inverse
  getInverse <- function() inv
  list( set = set,
        get = get,
        setInverse = setInvers,
        getInverse = getInverse)

}


## Write a short comment describing this function
## this is the function that can calculate the inverse of the matrix
## and can then store it back in the matrix

## so now we are going to get the inverse of the matrix
## we will also see if the inverse has already been found, and if so
## pull it from the matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## check to see if the inverse is already there
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("retriving cached data")
    return(inv)
  }
  ## calculate the inverse of the matrix
  ## then store it back into the matrix
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
}

## Matrix Inversion - Week 3 R Programming Assignment 
## I will use makeCacheMatrix & cacheinverse & solve

## makeCacheMatrix - creates a special “matrix”, which is really a list containing a function to:
# - set the elements of the matrix
# - get the elements of the matrix
# - set the elements of the matrix inverse
# - get the elements of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheinverse - calculates the inverse of the special “matrix” created with the above function


cacheinverse <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}

## Return a matrix that is the inverse of 'x'

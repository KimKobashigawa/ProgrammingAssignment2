## makeCacheMatrix() and cachesolve() will create and access the value
## for an inverse matrix.

## makeCacheMatrix() creates a special "vector"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## sets the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL ## NULL is an empty placeholder for the inverse matrix
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  ##list formats the objects
}


## cacheSolve() checks for the cached matrix 
## and returns the matrix that is the inverse of 'x'
## if there is no cached data, the inverse will be calculated and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() 
  ## searches for cached data and returns it
  if(!is.null(m)) { 
    message("getting chached data")
    return(m)
  }
  ## if there is no cached data the below code calculates the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

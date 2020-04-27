## These combined functions of makeCacheMatrix and cacheSolve cache 
## a matrix of the users making and store the inverse of that matrix
## for future use

## makeCacheMatrix will cache the matrix of user's choice (for example, 
## matrix(1:4, nrow = 2, ncol = 2)) and prepare the object to store the
## matrix inverse once it is solved

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve will solve for the inverse of a matrix created by
## makeCacheMatrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

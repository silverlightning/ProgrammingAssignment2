## Takes or makes a matrix and allows for solved data (inverse of a 
## matrix) to be retrieved if available, otherwise to be calculated
## and cached.


## makeCacheMatrix takes data from given matrix or makes it into 
## a matrix if none is given, and adds another field to allow
## for the inverse to be cached


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cachSolve will replace NULL with solved data if this field in 
## makeCacheMatrix is empty, otherwise it will say "getting cached
## data and provides the cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

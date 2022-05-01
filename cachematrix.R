## makeCacheMatrix allows for caching the inverse of a matrix
## cacheSolve returns the inverse of the matrix, whether or not it has been cached. Indicates whether this is a cached matrix or a newly calculated matrix

## Creates a list with functions to set/get matrix X and set/get inverse of matrix X

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Checks if inverse of matrix has been calculated before and returns cached result m if it has. 
## If not, returns inverse of matrix

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  
  }
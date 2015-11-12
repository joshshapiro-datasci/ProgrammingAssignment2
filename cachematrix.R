## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}

main <- function() {
  datam <- matrix(rnorm(9), 3, 3)
  dataCM <- makeCacheMatrix(datam)
  for (i in 1:3) {
    cacheSolve(dataCM)
  }
}

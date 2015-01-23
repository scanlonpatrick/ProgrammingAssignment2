## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# takes a matrix and returns a list of functions that allow access to the matrix and its cached inverse
makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  getInverse <- function() m
  setInverse <- function(inverse) m <<- inverse
  list(
    get = get,
    set = set,
    getInverse = getInverse,
    setInverse = setInverse
    )
}


## Write a short comment describing this function
# takes a makeCacheMatrix, calculates inverse, then caches it
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if (!is.null(m)){
    message('cached inverse exists, returning it rather than recalculating')
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setInverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}

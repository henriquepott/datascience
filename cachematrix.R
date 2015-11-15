#Below are two functions that are used to create a special object that stores a Matrix and caches its inverse.
#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
#This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and 
#the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
## Sample run:
##samp <- sample(1:80, 4, replace=F)
##x = matrix(samp, ncol=2, byrow=TRUE)
##m = makeCacheMatrix(x)
##m$get()
##[,1] [,2]
##[1,]   62   41
##[2,]   47   29
## No cache in the first run
##cacheSolve(m)
##[,1]       [,2]
##[1,] -0.2248062  0.3178295
##[2,]  0.3643411 -0.4806202
## Retrieving from the cache in the second run
##cacheSolve(m)
##getting cached data
##[,1]       [,2]
##[1,] -0.2248062  0.3178295
##[2,]  0.3643411 -0.4806202

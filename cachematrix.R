# This assignment illustrates two functions to cache the inverse of a matrix because there may be some benefit to caching the inverse of a matrix instead of computing it over and over.  
# 
# The first is the makeCacheMatrix.  It is a function that creates a list to 
# 1.	Set the value of the matrix (changes the vector stored in the main function)
# 2.	Get the value of the matrix (returns the vector x stored in the main function)
# 3.	Set the value of inverse of the matrix  (stores the value of the input in a variable m)
# 4.	Get the value of inverse of the matrix (returns the value of the input in a variable m)
# The second is the CacheSolve which computes the inverse of the special matrix.  If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache. 
# 
# 
# The makeCacheMatrix is a special "matrix" object that can cache its inverse.

makeCacheMatrix <-function (x =  matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse =  getinverse)
}


# This function determines if the inverse is computed.  If there is an inverse in cache, 
# then it skips the computations and retrieves the data.  Otherwise, it will compute it and store it in cache.

cacheSolve  <- function(x, ...)  {
  # Return a matrix that is the inverse of "x"
  m <- x$getinverse()
  if(!is.null(m)) {
    message ("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

@@ -1,15 +1,47 @@
## These functions allow one to create a matrix and store the inverse of the matrix in a cache to reduce computation time

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  
  m <- NULL
  
  set <- function(y) {
    x <<- y                 ##This sets the value
    m <<- NULL              ##This clears the cache and sets it to NULL
  }
  
  get <- function() x       ##This defines the function which gets the matrix
  setinverse <- function(inverse) m <<- inverse   ##This defines the function which sets the inverse
  getinverse <- function() m                      ##This defines the function which gets the inverse
  list(set = set, get = get,          ##This returns a list with the functions set, get, setinverse and getinverse
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m<- x$getinverse()    ##Fetch the cached value for the inverse of the matrix 
  if(!is.null(m)) {     ##This tests if the cache is empty.  If it wasn't, the cached value is returned.
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()    ##This gets the value of the matrix (we now need to calculate the inverse as the cache is empty)
  m <- solve(data)   ##Solve calculates the inverse of the matrix
  x$setInverse(m)    ##This caches the result of the solve function
  m                 ##This returns the inverse
}

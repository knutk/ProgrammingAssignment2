## These two functions makes it possible to cache values of inverted matrices.


## makeCacheMatrix makes a "special" matrix that is to be passed to the cacheSolve() function. 
# It returns a list with functions for setting and getting the values of the special matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve is the function to be called on the cacheMatrix object when wishing to invert the matrix. 
# It checks if the matrix is already inverted, and if so returns the cache value and if not 
# it calls the solve() function and returns the value

cacheSolve <- function(x) {
  inv <- x$getInv()
  
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}


## Test
x <- matrix(c(1, 4, 7, 5), 2, 2)
ll <- makeCacheMatrix(x)
cacheSolve(ll)

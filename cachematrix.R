
## build invertible matrix frame

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(invMatrix) inv <<- invMatrix
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## solve for inverse of matrix if not already solved

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  
  if (!is.null(inv)){
    message("Returning cached data")
    return(inv)  
  }

  
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
  
}

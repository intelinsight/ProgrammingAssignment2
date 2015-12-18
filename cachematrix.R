## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  if (!is.matrix(x)) stop("Please pass a valid inversable matrix")
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  isIdentical <- function(y) {
    if (identical(x,y)) TRUE
    else FALSE
  }
  
  get <- function() x
  
  setInv <- function(i) inv <<- i
  
  getInv <- function() inv
  
  list(set = set, get = get, setInv = setInv, getInv = getInv, isIdentical = isIdentical)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv) && x$isIdentical(x)) {
    message("Getting cached inverse")
    return (inv)
  }
  
  data <- x$get()
  inv <- solve(x,...)
  x$setInv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}


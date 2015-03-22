## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize local variable
  inv <- NULL
  
  ##1 of 4 function definition. Sets the cached matrix value
  set <- function(y) {
    x <<- y ##  assign value to a variable in the parent environment.
    inv  <<- NULL
  }
  ##2 of 4 function definition; returns the matrix
  get <- function() x
  
  ##3 of 4 function definition. sets the cached inverse
  setinv <- function(inverse) inv <<- inverse
  
  ##4 of 4 function definition, returns the cached inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## sets the local variable to the inverse
  inv <- x$getinv() 
  
  ## if inv is not null, it will get cached data
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## if inv is null, the function will obtain the matrix from the makeCacheMatrix object and assign it to data
  data <- x$get()
  
  ## get the inverse
  inv <- solve(data, ...)
  
  ## set the inverse of matrix
  x$setinv(inv)
  
  ## returns the inverse
  inv
}

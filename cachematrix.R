## Programming Assignment 2
  ## objective: write a function that will solve and cache the matrices inverse 

## makeCacheMatrix - builds matrix needed to cache the inverse of the matrix of interest.
  ## creates empty matrix
  ## create mechanism to store solved matrix in empty matrix

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) Inv <<- inverse
  getInv <- function() Inv
  list(set = set, 
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## CacheSolve - calculates the inverse of the matrix. 
  ## first the function checks to see if "Inv" has has already been calculated
      # if yes provides message and returns the inverse of x, Inv
      # if NUll x is solved to produce Inv and then Inv is placed in cache matrix. 

cacheSolve <- function(x, ...) {
    Inv <- x$getInv()
    if(!is.null(Inv)) {
      message("getting cached data")
      return(Inv)
    }
    data <- x$get()
    Inv <- solve(data, ...)
    x$setInv(Inv)
    Inv
       
}

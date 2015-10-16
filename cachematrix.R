## Function makeCacheMatrix

## The objective of this function is to cache a matrix that is passed as argument
## it returns a list of Functions to manipulate the matrix and it`s inverse (getters, setters)
## when invoked it initializes the i matrix (inverse) to null and caches it.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse = matrix()) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheSolve

##This function calculates the inverse of the matrix in the variable assigned
##to the makeCacheMatrix Function, if the inverse has already been calculated
##gets the values from the variable passed as argument.
## In case the inverse is null, it calculates it and set it in the variable


cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i  
}

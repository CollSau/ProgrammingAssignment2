## These two functions create a cache of the inverse of a square (inversible) matrix
##

## The function makeCacheMatrix() takes a matrix (x) as its only argument where x is 
## the matrix to be inverted.  This function can set and get the value of the matrix,
## as well as set and get the inverse of the matrix and stores these values in a list

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve() is used to return the inverse of a matrix
## It checks to see if the inverse of the matrix has already been solved and cached, and if
## so returns the inverse.  If not, it solves and sets the value of setinverse in 
## the previous function and returns the inverse

cacheSolve <- function(x, ...){
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...) 
  x$setinverse(i)
  i
}

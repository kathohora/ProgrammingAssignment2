## This set of functions are created to find the inverse of a Matrix 
## since finding the inverse of a Matrix can be a length process, if it has already 
##been calculated in the environment, the function will pull the inverted matrix from the Cache

## This first matrix creates a list that 
##1.  sets the value of the vector
##2.  gets the value of the vector
##3.  sets the value of the inverted martix
##4.  get the value of inverted Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get<- function() x
  setinverse <-  function(solve) m<<-solve
  getinverse<- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## The 'cacheSolve' function takes the list created from 'makeCacheMatrix' 
##and returns the matrix that is the inverse of x 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


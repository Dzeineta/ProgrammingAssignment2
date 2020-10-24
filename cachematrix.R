## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
## Set the value of the matrix
  set <- function(y){
    x <<- y
    i <<- NULL
  }
## Get the value of the matrix
  get <- function()x
## Set the value of the inverse
  setinverse <- function(inverse) i <<- inverse
## Get the value of the inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)

}


## Write a short comment describing this function
## This function computes inverse of the special matrix returned by the function makeCacheMatrix above
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
## If the inverse of given matrix has been cached and the matrix has not changed
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
## Else compute inverse and cache it into special object and return the inverse
  data <-x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## The functions in this R script cache the inverse 
#of a given matrix to help reduce computation costs.

## The makeCacheMatrix function creates a special object "matrix" which 
#can cache its inverse. It does this in this order:
#1. Sets the value of matrix to 'set'
#2. Gets the value of matrix to 'get'
#3. Sets the value of inverse to 'setinv'
#4. Finally, gets value of inverse in 'getinv'

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL             #Create NULL object
  set <- function(y) { 
    x <<- y                   #Set the matrix 'x'
    inverse <<- NULL          
  }
  get <- function() x         #Get value of 'x'
  setinv <- function(solve) inverse <<- solve   #Cache Value of inverse
  getinv <- function() inverse                  #Get cached inverse value
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The cacheSolve function computes the inverse of the 
#special matrix returned by the function above (i.e. makeCacheMatrix)
#It does this by following these steps:
#1. Check if inverse has already been calculated
#2. If calculated, get inverse from cache
#3. If not, calculate the inverse & 
#use set_inverse function to set the value in its cache

cacheSolve <- function(x, ...) { 
  inverse <- x$getinv() 
  if(!is.null(inverse)) {         #Check if inverse already calculated
    message("getting cached data") 
    return(inverse)
  }
  data <- x$get()                 #Get matrix
  inverse <- solve(data, ...)     #Solve inverse
  x$setinv(inverse)               #Cache inverse
  inverse 
}

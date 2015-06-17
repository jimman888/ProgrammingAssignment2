## Put comments here that give an overall description of what your
## functions do
## The functions will create a matrix, get the inverse of a matrix for the
## 1st time and cache the inverse matrix. It will return the cached inversse 
## matrix instead of recalculating the inverse when the inverse operation 
## is called later
## 

## Write a short comment describing this function
## creates a special "matrix" object that can cache its inverse.
## The "matrix" object is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, 
         setinv=setinv,
         getinv=getinv)   
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached inverse matrix")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

## Put comments here that give an overall description of what your
## functions do

## The following functions are written in the fulfilment of caching the inverse
## of a matrix rather than computing it repeatedly since matrix inversion if 
## generally a costly computation.

## Write a short comment describing this function

## The below function creates a special Matrix object that has the ability to
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Write a short comment describing this function

## The below function computes the inverse of the special Matrix returned by the 
## makeCacheMatrix function defined above. 
## If the inverse has already been calculated then the cachesolve function 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

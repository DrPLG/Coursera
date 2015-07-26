# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.



makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix creates "special matrix" (in fact a list)  containing a function to
  ## 1. set the value of the matrix
  ## 2. get the value of the matrix
  ## 3. set the value of inverse of the matrix
  ## 4. get the value of inverse of the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## The following function returns the inverse of the special matrix.
  ## It first checks if the inverse has already been computed. 
  ## If so, it gets the result and skips the computation. 
  ## Otherwise, it calculates the inverse and sets its value in the cache via the
  ## setinverse function.
  
  ## It is here assumed that the matrix is invertible.
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


## To test if this works :

## Creation of the matrix
## > x = rbind(c(5, 3), c(3, 5))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]    5    3
## [2,]    3    5

## Geting the inverse matrix
## First time : no cache
## > cacheSolve(m)
## [,1]    [,2]
## [1,]  0.3125 -0.1875
## [2,] -0.1875  0.3125
## Second time : getting cached data
## > cacheSolve(m)
## getting cached data.
## [,1]    [,2]
## [1,]  0.3125 -0.1875
## [2,] -0.1875  0.3125
 




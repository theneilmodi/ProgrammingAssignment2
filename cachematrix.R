#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
#rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
#Your assignment is to write a pair of functions that cache the inverse of a matrix.

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL   #starts out as null
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  #internal get
  get <- function() x
  
  #set function
  setinverse <- function(m_inverse) inverse <<- m_inverse
  
  #get function
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  inverse<- x$getinverse()
  
  #checks if inverse is already cached. If so, returns it
  if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
  }
  #otherwise...
  
  data <- x$get()
  inverse <- solve(data) #solves for inverse
  x$setinverse(inverse)
  inverse
}

## This script have a pair of functions that cache the inverse of a matrix.

#This function creates a special "matrix" object that can cache its inverse., 
#which is really  a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  #Inverse initialization
  inv <- NULL
  
  #Get and Set for the matrix value
  set <- function(y) {
    #Setting matrix value
    x <<- y
    #Inverse re-initialization
    inv <<- NULL
  }
  get <- function() x
  
  #Get and Set
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  #Return the list to encapsulates the 'special' matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#above. If the inverse has already been calculated(and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  #check if the inverse is cached and return it
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #Get the 'special' matrix value
  data <- x$get()
  
  #Compute the inverse matrix and cache it
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  #Return the inverse matrix
  inv
}

#Function to test makeCacheMatrix and cacheSolve
testCacheMatrix <- function() {
  # 1-4 square matrix
  m <- matrix(1:4, ncol = 2)
  # special matrix initialization
  cacheM <- makeCacheMatrix(m)
  
  print(cacheM);
  
  #compute inverse Matrix
  inv <- cacheSolve(cacheM)
  inv <- cacheSolve(cacheM)
  
  inv
}


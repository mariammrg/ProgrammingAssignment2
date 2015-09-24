##Programming assignment 2##

#makeCacheMatrix: This function creates a special "matrix" object that can cache
#its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #The inverse matrix is established as NULL until it will be estimated
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  #function to get the actual matrix value
  get <- function() x
  
  #fuction to set the inverse value of the matrix
  setinverse <- function(i) inverse <<- i 
  
  #function to get the inverse value of the matrix
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  
  #if the result is diffferent that the null value, the message "getting cache 
  #inverse" will appear
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  
  #The inverse matrix is calculated, it is shown and finally the result is kept
  #in the cache
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
}

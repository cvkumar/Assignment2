#This function creates a special "matrix" object that can cache its inverse.
#x ~ the given matrix
#inverse ~ the inverted matrix of x
makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL #initially inverse is null
      
      #assuming that we are given an invertible matrix, we set 'x' to be that matrix
      set <- function(matrix) {
            x <<- matrix
            inverse <<- NULL
      }
      get <- function() x #retrieve the matrix if asked for it
      
      setinverse <- function(inv) inverse <<- inv #set the inverse matrix if prompted by user
      getinverse <- function() inverse #return the inverse
      #create and return a list containing the above functions
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      #check if the inverse has already been calculated, if so then return it and let the user see it
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      #if it hasn't, then calculate the inverse, set it, and then return it
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}

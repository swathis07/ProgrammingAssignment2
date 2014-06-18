
## The function makeCacheMatrix creates a special matrix and caches its inverse.
## It allows setting the values in the matrix using set() function,
## returning the matrix using get(), setting the inverse of the matrix using setinv(),
## and returning the inverse of the matrix using getinv().


makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      set <- function(y) {            ## Setting values of matrix
          x <<- y
          inv<<-NULL
      }
      
      get <- function() x             ## Returning the values in the matrix
      
      setinv <- function(inverse){    ## Setting the inverse passed as an argument
          inv <<- inverse
      }  
      
      getinv <- function() inv        ## Returning the inverse matrix
      
      list(set = set, get = get, setinv = setinv,
           getinv = getinv)           ## Returning list of above functions
}


## The function cacheSolve calculates the inverse of a matrix.
## cacheSolve takes in the matrix created in makeCacheMatrix as its argument.
## It checks if inv already exists, if so it returns the cached inverse.
## Otherwise it calculates it and returns it.

cacheSolve <- function(x, ...) {
        
        inv<-x$getinv()
        if(!is.null(inv)) {           ## Checking if inverse is cached
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## the functions optimize the inverse matrix calculation when the result is used
## more one time 

##  makeCacheMatrix creates a special "Matrix", which is in fact a list 
## containing 4 functions to set/get the value of the matrix and set/get
## the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
       ## Initialize variable
       m <- NULL
  
       ## Internal functions to help to define the cache
       ### set the value of the matrix
       set <- function(y) {
         x <<- y
         m <<- NULL
       }
       ### get the value of the matrix
       get <- function() x
  
       ### set the inverse of the matrix
       setSolve <- function(solve) m <<- solve
       ### get the inverse of the matrix
       getSolve <- function() m
  
       ##define the return value
       list(set = set, get = get,
            setSolve = setSolve,
            getSolve = getSolve)
}


##  cacheSolve calculates the inverse of the special "Matrix" created
##with makeCacheMatrix. 
###
cacheSolve <- function(x, ...) {
      ## get the potential value, if the Solve has already calculated
      m <- x$getSolve()
  
      if(is.null(m)) {
        ## without the cache, calculate the inverse of 'x' 
        data <- x$get()
        m <- solve(data, ...)
      
        ## set the calculation result
        x$setSolve(m)
      }
  
    ## return the inverse
    return(m)
}

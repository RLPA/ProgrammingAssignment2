## Function for very long vectors, as it looks up in cache reducing time with long recomputinng. 

## This function includes functions used by cacheSolve to get or set inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <-function(A){
              x <<- A
              inv <- NULL
            }
            get <- function() x
            setinverse <- function(inverse) inv <<- inverse
            getinverse <- function() inv
            list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## This function calculates the inverse of a matrix in makeCashMatrix, if it exists in cash. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinverse()
          if(!is.null(inv)){
            message("getting cached data")
            return(inv)
          }
          mtrxdata <- x$get
          inv <- solve(mtrxdata,...)
          x$setinverse(inv)
          inv
}
ProgrammingAssignment2

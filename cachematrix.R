## The "makeCacheMatrix" function below computes the inverse of a matrix (x) where I changed "m" 
## to "inv" set to "NULL" and changed "mean" to "solve" as referenced from the "example" code. 



makeCacheMatrix <- function(x = matrix()) { ## This function creates a matrix that can cache its inverse.
        inv <- NULL              ## Set "inv" as NULL to hold value of matrix inverse.
        set <- function(y) {     ## Define the "set" function to a new valued matrix in parent environment.
               x <<- y           ## Use "<<-" (superassign) to enclose the new environment.
               inv <<- NULL      ## If there is a new matrix inverse, reset "inv" again to NULL.
  }
        get <- function() {x}            ## Define the get fucntion - returns value of the matrix argument.
        setinverse <- function(inverse) {inv <<-inverse}  ## Assign value of inv in parent environment
        getinverse <- function() {inv}   ## Get the value of inv where called.
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
                                 ##You need this in order to refer to the functions with the $ operator.
}


## The "cacheSolve" function below computes the inverse of the matrix "makeCacheMatrix" (above) and returns the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
              message("getting cached data")
             return(inv)
  }
       mat <- x$get()
       inv <- solve(mat, ...)
       x$setinverse(inv)
       inv
}


## Put comments here that give an overall description of what your
## functions do

## Acts as a class for our matrix. Stores all the data and the
## relevant methods to be used in the cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        
        # Initialization Stuff
        x_i <- NULL
        set <- function(y){
                x <<- y
                x_i <<- NULL
        }
        
        # Returns the matrix
        get <- function() x
        
        # Caches the value of the provided inverse (NOTE: DOES NOT COMPUTE IT!)
        setinv <- function(inverse) x_i <<- inverse
        
        # Returns the value of the inverse
        getinv <- function() x_i
        
        # Returns the list of functions to be used as methods
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## Takes an object made by "makeCacheMatrix" to find the
## inverse of it. If the inverse is already cached, it returns
## the cached value directly.

cacheSolve <- function(x, ...) {
        
        # Looks if the inverse is stored in cache
        x_i <- x$getinv()
        
        # If it finds a cached copy of the inverse, it directly
        # returns it.
        if (!is.null(x_i)){
                message("Acquired through cached data")
                return(x_i)
        }
        
        # If the inverse isn't stored in memory, it 
        # calculates and returns the inverse.
        data <- x$get()
        x_i <- solve(data, ...)
        x$setinv(x_i)
        
        x_i
}

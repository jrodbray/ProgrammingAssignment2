##      this function caches the (potentially) expensive operation of
## finding the inverse of a matrix.  This is accomplished by creating
## a list of the functions used to set/get the matrix and set/get the
## solved inverse of that matrix.  These functions will be used by the
## following function "cacheSolve" to cache and retrieve matrix inverses.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function (inverse) i <<- inverse
        getinverse <- function() i
                
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##      The following function calculates the inverse of a matrix using 
## the above function. It first checks to see if the inverse  has already 
## been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets it into the 
## the cache via the setinverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if( !is.null(i) ) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}

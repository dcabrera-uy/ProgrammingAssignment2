## Programming Assignment II - 5/9/2020 - DCabrera

## Returns list of functions for getters and setters
makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Returns the inverse of matrix, cached in makeCacheMatrix if exist
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...) #using solve to find inverse of matrix
    x$setinverse(i)
    i        
    
    
}




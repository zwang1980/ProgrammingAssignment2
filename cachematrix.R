## The functions Caches the Inverse of a Matrix

## This function creates a list to 1, set the value of the matrix, 2, get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL                             
    set <- function(y) {                    
        x <<- y                             
        inv <<- NULL                        
    }
    get <- function() x                     
    
    setinverse <- function(inverse) inv <<- inverse  
    getinverse <- function() inv                     
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}

## This function chaches the calculated invers of matrix
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
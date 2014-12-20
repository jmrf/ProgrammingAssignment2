## Creator function: creates a special matrix object capable of 
## caching the inverse of 'x'.
## A call to the 'set' method resets the inverse, 
## so it will be recomputed when calling to 'cacheSolve'

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(m){
        x <<- m
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Function that computes the inverse of a given matrix if not computed
## previously. Otherwise return a cached inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    # if the inverse is already computed...
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    # otherwise... 
    # (assuming always invertible matrices)
    mat <- x$get()
    inv <- solve(mat)
    x$setinverse(inv)
    inv
}

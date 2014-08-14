## Put comments here that give an overall description of what your
## functions do

## makeCacheMarix
## input: an invertible matrix
## output: a list of four functions "set", "get",
## "setinverse" and  "getinverse" corresponding to x
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
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


## cacheSolve
## input: a list of four functions created by makeCacheMatrix
## output: inverse matrix corresponding to the input list
##              returns cached inverse matrix when it is available
##              returns inverse matrix derived by solve() when cached value is not available
##                      and sets inverse matrix to cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

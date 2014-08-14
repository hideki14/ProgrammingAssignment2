## Inverse matrix calculation is often prohibitively expensive. To aviod recalcuation, 
## we use cache variables that store imput matrix and the inverse. 
## The function makeCacheMatrix(m) stores matrix m in cache and returns
## functions "get" , "set", "setinverse" and "getinverse" attatched to m.
## Function "get()" returns the matrix (m).
## Function "set(a)" sets matrix a to cache.
## Function "setinverse(inverse.matrix)" sets inverse.matrix to cache.
## Function "getinverse()" returns inverse matrix.
## The function cacheSolve(p) returns the inverse matrix for m where p is 
## obtained by p <- makeCacheMatrix(m). The function cacheSolve(p) returns cached
## inverse matrix whenever available. It returns inverse matrix obtained with solve()
## when cached invers is not available. It also sets inverse to cache.

## example:
## create an invertible matrix
## m <- matrix(c(1,0,1,0,2,0,1,0,3), nrow = 3, ncol = 3)

## store m in the cache and create four functions.
## p <- makeCacheMatrix(m)

## calculate inverse of m and return it
## set it to cache for future use
## cacheSolve(p)

## returns cached inverse
## cacheSolve(p)

## name: makeCacheMarix
## input: an invertible matrix m
## output: a list of four functions "set", "get",
## "setinverse" and  "getinverse" attatched to the imput matrix m
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


## name: cacheSolve
## input: a list of four functions created by makeCacheMatrix which corresponds to a matrix
## output: inverse matrix corresponding to the list of four functions attatche to input matrix
##              i) returns cached inverse matrix when available
##              ii) returns inverse matrix derived by solve() when cached inverse is not available
##                      and sets the inverse to cache
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

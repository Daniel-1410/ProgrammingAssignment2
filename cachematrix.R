##In order to simplify some calculations solving a matrix, the following 
##functions cache the inverse of a matrix.

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(aux) inv <<- aux
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Check if the inverse matrix "exists" already. Otherwise 
##calculates it and set in the cache 

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    matr <- x$get()
    inv <- solve(matr)
    x$setinv(inv)
    inv
}

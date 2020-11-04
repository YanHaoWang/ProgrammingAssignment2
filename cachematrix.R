
## Function creates a special matrix object that can cache its inverse

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() {x}
        setInv <- function(inverse) {inv <<- inverse}
        getInv <- function() {
                inver <- ginv(x)
                inver%*%x
        } 
              
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Function computes the inverse of the special matrix returned by "makeCacheMatrix"
## If inverse has already been calculated, function retrieves inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)){
                message("retrieve inverse from the cache")
                return(inv)
        }

        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}

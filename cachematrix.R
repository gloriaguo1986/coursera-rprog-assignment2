## Caching the Inverse of A Matrix

## First function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function (x=matrix()) {
        inv() <- NULL
        set <- function(y){
                x<<-y
                inv<<-NULL
        }
        get <-function()x
        setinverse<- function(inverse) inv <<-inverse
        getinverse<- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Second function compute the inverse of the special "matrix" created by the function above

cacheSolve <-function (x,...) {
        inv <-x$getinverse()
        if (!is.null(inv)){
                message ("getting cached data")
                return (inv)
        }
        data <-x$get()
        inv <-solve (data(),...)
        x$setinverse(inv)
        inv
}

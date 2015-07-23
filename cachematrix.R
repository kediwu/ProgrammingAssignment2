## Store the inverse matrix in the cache and use it

## The function create a special vector composed of four functions

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y){
      x <<- y
      inver <- NULL
    }
    get <- function() x
    setinverse <- function(inv) inver <<- inv
    getinverse <- function() inver
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function will first check if the inversed matrix is stored in cache
## If yes, return the inverse matrix in cache
## If no, calculate the inverse matrix, store it in the cache, and then return

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinverse()
        if(!is.null(inver)){
            message("getting cached inverse matrix")
            return(inver)
        }
        matr <- x$get()
        inver <- solve(matr, ...)
        x$setinverse(inver)
        inver
}

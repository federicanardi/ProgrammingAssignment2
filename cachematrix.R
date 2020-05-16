## the functions cache the inverse of a matrix

##the function below stores a matrix and caches its inverse


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){ 
                x <<- y
                inv <<- NULL
        }
        
        get <- function() {x}
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function() {inv}
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##the function below calculates the inverse of the matrix that was generated with the above functions. 
##If the inverse has been already generated it would retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mymatrix <- x$get()
        inv <- solve(mymatrix, ...)
        x$setinverse(inv)
        inv
}

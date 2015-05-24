makeCacheMatrix <- function(x = matrix()) {
        ##create a matrix object that can cache its inverse
        inv <- NULL
        set <- function (y) {
                ##assign internal value and reset the inverse
                x <<- y
                inv <<- NULL
        }
        get <- function() x ## return internal value of matrix
        setinverse <- function(inverse) inv <<- inverse  ##assign internal value of inverse
        getinverse <- function() inv ## return value of internal inverse
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}

cacheSolve <- function(x,...) {
        ##return the inverse of the matrix x
        inv <- x$getinverse()
        if(!is.null(inv)) { ##if the cached value exists, return that
                message("getting cached inverse")
                return (inv)
        }
        ##if cached value doesn't exist, compute inverse,cache it, return it
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}


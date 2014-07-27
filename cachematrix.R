## makeCacheMatrix creates a matrix and can cache its inverse with solve() function.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(b) {
                a <<- b
                s <<- NULL
        }
        get <- function() a
        setinv <- function(solve) s <<- solve
        getinv <- function() s
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve gives the cache value for the inverse matrix, or calculates it when there's none.

cacheSolve <- function(x, ...) {
        s <- a$getinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- a$get()
        s <- solve(data, ...)
        a$setinv(s)
        s
}

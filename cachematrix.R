## Put comments here that give an overall description of what your
## functions do

## This makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## If there is no cached inverse available, we will need to
    ## calculate matrix inverse by solve().
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}


## try it out
set.seed(333)
mat1 <- matrix(round(rnorm(9),2), 3,3)

vv <- makeCacheMatrix(mat1)
## First time, compute and set the inverse.
cacheSolve(vv)
## second time, it should print out the "getting cached data" message
cacheSolve(vv)

## check the answer
round(mat1 %*% cacheSolve(vv), 5)

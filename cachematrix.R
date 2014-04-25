## HANA | 25.04.2014
## Programming Assignment 2
## R Programming | COURSERA

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    ## check if catched inverse of 'x' exits
    if(!is.null(m)) {
        message("getting cached inverse of matrix")
        ## Return a cached matrix that is the inverse of 'x'
        return(m)
    }
    data <- x$get()
    ## calculate inverse of CacheMatrix 'x'  as it does not exist yet
    m <- solve(data, ...)
    ## cache just calculated inverse of matrix
    x$setsolve(m)
    ## Return just calculated matrix that is the inverse of 'x'
    m    
}



makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
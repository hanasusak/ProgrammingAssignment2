## HANA | 25.04.2014
## Programming Assignment 2
## R Programming | COURSERA

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## element set for CacheMatrix, with NULL inverse.
    ## function to define matrix itself and inverse. 
    set <- function(y) {
        ## with <<- we access variable names from enclosing environment of the makeCacheMatrix function
        x <<- y
        m <<- NULL
    }
    
    ## element get  for CacheMatrix, returns itself - matrix x
    get <- function() x
    
    ## element setsolve  for calculationg CacheMatrix inverse
    ## passing inverse of the matrix as solve variable and storing in in m from enclosing environment of the makeCacheMatrix function
    setsolve <- function(solve) m <<- solve
    
    ## element getsolve for CacheMatrix inverse - returning inverse
    getsolve <- function() m
    
    ## return get and set methods for CacheMatrix  'x' and it's inverse
    list(set = set, 
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
    ## get inverse from 'special' matrix x
    m <- x$getsolve()
    
    ## check if catched inverse of 'special' matrix 'x' exits
    if(!is.null(m)) {
        message("getting cached inverse of matrix")
        ## Return a cached matrix that is the inverse of 'x' end get out of function
        return(m)
    }
    
    ## if there is no sotred invers, get norma matrix from special matrix x
    data <- x$get()
    
    ## calculate inverse of  matrix  as it does not exist yet
    m <- solve(data, ...)
    
    ## cache/store just calculated inverse of matrix with setsolve element
    x$setsolve(m)
    
    ## Return just calculated matrix that is the inverse of 'x'. Further on it will be stored and read from there.
    m    
}


## examples for 'special' vector and cached mean value
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
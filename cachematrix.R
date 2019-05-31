## Week3 Assignment: Caching the Inverse of a Matrix
## To write a pair of functions that cache the inverse of a matrix

## Matrix inversion is usually a costly computation 
## there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly

## Example
## a=rbind(c(1,2),c(3,4))
## b<-makeCacheMatrix(a)
## cacheSolve(b)  #Get cached data, the value of the inverse of matrix "a"


## This function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m_inverse <- NULL
    ## Set the value of the matrix
    set <- function(y) {
        x <<- y
        m_inverse <<- NULL
    }
    ## Get the value of the matrix
    get <- function() x
    ## Set the value of the inverse of the matrix
    setinverse <- function(inverse) m_inverse <<- inverse
    ## Get the value of the inverse of the matrix
    getinverse <- function() m_inverse
    ## Return a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    m_inverse <- x$getinverse()
    if(!is.null(m_inverse)) {
        message("getting cached data")
        return(m_inverse)
    }
    data <- x$get()
    m_inverse <- solve(data, ...)
    x$setinverse(m_inverse)
    m_inverse
}


## X is a square invertible matrix
## returns a list containing functions that
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse

## This function creates a special "matrix" object that can cache its inverse.
## For this assignment, assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache. Per assignment instructions.
## the list from makeCacheMatrix is used for input 
## 1. Return a matrix that is the inverse of 'x'
## 2. if the inverse has already been calculated
## 3. get inverse from the cache
## 4. if not in cache, calculate the inverse
## 5. set the values of the inverse in cache with setinv function

cacheSolve <- function(x, ...) {    
        
        inv <- x$getinverse()       
        
        if (!is.null(inv)){
              
                message ("getting data from cache")
                return(inv)
        }
       
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)   
        return(inv)
}


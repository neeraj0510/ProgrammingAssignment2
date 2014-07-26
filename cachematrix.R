## This file has two functions : 1) makeCacheMatrix and 2) cacheSolve
## These two functions together cache the inverse of a matrix, if the inverse
## for the same matrix has been calculated before.
## If the inverse is not present in cache, then the inverse is calculated 
## first time and stored in cache, and from the next time, for the same
## matrix, when the inverse is needed, it is taken from the cache.



## The function below is "makeCacheMatrix". This function creates 
## a list from our given matrix. This list contains four functions:
## 1) "set" function: To set the value of the matrix
## 2) "get" function: To get the value of the matrix
## 3) "setinverse" function: To set any value to the inverse
## 4) "getinverse" function: To get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function below is "cacheSolve". This function calculates the inverse 
## of the special matrix created with the function "makeCacheMatrix" above. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation
## and saves us time. Else, it computes the inverse for the first time
## using the solve() function and stores it in the cache to retrieve it 
## whenever required later. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv        
}

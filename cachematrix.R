## This entire code calculates the inverse of a (invertible) given matrix
## and stores it result in the cache memory. So whenever the result is needed,
## there would be no need to recalculate the inverse of the matrix,
## it takes it from the cache memory.

## The `makeCacheMatrix` function creates a special "Matrix" 
## that stores a list of functions to:
## 
## 1. set the values in the form of a matrix class
## 2. get the values in the form of a matrix class
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ma<- NULL
        set <- function(z) {
                x <<- z
                ma <<- NULL
}
        get <- function() x
        setsolve <- function(solve) ma <<- solve
        getsolve <- function() ma
        
list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## The `cacheSolve` function calculates the inverse of the special `matrix` 
## created with the `makeCacheMatrix` function. However, it first checks 
## to see if the inverse has been calculated. If so, it `get`s the inverse
## from the cache and skips the calculation. If not, it calculates the inverse 
## of the matrix and sets the inverse matrix in the cache 
## via the `setSolve` function.

cacheSolve <- function(x, ...) {
        ma <- x$getsolve()
        if(!is.null(ma)) {
                message("getting cached data")
                return(ma)
  }
        data <- x$get()
                ma <- solve(data, ...)
                x$setsolve(ma)
                ma
}

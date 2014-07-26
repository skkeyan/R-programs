## Cachematrix.R is a program that caches and returns the inverse of a matrix.
## Since matrix inverse is a time consuming operation especially for large matrices, caching the inverse of a matrix is useful

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. Returns a list containing the following functions:
## 1. Set the values in the matrix (set)
## 2. Get the values of the matrix (get)
## 3. Set the values for the inverse of the matrix (setMatrixInverse)
## 4. Get the values of the inverse of the matrix (getMatrixInverse)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatrixInverse <- function(Matrix_Inverse) m <<- Matrix_Inverse
    getMatrixInverse <- function() m
    list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
         
}


## cacheSolve: This function calculates the inverse of the special matrix created with the above function. However, it checks to see if the inverse is 
## already calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise it calculates the inverse and sets the inverse matrix
## in the cache using the setMatrixInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrixInverse()
        if(!is.null(m)){
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrixInverse(m)
        m
}

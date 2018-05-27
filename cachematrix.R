## These functions store the value of a matrix
## and then compute its inverse

## this function creates a special "matrix", which:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse
## 4. gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
     imat <- NULL  ##initialize the 'inverse' matrix to be NULL
     set <- function(y) {
          x <<- y  ##take the user input and set it to 'y'
          imat <<- NULL
     }
     get <- function() x  ##get the value of the user matrix
     setinv <- function(inverse) imat <<- inverse  ##set the value of the inverse
     getinv <- function() imat  ##get the value of the inverse
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## This function calculates the inverse of the special "matrix" created with the above function
## It first checks to see if the inverse has already been calculated; if so, it checks the cache
## and skips the computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     imat <- x$getinv()
     if(!is.null(imat)) {
          message("getting cached data")
          return(imat)
     }
     data <- x$get()
     imat <- solve(data, ...)
     x$setinv(imat)
     imat
}

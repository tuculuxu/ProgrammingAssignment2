## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a square invertible matrix X as an argument and returns a list of 4 functions. These functions are as follows:
## 1 - set -> sets the value of the vector
## 2 - get -> gets the value of the vector
## 3 - setInv -> sets the value of the inverse of the matrix X
## 4 - getInv -> gets the value of the inverse of the matrix X

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(solve) inv <<- solve
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## This function checks whether the inverse of x has already been calculated. If yes then it returns this value, sparing a recalculation, 
## otherwise it just calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}

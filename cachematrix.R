

## Put comments here that give an overall description of what your
## functions do
## These functions were written for Cousera Week 3 programming assignment 


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## This function creates a special "matrix" object that can cache its inverse
        
        makeCacheMatrix <- function(x = matrix()) { ## define the argument with a default object of matrix
                inv <- NULL                             ## initialize inv as a null value, this will hold the inverse of the matrix 
                set <- function(y) {                    ## define the set function to assign new 
                        x <<- y                             ## set the value of matrix in the parent environment
                        inv <<- NULL                        ## if a new matrix is passed, this will assign it to null
                }
                get <- function() x                     ## define the get fucntion, this returns value of the matrix argument
                
                setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
                getinverse <- function() inv                     ## gets the value of inv where called
                list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
                ## to the functions with the $ operator
        }
}

## Write a short comment describing this function
## This function will calculate the inverse of the special "matrix" returned by makeCacheMatrix function above.
## If the inverse has previously been calculated,
## then cacheSolve will get the inverse from the cache instead of computing again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
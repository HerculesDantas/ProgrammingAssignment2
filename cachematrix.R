## Put comments here that give an overall description of what your
## functions do

## A: These functions has the objective to save the inverse matrix given
## any inversible matrix, so the user will not need to recalculate
## the inverse matrix every time he needs it.

## I've tested the functions with the following code:
##test_matrix <- matrix(c(3,9,1,0,1,0,2,7,1),nrow = 3,ncol = 3)
## test_result <- makeCacheMatrix(test_matrix)
## cacheSolve(test_result)

## and it gave me the following result:
##         [,1] [,2] [,3]
##   [1,]    1    0   -2
##   [2,]   -2    1   -3
##   [3,]   -1    0    3
## Which is the inverse matrix of the matrix above.

## Then I used again the cacheSolve(test_result) and it returned the following result
## getting cached data
##         [,1] [,2] [,3]
##   [1,]    1    0   -2
##   [2,]   -2    1   -3
##   [3,]   -1    0    3




## Write a short comment describing this function

## A: This function create a list with 4 functions inside it
## that save in memory get from it a given matrix and another variable
## that will be the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(matrix) m <<- matrix
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function

## A: This function checks if the inverse matrix is already saved
## in memory, and if it is, it will return its value, if not, it will
## calculate and store the result in memory

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

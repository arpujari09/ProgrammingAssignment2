## Two functions makeCacheMatrix and cacheSolve that 
##  creates, store and compute the inverse of a matrix

## The function creates a matrix and then stores its inverse

makeCacheMatrix <- function(x = matrix()) {
        inversematrix <- NULL # Initial  inverse matrix to null
        setmatrix <- function(y) {                    
        x <<- y                  # function to set new value of a matrix            
        inversematrix <<- NULL                        
        }
        getmatrix <- function() x  # funtion to get value of the matrix            
        setinverse <- function(inverse) inversematrix <<- inverse  # function to set the inverse value of the matrix
        getinverse <- function() inversematrix          # function to get the inverse matrix           
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## ##This function computes inverse of matrix returend from 1st function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversematrix <- x$getinverse()
        if(!is.null(inversematrix))  { 
          #message to display data being fetched from cache
        message("Fetching cached data....")
        return(inversematrix)
  }
        data <- x$getmatrix()
        inversematrix <- solve(data, ...)
        x$setinverse(inversematrix)
        inversematrix
}

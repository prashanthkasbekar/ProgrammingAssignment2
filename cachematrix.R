## Author: Prashanth Kasbekar
## Date: 1/25/2015
##
## Description of both functions
##
## makeCacheMatrix and cacheSolve are two functions that when used together will
## solve the problem of computing the inverse of a matrix which is expensive. 
## makeCacheMatrix will cache the inverse of the matrix in question. As long as
## the matrix "x", doesn't change,calling cacheSolve will return the cached 
## value of the inverse and will not compute it again.
## 
## If the user wishes to compute the inverse of a different matrix, he/she 
## should call makeCacheMatrix first and then cacheSolve. Comments are added
## wherever necessary in both functions.
## 
## Assumptions:
## The input matrix to makeCacheMatrix is square
## The matrix is invertible.
##
## 
## Function name: makeCacheMatrix
## Parameters: x : invertible square matrix
## Return Value: list of pointers to functions required to store and retrieve
##               the matrix and its inverse (cached values).
## Description: makeCacheMatrix takes an invertible square matrix as an
##              argument. It stores the inverse matrix in a cache if
##              required again.

makeCacheMatrix <- function(x = matrix()) {
    ## define inverse in the environment of makeCacheMatrix
    inverse<-NULL
    
    ## set function
    set<-function(y)
    {
        ## store x in the parent environment which is that of makeCacheMatrix
        x<<-y
        
        ## Set inverse to NULL in the parent environment.
        inverse<<-NULL
    }
    
    ## get function, returns x which is in parent environment(makeCacheMatrix)
    get<-function()x
    
    ## getinverse function, returns inverse from parent environment
    getinverse<-function()inverse
    
    ## setinverse function, sets inverse in the parent environment
    ## <<- is required because, if <- is used, the inverse variable
    ## would be set inside the environment for setinverse() function.
    ## It would be destroyed on exiting setinverse().
    setinverse<-function(inv)inverse<<-inv
    
    ## return a named list, where each element is a pointer to a function.
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Function name: cacheSolve
## Parameters: x : list of pointers to functions required to store and retrieve 
##                 the matrix and its inverse (cached values).
## Return Value: The inverse of the matrix.
## Description: cacheSolve will compute the inverse of the matrix, if its
##              inverse is not cached. Otherwise it returns the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## inverse is a local variable defined in cacheSolve.
    ## getinverse() returns the cached value of the inverse, if it exists
    inverse <- x$getinverse()
    
    if(!is.null(inverse)) {
        ## Cached value of inverse exists.
        message("Returning cached value of inverse")
        
        ## Simply return the cached value and exit.
        return(inverse)    
    }
    
    ## There is no cached value. Hence get the matrix
    tmpdata <- x$get()
    
    ## Compute inverse
    inv <- solve(tmpdata)
    
    ## Write back inverse into cache.
    x$setinverse(inv)
    
    ## return inverse matrix.
    inv
}

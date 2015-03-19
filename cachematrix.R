#This function creates a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    #initialize the 'inverse' of the matrix x
    i <- NULL
    #function to set the value of the matrix x in the parent environment
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    #function to get the value of the matrix x 
    get <- function() x
    #function to set the inverse value as the inverse value i 
    setInverse <- function(inverse) i <<- inverse
    #get the value of the inverse value i
    getInverse <- function() i
    #list out the values in all the functions in this makeCacheMatrix function
    list(set = set, get = get,
         setInverse = setInverse, 
         getInverse = getInverse)
}

#This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    #get the value of i stored in the environment where makeCacheMatrix(x) was calculated
    i <- x$getInverse()
    #print a cached value of i, if present
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    #if there is no cached value of i, store matrix x into 'data'
    data <- x$get()
    #get the inverse of matrix x (using the solve function) and store it as i
    i <- solve(data, ...)
    #set the calculated inverse i as the inverse of matrix x
    x$setInverse(i)
    #show the inverse
    i
}
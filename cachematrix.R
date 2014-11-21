## Read a matrix and cache its inverse
## There are 2 functions below. 
##One to create a matrix and the other to calculate the inverse

## The first function here takes a matrix as input and creates a list 
## the list contains the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}


## The second function here checks if the inverse has already been cached. 
## If yes then it returns the cached value. 
##Else it calculates the inverse and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
       message("getting cached data")
       return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i

}

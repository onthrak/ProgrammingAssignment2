#  First sorry for my bad english ;)
# function creates list of functions, which contains:
# set -> set the value of the matrix
# get -> get the value of the matrix
# setinverse -> set the value of the inverse of matrix
# getinverse -> get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inversefun)  inverse<<- inversefun
        getinverse <- function() inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# casheSolve
# function returns the inverse of the matrix if the inverse hasnt been computed. If inverse has been computed,
# it gets the result and skips the computation
# ah, in this case we assume that the matrix supplied is always invertible. so if You get an error, please check if
# your matrix is invertible. Thank You :)
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data.")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}

#This function takes a matrix and it returns a list of functions which 1) set the value of the matrix 2) gets the value of the matrix 3) sets the value of the inverse of the matrix 4) gets the value of the inverse of the matrix

makeCacheMatrix <- function(x=matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(z) inv <<- z
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

#This function takes a matrix and returns the inverse matrix. If the inverse has not been computed before, it computes and returns the inverse and also stores the value of the inverse in the above object. If the inverse has been computed before, it gives the message "getting cached data" and returns the value of the inverse stored in the object created above.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
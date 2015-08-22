## Put comments here that give an overall description of what your
## functions do

## this function creates a "cached matrix", which caches the 
## not only the matrix, var "x", but also its inverse, var "cmInv"

makeCacheMatrix <- function(x = matrix()) {
    cmInv <- NULL
    set <- function(nm) {     # store the new matrix and set cmInv to NULL
             # NOTE: cacheSolve(x) must later be called to set cmInv
        x <<- nm
        cmInv <<- NULL
    }
    get <- function() x    # simply return the matrix
    setInv <- function(newInv) cmInv <<- newInv
    getInv <- function() cmInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
    
 
}


## This calculates the inverse of matrix "x".
## It first checks to see if there is a cached solution, if not 
## it then solves for the inverse and sets the cached solution.
##

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if (!is.null(inv)) {     # if a cached version exists, return it
        message("getting cached inverse of matrix")
        return(inv)
    }
    # if not cached solutin exits, solve at set cached solution
    tempMat <- x$get()
    inv <- solve(tempMat)
    x$setInv(inv)
    inv
}


## to call/use, see commented out calls below
# aMatrix <- makeCacheMatrix()
# aMatrix$set( matrix(c(4,3,3,2),2,2,byrow=TRUE)  )
# aMatrix$getInv()   # should return null because cacheInverse() not yet called
# cacheInverse(aMatrix)
# aMatrix$getInv()







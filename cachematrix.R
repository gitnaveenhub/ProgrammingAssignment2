# MakeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# i have used "InputMat" in the place of "x"


makeCacheMatrix <- function(InputMat = matrix()) {
    MatrixInverse <- NULL
    set <- function(AssignMat) {
        InputMat <<- AssignMat
        MatrixInverse <<- NULL
    }
    get <- function() InputMat
    setinverse <- function(inverse) MatrixInverse <<- inverse
    getinverse <- function() MatrixInverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# CacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(InputMat, ...) {
    inv <- InputMat$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    MatData <- InputMat$get()
    MatrixInverse <- solve(MatData)
    InputMat$setinverse(MatrixInverse)
    MatrixInverse
}

#Execution and Output validation

InputMat <- matrix(1:4,2,2)
GetCacheMat <- makeCacheMatrix(InputMat)

GetCacheMat$get()
#       [,1] [,2]
#[1,]    1    3
#[2,]    2    4

cacheSolve(GetCacheMat) #Run 01
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

cacheSolve(GetCacheMat) #Run 02
#getting cached data.
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
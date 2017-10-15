## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The first function, makeVector creates a special "vector", which is really a list containing a function to
##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean


## The function makecachematrix creates a matrix,
## later we have to populate "m" with the function "m$set" i.e -m$set(matrix(c(1:4),2,2))
## Additionaly we can get the value of "m" "using m$get()"
## alternatively we can get or set the inverse of "m", getinv will get the value of "m" assigned in setinv.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## the function cachesolve takes the inverse of "m", if "m" is not null,
## will get the "cache data" message and the value.
## If not it will call the get and setinv functions within makecachematrix 

cacheSolve <- function(x){
     m <- x$getinv()
     if(!is.null(m)) {
         message("getting cached data")
         return(m)
     }
     data <- x$get()
     m <- solve(data)
     x$setinv(m)
     m
 }
##test
## > source("cachematrix.R")
# > m <- makeCacheMatrix()
# > m$set(matrix(c(1:4),2,2))
# > m$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 
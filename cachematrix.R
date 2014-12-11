## These functions create a special matrix that can cache its inverse


## The first function creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the incerse

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y) {
        y<<-x
        inv<<-NULL
    }
    get<-function() x
    setinv<-function(inverse) inv<<-inverse
    getinv<-function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data)
    x$setinv(inv)
    return(inv)
}

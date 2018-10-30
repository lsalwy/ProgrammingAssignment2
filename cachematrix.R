## computing the inverse of a matrix and cache it to 
## accelerate subequent access to the same matrix

## makeCacheMatrix fucntion creates a special 
##"matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {s <- NULL
set <- function(y){
        x <<- y
        s <<- NULL
}
        get <- function()x
        setsol <- function(sol) s <<- sol
        getsol <- function()s
        list(set = set, get = get,
        setsol = setsol,
        getsol = getsol)

}


## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function above. If the inverse has 
## already been calculated, then cacheSolve should retrive the inverse 
## stored in the cache

cacheSolve <- function(x, ...) {
        s <- x$getsol()
        if(! is.null(s)){
                message("getting cached data")
                return(s)}
        matrix <- x$get()
        s <- solve(matrix, ...)
        x$setsol(s)
        s
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(matr) {
                x <- matr
                inv <- NULL
        }
        get <- function() x
        set_cache <- function(m) inv <- m   #when to use "<<-"?
        get_inv <- function() inv
        list(set = set, get = get,
             set_cache = set_cache,
             get_inv = get_inv)
}


## Write a short comment describing this function

cacheSolve <- function(funclist, ...) {
        ## Return a matrix that is the inverse of 'x'
        in_ma<-funclist$get_inv()
        if (!is.null(in_ma)) {
                message("getting cached data")
                return(in_ma)
        }
        in_ma<-solve(funclist$get())
        funclist$set_cache(in_ma)
        in_ma
        
}

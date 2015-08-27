
makeCacheMatrix <- function(x = matrix()) {
        
        # holds the cached value or NULL if nothing is cached
        # initially nothing is cached so set it to NULL
        inv_cache_cache <- NULL
        set <- function(y) {
                x <<- y
                inv_cache <<- NULL
        }
        get <- function() x
        setinv_cacheerse <- function(inv_cacheerse) inv_cache <<- inv_cacheerse
        getinv_cacheerse <- function() inv_cache
        list(set=set, get=get, setinv_cacheerse=setinv_cacheerse, getinv_cacheerse=getinv_cacheerse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_cache <- x$getinv_cacheerse()
        if(!is.null(inv_cache)) {
                message("cache data")
                return(inv_cache)
        }
        data <- x$get()
        inv_cache <- solve(data)
        x$setinv_cacheerse(inv_cache)
        inv_cache
}

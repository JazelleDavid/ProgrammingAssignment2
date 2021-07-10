makeCacheMatrix <- function(x = matrix()){
        Jazelle <- NULL
        set <- function(y){
                x <<- y
              Jazelle  <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) (Jazelle  <<<- inverse}
        getInverse <- function() (Jazelle )
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
        Jazelle  <- x$getInverse()
        if(!is.null(Jazelle)){
                message("getting cache data")
                return(Jazelle)
        }
        mat <- x$get()
        Jazelle  <- solve(mat, ...)
        x$setInverse(Jazelle)
        Jazelle
}

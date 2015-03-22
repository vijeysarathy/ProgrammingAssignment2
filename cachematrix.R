## An R function to potentially cache time-consuming computations
## The first function, makecachematrix creates a list
## of 4 functions which set, get, inverse of set and get
## uses <<- assignment operator "an internal assignment 
## operator"

makeCacheMatrix <- function(x = matrix()) {
    ## @x: a square invertible matrix
    ## return : a list containing above 4 functions
    ## and is used as an input to cachesolve
    inv = NULL
    set = function(y) {
          # use ' <<-' to assign a value to an object in an
          # environment different from current environment.
          x <<- y
          inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Below function uses cachesolve function to retrieve
## the inverse from the cache as long as the matrix has
## not changed.

cacheSolve <- function(x, ...) {
        ## @ x: output of makecachematrix()
        ## inverse of the original matrix input
        ## to makeCacheMatrix()
  
        inv = x$getinv()
        
        # if the inverse has already available
        if(!is.null(inv)) {
              # get it from the cache and skips the computation
              message("getting cached data")
              return(inv)
              
        }
        
        # otherwise calculate the inverse
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache
        # via the inverse function
        
        x$setinv(inv)
        
        return(inv)
}

test = function(mat){
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}

set.seed(1110201)
r = rnorm(250000)
mat1 = matrix(r, nrow=500, ncol=500)
test(mat1)



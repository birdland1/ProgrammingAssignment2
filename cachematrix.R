## MATRTIX INVERSION & CASHING THE RESULT
##   The two functions below ...
##      a) Calculate the inverse of a matrix and write the result into cache, 
##      b) Validate whether a cached value already exists for a 
##              given matrix, and if it does then it uses the cached result
##              while informing the user that a cached value is used as result.
##   The purpose of leveraging a cached result is that matrix inversions could 
##      take a long time to compute, and using a cached value would save time.

## The "makeCacheMatrix" function generates as special "Matrix Object" that can
##      store its inverse matrix in a cache.

makeCacheMatrix <- function(x = matrix()) {
        inv_my_matrix <- NULL
        set <- function(y) {
                x <<- y
                inv_my_matrix <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv_my_matrix <<- inverse
        get_inverse <- function() inv_my_matrix
        list (set = set, get = get,
              set_inverse = set_inverse,
              get_inverse = get_inverse)
}


## The "CacheSolve" function uses the "solve" function to caculate the inverse 
##      of the special matrix object from the "makeCacheMatrix" function.
## If the inverse matrix calculation for a given matrix was already done,
##      and values did not change, then the cached result will be displayed.

cacheSolve <- function(x, ...) {
        inv_my_matrix <- x$get_inverse()
        if(!is.null(inv_my_matrix)) {
                message("getting cached data")
                return(inv_my_matrix)
        }
        my_matrix <- x$get()
        inv_my_matrix <- solve(my_matrix, ...)
        x$set_inverse(inv_my_matrix)
        inv_my_matrix
        
}

## EXAMPLE: Testing the functions...
## 1. Assign to a test-variable (e.g. "test_matrix)" the definition of the 
##      matrix-content and execute function "makeCacheMatrix".
##       test_matrix <- makeCacheMatrix(matrix(c(4, 2, 7, 6), 2, 2))
## 1a) print the values of the "test_matrix"
##      > test_matrix$get()
##                      [,1] [,2]
##              [1,]    4    7
##              [2,]    2    6
## 1b) print: "test_matrix$get_inverse()"
##      Result shuld be "NULL", and it indicates that the cache is empty 
##              (= initialized value)
## 2) Execute function "cacheSolve(test_matrix)". The inverse matrix is 
##      returned:
##      > cacheSolve(test_matrix)
##                   [,1] [,2]
##              [1,] 0.6 -0.7
##              [2,] 0.2  0.4
## 3) Execute function "cacheSolve(test_matrix)" again. Now the message 
##      "getting cached data" is returned together with the previously cached
##      result:
##      > cacheSolve(test_matrix)
##              getting cached data
##                    [,1] [,2]
##              [1,]  0.6 -0.7
##              [2,] -0.2  0.4
## 4) Now repeat steps 1-3 with other values for "test_matrix". Notice
##      on the first pass for the new matrix values the cache will be NULL
##      again, and on the second pass the newly cached result will be used.
##

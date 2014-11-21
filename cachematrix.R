# The makeCacheMatrix and cacheSolve defined in this file 
# provide functionality to cache the result of the 
# potentially time-consuming matrix-inversion computation.
# It may too computationally expensive to derive the inverse, 
# especially if it has to be computed repeatedly (e.g. in a loop).
# The functions in this file provide the option to look up 
# the inverted matrix in the cache rather than recompute it each time.



# The first function (makeCacheMatrix) creates four functions which respectively
# - set the value of the matrix;
# - get the value of the matrix;
# - set the value of inverse of the matrix;
# - get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    invOfXmatrix <- NULL
    
    # Original matrix setter function which also resets the inverse of the matrix to NULL.
    set <- function(a) {
        x <<- a
        invOfXmatrix <<- NULL
    }
    
    # Original matrix getter function.
    get <- function() x
    
    # Inverse matrix setter function.
    setinv <- function(inverse) invOfXmatrix <<- inverse
    
    # Inverse matrix getter function.
    getinv <- function() invOfXmatrix
    
    # Return the matrix with the set of new functions that we have just defined.
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}





# The second function (cacheSolve) returns a matrix that is the inverse of 'x'
# It assumes that the matrix is always invertible.
# If the inverse has alredy been computed cacheSolve returns the cached data.
# If the inverse has not yer been computed 
# cacheSolve computes the result, caches it and returns it.

cacheSolve <- function(x, ...) {
    # Assign the current version of the inverse matrix to invOfXmatrix.
    invOfXmatrix <- x$getinv()
    
    # If the current version of the inverse matrix (invOfXmatrix)
    # has been computed in the past (i.e. invOfXmatrix is not Null)
    # then return the cached data.
    if(!is.null(invOfXmatrix)) {
        message('getting cached data')
        return(invOfXmatrix)
    }
    
    # If the current version of the inverse matrix (invOfXmatrix)
    # has not been computed in the past (i.e. invOfXmatrix is Null)
    # then compute the inverse and cache it.
    data <- x$get()
    invOfXmatrix <- solve(data)
    x$setinv(invOfXmatrix)
    
    # Returns the matrix that is the inverse of 'x'.
    invOfXmatrix
}




# Example use:
# 
# # 1: creating the matrix
# > x = rbind(c(1, 1/2, 1/3, 1/5, 1/7), c(1/13, 1/17, 1/19, 1/23, 1/29), c(1/31, 1/37, 1/41, 1/43, 1/47), c(1/53, 1/59, 1/61, 1/67, 1/71), c(1/73, 1/79, 1/83, 1/89, 1/97))
#
# # 2: Create a matrix with a set of new functions (getters and setters) that we have defined.
# > cm <- makeCacheMatrix(x)
#
# # 3: Return the new special matrix.
# > cm$get()
#            [,1]       [,2]       [,3]       [,4]       [,5]
# [1,] 1.00000000 0.50000000 0.33333333 0.20000000 0.14285714
# [2,] 0.07692308 0.05882353 0.05263158 0.04347826 0.03448276
# [3,] 0.03225806 0.02702703 0.02439024 0.02325581 0.02127660
# [4,] 0.01886792 0.01694915 0.01639344 0.01492537 0.01408451
# [5,] 0.01369863 0.01265823 0.01204819 0.01123596 0.01030928
#
# # 4: Return the inverse of the new special matrix.
# > cacheSolve(cm)
#
#            [,1]      [,2]        [,3]         [,4]      [,5]
# [1,]  -7.042271  126.2680   653.94032  1119.844330 -3204.309
# [2,]  28.362853 -476.9373 -1891.35094 -3955.351599 10509.461
# [3,]  -8.597354  209.3785    36.40268  2013.447631 -3407.096
# [4,] -24.163403  398.9662  1761.91663   939.280507 -5919.176
# [5,]  10.915166 -261.6980  -509.47677    -8.216898  1883.781
#
# # 5: Ask for the inverse of the new special matrix again and respecively have the cached values returned.
# > cacheSolve(cm)
#
# getting cached data
#
#            [,1]      [,2]        [,3]         [,4]      [,5]
# [1,]  -7.042271  126.2680   653.94032  1119.844330 -3204.309
# [2,]  28.362853 -476.9373 -1891.35094 -3955.351599 10509.461
# [3,]  -8.597354  209.3785    36.40268  2013.447631 -3407.096
# [4,] -24.163403  398.9662  1761.91663   939.280507 -5919.176
# [5,]  10.915166 -261.6980  -509.47677    -8.216898  1883.781
#
# The end.

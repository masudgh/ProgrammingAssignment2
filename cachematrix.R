## The functions will be useful to calculate a matrix inversion. There are two function 'makeCacheMatrix'
#and 'cacheSolve' which are interdependent to each other. The goal is to calculate inverse of a matrix 
#from the 'cache' if available.
#
#Example: The following example will show , how the functions can be use to calculate inverse matrix
#
#Create a smaple matrix:
# > tmp<-c (1,2,3,0,1,4,5,6,0)
# > m<-matrix(tmp,ncol=3,byrow=TRUE)
# > m
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    0    1    4
# [3,]    5    6    0
# 
#Setup the Environment:
# >m.tmp<-makeCacheMatrix(m)
# 
#Calculate the inverse of the matrix for the FIRST time:
# > cacheSolve(m.tmp)  #First time the calculation will be done using R 'solve' function 
#      [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
#
#Calculate the inverse of the same matrix again:
# > cacheSolve(m.tmp)  # The stored inverse matrix will be retrived from 'cache'
# getting cached data # <---- a comment about 'cache'
#      [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1



#--------------------------------------------------------------------------------------
## makeCacheMatrix: The function is required to setup an environment to calculate 
# the inverse of a matrix 
# 
# input x : A valid matrix object which is valid for matrix-inverse calculation
# output list(..) : A list of objects which are functions,i.e. set, get, setInverse and 
# getInverse.Each function has specific task as explained in the function's comments 
#-------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
        #This is inverse matrix, initialize to null
        inv.m<- NULL
        
        #setter function to set original matrix 'x' variable from a passive variable 'y'
        set <- function (y) {
                x <<- y
                inv.m <<- NULL
        }
        
        #getter function to retrive the value of the original matrix 'x' variable
        get <- function() x
        
        #setter function to set the calculated inverse matrix to a variable 'inv.m'
        setInverse <- function(im) inv.m <<- im
        
        #getter function to retrive the stored inverse matrix from a variable 'inv.m'
        getInverse <- function() inv.m
        
        #return list of variables
        list (set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse
              )
}

#------------------------------------------------------------------------------------------
## cacheSolve: The function is required to calculate the inverse of a matrix using 'cache'
# 
# input x: A list of objects which are functions,i.e. set, get, setInverse and getInverse.
# output inv.m : A calculated matrix object 
#-----------------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv.m <- x$getInverse()
        
        if(!is.null(inv.m)){
                message("getting cached data")
                return(inv.m)
        }
        #retrive the original matrix
        data <- x$get()
        
        #calculate inverse natrix
        #inv.m <- data %*% solve(data)
        inv.m <- solve(data)
        
        #putting the inverse matrix value back to the cache
        x$setInverse(inv.m)
        
        #return calculated inverse matrix
        inv.m
}

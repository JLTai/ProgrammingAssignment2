## makeCacheMatrix sets up various functions which can create a 
## cached Inverse Matrix
##
## inverse matrix = determinant multiplied by the adjugate matrix
## cacheSolve activates all sub fauntions in makeCacheMatrix, 
## loading if possible the cached Matrix
## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
# variable to handle number of rows, as inverse matrix has to be square
    n<-nrow(x)
    
    cacheInvMatrix<-NULL

    set <- function(y) {#function takes in a variable adn overwrites both 
        x <<- y #__outside the scope of this function
        cacheInvMatrix <<- NULL
    }

# returns the value of input matrix
    get <- function() x 

# adjEle calculates the adjugate cofactor
    adjEle<-function(i,j) ((-1)^(i+j))*x[n-i+1,n-j+1]

# Calculate cofactor Matrix
    getCoMatrix <- function() {
        coMatrix<-matrix(numeric(),n,n)
        for(i in 1:n){
            for(j in 1:n){
                coMatrix[j,i]<-adjEle(i,j)
            }
        }
        coMatrix
    }

# utilising the det function in R to calculate the determinant
    getDet<-function() {
        detMatrix<-det(x)
        detMatrix
    }

# following the makeVector example, loading Inverse Matrix to cache
    setInvMatrix<- function(InvMatrix) cacheInvMatrix<<-InvMatrix
# returns cached Inversed Matrix
    getinverse <- function() cacheInvMatrix 

    list(
        set=set,
        setInvMatrix=setInvMatrix,
        getCoMatrix=getCoMatrix,
        get = get, 
        detMatrix = detMatrix,
        getDet = getDet, 
        getinverse=getinverse
    )
        
}

## CacheSolve takes in the returned list from above and uses the cached
## data of matrix and prints to screen, if no cached present, then calculates
## and saves to cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cacheInvMatrix <- x$getinverse()
    if(!is.null(cacheInvMatrix)) {
        message("getting cached data")
        return(cacheInvMatrix)
    }
    
    detMatrix<-x$getDet() #gets the determinant
    coMatrix<-x$getCoMatrix()#gets the cofactor Matrix
    cacheInvMatrix<-(1/detMatrix)*coMatrix #calculates Inverse Matrix
    x$setInvMatrix(cacheInvMatrix)# save the inverse Matrix to cache
    cacheInvMatrix
}
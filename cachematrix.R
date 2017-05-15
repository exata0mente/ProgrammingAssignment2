## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        MatrixInv <- NULL
        
        setMatrixValue <- function(NewMatrix = matrix()){ ## Recebe novos valores para a matriz        
                MatrixData <<- NewMatrix
                MatrixInv <<- NULL ## Como são novos valores precisaremos "resetar" o cache
        } 
        getMatrixValue <- function() MatrixData ## Define o valor da matriz
        
        
        setMatrixInv <- function(Inverso){ ## Recebe o inverso da matriz e armazena o Inverso recebido porparâmetro à variável MatrixInv
                MatrixInv <<- Inverso        
        }                                   
        
        getMatrixInv <- function(){ ## Define o inverso da matriz
                MatrixInv 
        }
        
        list(set = setMatrixValue,
             get = getMatrixValue,
             setInv = setMatrixInv,
             getInv = getMatrixInv)
}


## Write a short comment describing this function

cacheSolve <- function(NewMatrix, ...) {
        MatrixInv <- NewMatrix$getInv() ## Recebe o valor do inverso da Matriz
        if(!is.null(MatrixInv)){ ## Caso já exista o valor em cache ...
                
                message("Inverso da Matriz já está armazenado")
                return(MatrixInv) ## O valor é impresso na tela.
        }
        
        MatrixData <- NewMatrix$get() ## Recebe os valores "bruto" da matriz
        MatrixInv <- solve(MatrixData, ...) ## Calcula o inverso da matriz
        
        NewMatrix$setInv(MatrixInv)## "Envia" ao cache o valor calculado
        
        MatrixInv                           ## Devolve ao usuário o inverso da
}

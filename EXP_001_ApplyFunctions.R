# Housekeeping -----------------------------------------------------------------

library(tidyverse)


# Tests ------------------------------------------------------------------------

# apply, lapply, vapply, sapply, tapply, eapply, mapply, sweep, aggregate


# sapply é wrapper para lapply
# sapply(x, f, simplify = FALSE, USE.NAMES = FALSE) == lapply(x,f)
# vapply é similar a sapply
# replicate é wrapper para uso comum de sapply

# mapply is a multivariate version of sapply

# Apply ------------------------------------------------------------------------

M <- matrix(1:12, nrow = 4)

apply(X = M, MARGIN = 1, FUN = function(n){n+1})
apply(X = M, MARGIN = 2, FUN = function(n){n+1})
apply(X = M, MARGIN = 1:2, FUN = function(n){n+1})

apply(X = M, MARGIN = 1, FUN = mean) # linha a linha
apply(X = M, MARGIN = 2, FUN = mean) # coluna a coluna
apply(X = M, MARGIN = 1:2, FUN = mean) # elemento a elemento
apply(X = M, MARGIN = 2:1, FUN = mean) # elemento a elemento com transposição

DF <- apply(X = M, MARGIN = 2, FUN = as.data.frame)


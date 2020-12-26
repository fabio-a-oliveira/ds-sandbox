library('caret')

x <- matrix(runif(20,0,1), ncol = 2)
colnames(x) <- c('x1','x2')

y <- matrix(sample(c(0,1),10,replace = T),
            ncol = 1)

# RMSE error
train(x = x,
      y = y)

# changing metric does not solve it
train(x = x,
      y = y,
      metric = 'Accuracy')

# changing y to vector solves it (as.numeric and as.integer also do)
train(x = x,
      y = as.vector(y))

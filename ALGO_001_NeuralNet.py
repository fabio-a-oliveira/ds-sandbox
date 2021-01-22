class NeuralNet(object):
    """
    Simple class implementing a vanilla Neural Network for binary classification
    """
    
# %%
    
    def __init__(self, L=1, n_x=1, n_h=1, n_y=1):
        """
        Initialize NeuralNet object
        
        Arguments:
        n_x -- units in input layer
        n_h -- units in hidden layer (as integer or list)
        n_y -- units in output layer
        """
        
        import numpy as np
        
        self.L = L
        self.n_x = n_x
        self.n_y = 1
        self.activation = "sigmoid"

        # if type(n_h) == int:
        if isinstance(n_h,int):
            self.n_h = [n_h for i in range(L-1)]
        else:
            self.n_h = n_h
        
        self.n = []
        
        for i in range(L+1):
            if i == 0: 
                self.n.append(n_x)
            elif i < L:
                self.n.append(self.n_h[i-1])
            else:
                self.n.append(n_y)
            
        self.W, self.b = ([0],[0])        
        for l in range(1,L+1):
            W_l = np.random.randn(self.n[l],self.n[l-1]) * 0.5
            b_l = np.random.randn(self.n[l],1) * 0.5
            self.W.append(W_l)
            self.b.append(b_l)

# %%            
    def propagate(self, X, Y):
        """
        Perform 1 step of forward and backpropagation
        
        Arguments:
        X -- input vector (n x m : n variables, m observations)
        Y -- output vector - "ground truth" (1 x m : 1 output, m observations)
        alpha -- learning rate
        
        Return:
        dW - update to vector of weights
        db -- update to vector of biases
        J -- cost function calculated on entire set
        """
        
        import numpy as np
        
        Z,A = ([0],[])
        A.append(X)

        # forward propagation

        for l in range(1,self.L+1):
            
            if self.activation == "sigmoid":
                Z.append(np.dot(self.W[l],A[l-1]) + self.b[l])
                A.append(1 / (1 + np.exp(-Z[l])))  
        
        # cost function        
        
        Y_hat = A[-1]
        m = X.shape[1]
        # J = -1/m * np.sum(Y * np.log(Y_hat) + (1-Y)*np.log(1-Y_hat))
        J = -1/m * np.sum(np.multiply(Y,np.log(Y_hat)) + np.multiply(1-Y,np.log(1-Y_hat)))
    
        # back propagation
        
        dZ = [[] for l in range(0,self.L+1)]
        dW = [[] for l in range(0,self.L+1)]
        db = [[] for l in range(0,self.L+1)]
    
        for l in reversed(range(1,self.L+1)):
            
            if l == self.L:
                dZ[l] = A[l] - Y
            elif self.activation == 'sigmoid':
                s = 1 / (1 + np.exp(-Z[l]))
                s_inv = s * (1-s)
                dZ[l] = np.dot(self.W[l+1].T,dZ[l+1]) * s_inv
           
            dW[l] = 1/m * np.dot(dZ[l],A[l-1].T)
            db[l] = 1/m * np.sum(dZ[l], axis=1, keepdims=True)
            
        
        # output
        
        return J,dW,db
            
# %%
    def train(self, X, Y, alpha=0.01, iterations=10000, verbose=False):       
        """
        Train network through gradient descent
        
        Arguments:
        X -- input vector (n x m : n variables, m observations)
        Y -- output vector - "ground truth" (1 x m : 1 output, m observations)
        alpha -- learning rate
        iterations -- number of epochs for gradient descent
        """
        
        if verbose is True:
            import time
            tic = time.time()
        
        cost = []
        
        for step in range(iterations):
            
            J, dW, db = self.propagate(X,Y)
            cost.append(J)
            
            if (verbose is True) and (step % 100 == 0):
                toc = time.time()
                print("iteration " + str(step) + ", cost = " + str(round(cost[-1],6)) + ", elapsed = " + str(round(toc-tic)) + "s")
            
            for l in range(1,self.L+1):
                self.W[l] -= alpha * dW[l]
                self.b[l] -= alpha * db[l]
                
        return cost
        
# %%
    def predict(self, X, Y=False):
        """
        Makes predictions on X vector and calculates cost function on Y vector
        
        Arguments:
        X -- input vector (n x m : n variables, m observations)
        Y -- output vector - "ground truth" (1 x m : 1 output, m observations)
        
        Return:
        Y_hat -- predictions on input vector (1 x m : 1 output, m observations)
        J -- cost function, if ground truth is supplied (1 float)
        """
        
        import numpy as np
        
        Z,A = ([0],[])
        A.append(X)

        # forward propagation

        for l in range(1,self.L+1):
            
            if self.activation == "sigmoid":
                Z.append(np.dot(self.W[l],A[l-1]) + self.b[l])
                A.append(1 / (1 + np.exp(-Z[l])))  
        
        # cost function        
        
        Y_hat = A[-1]
        m = X.shape[1]
        
        if Y is not False:
            J = -1/m * np.sum(np.multiply(Y,np.log(Y_hat)) + np.multiply(1-Y,np.log(1-Y_hat)))
            return Y_hat,J
        
        else:
            return Y_hat

# %% 
  

# IMPROVEMENTS:
    # - train() to receive x_test and y_test, keep cost at each step
    # - include property: total gradient descent steps taken after multiple calls to train()
    # - at each gradient step, print total over all calls to train(), instead of the current one
    # - include properties: total number of unities, total number of parameters
    # - multiple outputs in parallel for multilabel task
    # - save/load NN parameters for later use
    # - better parameter initialization
# %%

import numpy as np
import pandas as pd
import time
from matplotlib import pyplot as plt

# %%

train_features = pd.read_csv("datasets\\MoA\\train_features")
train_features.columns = [col.replace('train_features.','') for col in list(train_features.columns)]
train_features.set_index('sig_id',drop=True,inplace=True)

train_targets = pd.read_csv("datasets\\MoA\\train_targets")
train_targets.columns = [col.replace('train_targets_scored.','') for col in list(train_targets.columns)]
train_targets.set_index('sig_id',drop=True,inplace=True)

df = train_features.join(train_targets)
del(train_features,train_targets)

df = df.loc[df.cp_type != "ctl_vehicle"]
df.drop(['cp_type','cp_dose','cp_time'],axis=1)

mechanism = "proteasome_inhibitor" # others - "nfkb_inhibitor", "proteasome_inhibitor",  "cyclooxygenase_inhibitor"

df_X = df.sample(frac=0.8).loc[:,"g.0":"c.99"]
df_Y = df.loc[X.index, mechanism]

df_X_test = df.drop(X.index,axis=0).loc[:,"g.0":"c.99"]
df_Y_test = df.loc[X_test.index, mechanism]

X = np.array(df_X).T
Y = np.array(df_Y).T

X_test = np.array(df_X_test).T
Y_test = np.array(df_Y_test).T

# %%

NN = NeuralNet(L=5, n_x = X.shape[1], n_h = [20, 10, 7, 3])

NN.n
[NN.W[l].shape for l in range(1,NN.L+1)]
[NN.b[l].shape for l in range(1,NN.L+1)]

cost = []

# %%

cost.extend(NN.train(X, Y, alpha=1, iterations=10000, verbose=True))
#cost.extend(NN.train(np.array(X).T, np.array(Y).T, alpha=0.001, iterations=10000, verbose=True))

(plt.plot(cost), plt.yscale('log'), plt.show())

Y_hat, cost_test = NN.predict(X_test, Y_test)
print("Cost after training:\nJ_train: {}\nJ_test: {}".format(str(cost[-1]), str(cost_test)))    

# %%

NN_5L = NeuralNet(L=5, n_x = X.shape[1], n_h = [20, 10, 7, 3])

NN_5L.n
[NN_5L.W[l].shape for l in range(1,NN_5L.L+1)]
[NN_5L.b[l].shape for l in range(1,NN_5L.L+1)]

cost_5L = []
cost_5L_test = []

# %%

#cost_5L.extend(NN_5L.train(np.array(X).T, np.array(Y).T, alpha=1, iterations=10000, verbose=True))
#(plt.plot(cost_5L), plt.yscale('log'), plt.show())
#Y_hat, cost_test = NN_5L.predict(np.array(X_test).T,np.array(Y_test).T)
#print("Cost after training:\nJ_train: {}\nJ_test: {}".format(str(cost_5L[-1]), str(cost_test)))

tic = time.time()
for step in range(10000):
    cost_5L.extend(NN_5L.train(X, Y, alpha=1, iterations=1))
    Y_hat, cost_test = NN_5L.predict(X_test, Y_test)
    cost_5L_test.append(cost_test)
    if step % 1000 == 0:
        print('iteration {}, cost_train = {}, cost_test = {}, elapsed = {}s'.format(str(step),
                                                                                   str(round(cost_5L[-1],6)),
                                                                                   str(round(cost_5L_test[-1],6)),
                                                                                   str(round(time.time()-tic))))

(plt.plot(np.arange(len(cost_5L)), cost_5L, np.arange(len(cost_5L_test)), cost_5L_test), plt.yscale('log'), plt.show())

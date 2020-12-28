# %%
# =============================================================================
# COURSE 4 - MODULE 2 - LINEAR REGRESSION
# =============================================================================

# Housekeeping

%reset -f

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import pylab as pl
import requests

%matplotlib qt

# Import the data

# command used in IBM lab
#!wget -O FuelConsumption.csv https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-ML0101EN-Coursera/labs/Data_files/FuelConsumptionCo2.csv

# command that works in spyder
URL = "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-ML0101EN-Coursera/labs/Data_files/FuelConsumptionCo2.csv"
df = pd.read_csv(URL)

# Data exploration

df.columns
df.dtypes
df.shape

df[['CYLINDERS','ENGINESIZE','FUELCONSUMPTION_COMB','CO2EMISSIONS']].hist()

plt.scatter(df.FUELCONSUMPTION_COMB, df.CO2EMISSIONS)
plt.scatter('ENGINESIZE', 'C02EMISSIONS', data = df)

df.plot.scatter('ENGINESIZE','CO2EMISSIONS')
df.plot.scatter('CYLINDERS','CO2EMISSIONS')

df.FUELTYPE.unique()

df[df.FUELTYPE == 'Z'].plot.scatter('FUELCONSUMPTION_COMB','CO2EMISSIONS')
df[df.FUELTYPE == 'D'].plot.scatter('FUELCONSUMPTION_COMB','CO2EMISSIONS')
df[df.FUELTYPE == 'X'].plot.scatter('FUELCONSUMPTION_COMB','CO2EMISSIONS')
df[df.FUELTYPE == 'E'].plot.scatter('FUELCONSUMPTION_COMB','CO2EMISSIONS')

ax_Z = df[df.FUELTYPE == 'Z'].plot.scatter('FUELCONSUMPTION_COMB','CO2EMISSIONS',color='b')
ax_D = df[df.FUELTYPE == 'D'].plot.scatter('FUELCONSUMPTION_COMB','CO2EMISSIONS',color='r',ax=ax_Z)
ax_X = df[df.FUELTYPE == 'X'].plot.scatter('FUELCONSUMPTION_COMB','CO2EMISSIONS',color='g',ax=ax_Z)
ax_E = df[df.FUELTYPE == 'E'].plot.scatter('FUELCONSUMPTION_COMB','CO2EMISSIONS',color='black',ax=ax_Z)
ax_Z.set(title = 'CO2 emissions vs fuel consumption for different fuel types')

# Train and test split

cdf = df[['CYLINDERS','ENGINESIZE','FUELCONSUMPTION_COMB','CO2EMISSIONS']]
mask = np.random.rand(len(cdf)) < 0.8
train = cdf[mask]
test = cdf[~mask]

# Perform linear regression

from sklearn import linear_model
reg = linear_model.LinearRegression()
reg.fit(train[['CYLINDERS','ENGINESIZE','FUELCONSUMPTION_COMB']], train[['CO2EMISSIONS']])

# Evaluate output

reg.intercept_
reg.coef_

test_y_hat = reg.predict(test.drop('CO2EMISSIONS',axis=1))
test_y = test.CO2EMISSIONS

from sklearn.metrics import r2_score
r2_score(test_y_hat, test_y)

np.sqrt(np.mean((np.asanyarray(test_y) - test_y_hat) ** 2))

# Multiple Linear Regression

%reset -f

import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt

%matplotlib qt

x = np.arange(-5,5,.1)
y= 2*x+3
y_noise = 2*np.random.normal(size = x.size)
y_data = y + y_noise
plt.plot(y_data,'bo')
plt.plot(y,color='r')

# Download dataset

URL = "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-ML0101EN-Coursera/labs/Data_files/china_gdp.csv"

df = pd.read_csv(URL)

# Explore dataset

df.describe()
df.plot.scatter('Year','Value')

# Build model

def sigmoid(x, beta1, beta2):
    y = 1 / (1 + np.exp(-beta1 * (x - beta2)))
    return y

x_norm = df.Year / df.Year.max()
y_norm = df.Value / df.Value.max()

from scipy.optimize import curve_fit
popt, pcov = curve_fit(sigmoid, x_norm, y_norm)
beta1 = popt[0]
beta2 = popt[1]

y_hat = sigmoid(x_norm,beta1,beta2)

plt.plot(x_norm, y_hat, color='red')
plt.plot(x_norm, y_norm, 'bo')

# %%
# =============================================================================
# COURSE 4 - MODULE 3 - CLASSIFICATION
# =============================================================================

# KNN

%reset -f

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import NullFormatter
import matplotlib.ticker as ticker
import itertools
from sklearn import preprocessing

%matplotlib qt

URL = "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-ML0101EN-Coursera/labs/Data_files/teleCust1000t.csv"

df = pd.read_csv(URL)

df.columns
df.dtypes
df.info()

df.custcat.value_counts()
df.custcat.pipe(type)

df.custcat.hist()
df.hist(column = "custcat")

X = df.drop("custcat", axis="columns").values
Y = df.custcat.values

from sklearn.model_selection import train_test_split
X_train, X_test, Y_train, Y_test = train_test_split(X,Y,test_size=.2)

scaler = preprocessing.StandardScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)

X_train.mean(axis = 0)
X_test.mean(axis = 0)

from sklearn.neighbors import KNeighborsClassifier
neighbors = KNeighborsClassifier(n_neighbors = 5).fit(X_train,Y_train)
neighbors.score(X_train,Y_train)
neighbors.score(X_test,Y_test)

from sklearn import metrics
metrics.accuracy_score(Y_test, neighbors.predict(X_test))
metrics.balanced_accuracy_score(Y_test, neighbors.predict(X_test))
metrics.confusion_matrix(Y_test, neighbors.predict(X_test))
metrics.log_loss(Y_test, neighbors.predict_proba(X_test))
print(metrics.classification_report(Y_test, neighbors.predict(X_test)))

from sklearn.model_selection import cross_val_score
scores = cross_val_score(KNeighborsClassifier(n_neighbors = 5), 
                         X_train, Y_train, cv = 10)
scores
scores.mean()
scores.std()

from sklearn.pipeline import make_pipeline
pl = make_pipeline(preprocessing.StandardScaler(), KNeighborsClassifier(n_neighbors = 5))
scores = cross_val_score(pl, X, Y)
scores
scores.mean()
scores.std()

for K in range(1,101):
    neighbors = KNeighborsClassifier(n_neighbors = K)
    neighbors.fit(X_train,Y_train)
    train_score = neighbors.score(X_train,Y_train)
    test_score = neighbors.score(X_test,Y_test)
    print("K: ",K," , train score: ",train_score," , test score: ", test_score)

from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import make_pipeline
pl = make_pipeline(preprocessing.StandardScaler(), KNeighborsClassifier())
clf = GridSearchCV(estimator = KNeighborsClassifier(),
                   param_grid = {'n_neighbors':list(np.array(range(1,101)))},
                   cv = 10)
clf.fit(X,Y)
clf.cv_results_
clf.cv_results_.keys()
score = clf.cv_results_['mean_test_score']
param = clf.cv_results_['param_n_neighbors'].data
plt.plot(param,score)

# Decision trees

import numpy as np
import pandas as pd
from sklearn.tree import DecisionTreeClassifier, plot_tree
import matplotlib.pyplot as plt

%matplotlib qt

URL = "https://s3-api.us-geo.objectstorage.softlayer.net/cf-courses-data/CognitiveClass/ML0101ENv3/labs/drug200.csv"
df = pd.read_csv(URL)

df.shape
df.columns
df.info()
df.Na_to_K
df.Na_to_K.plot()
df.Age.hist()
df.Sex.value_counts()
df.BP.hist()
df.Cholesterol.value_counts()
df.Na_to_K.describe()

X = df[['Age','Sex','BP','Cholesterol','Na_to_K']].values
Y = df.Drug.values

from sklearn.preprocessing import LabelEncoder
encoder_sex = LabelEncoder()
encoder_BP = LabelEncoder()
encoder_Cho = LabelEncoder()
X[:,1] = encoder_sex.fit_transform(X[:,1])
X[:,2] = encoder_BP.fit_transform(X[:,2])
X[:,3] = encoder_Cho.fit_transform(X[:,3])
encoder_sex.classes_
encoder_BP.classes_
encoder_Cho.classes_

from sklearn.model_selection import train_test_split
X_train,X_test,Y_train,Y_test = train_test_split(X,Y,test_size=.2)

tree = DecisionTreeClassifier(criterion="entropy")
tree.fit(X_train,Y_train)
plot_tree(tree)

Y_hat_test = tree.predict(X_test)

pd.DataFrame({'Y_hat':Y_hat_test, 'Y_actual':Y_test, 'result':(Y_hat_test==Y_test)})

from sklearn import metrics
metrics.accuracy_score(Y_test,Y_hat_test)
metrics.confusion_matrix(Y_test, Y_hat_test)

# Logistic Regression

%reset -f

import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
import scipy.optimize as opt
from sklearn import preprocessing

%matplotlib qt

URL = "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-ML0101EN-Coursera/labs/Data_files/ChurnData.csv"

churn = pd.read_csv(URL)

churn.columns
churn.shape
churn.info()
churn.describe().T

churn = churn[['tenure','age','address','income','ed','employ','equip','callcard','wireless','churn']]
churn.churn = churn.churn.astype('int')
churn.dtypes

X = churn.drop('churn',axis=1)
y = churn.churn

from sklearn.preprocessing import StandardScaler
X = StandardScaler().fit_transform(X)

from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=.2)

from sklearn.linear_model import LogisticRegression
LR = LogisticRegression(C=0.01,solver='liblinear').fit(X_train,y_train)
LR.get_params()

y_hat_train = LR.predict(X_train)
y_hat_test = LR.predict(X_test)

from sklearn.metrics import confusion_matrix, jaccard_score, classification_report, log_loss
confusion_matrix(y_test,y_hat_test)
jaccard_score(y_test,y_hat_test)
print(classification_report(y_test,y_hat_test))
log_loss(y_test, LR.predict_proba(X_test))

pd.DataFrame({'P(0)':LR.predict_proba(X_test)[:,0],
              'P(1)':LR.predict_proba(X_test)[:,1],
              'y':y_test,
              'correct':(y_test == y_hat_test)})

from sklearn.linear_model import LogisticRegressionCV
C = [0.01,0.02,0.03,0.05,0.07,0.1,0.2,0.3,0.5,0.7,1,2,3,5,7,10,20,30,50,70,100,200,300,500,700,1000,2000,3000,5000,7000,10000]
LRCV = LogisticRegressionCV(Cs=C,cv=4,solver='liblinear',scoring='neg_log_loss').fit(X_train,y_train)

dir(LRCV)
LRCV.scores_
LRCV.coef_
LRCV.C_

confusion_matrix(y_train,LRCV.predict(X_train))
confusion_matrix(y_train,LogisticRegression(C=0.01,solver='liblinear').fit(X_train,y_train).predict(X_train))
confusion_matrix(y_train,LogisticRegression(C=0.07,solver='liblinear').fit(X_train,y_train).predict(X_train))

confusion_matrix(y_test,LRCV.predict(X_test))
confusion_matrix(y_test,LR.predict(X_test))
confusion_matrix(y_test, LogisticRegressionCV(Cs=C,solver='liblinear',scoring="neg_log_loss").fit(X_train,y_train).predict(X_test))

LRCV.get_params()
LRCV.score(X_test,y_test)

# Support Vector Machines

%reset -f

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

%matplotlib qt

URL = "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-ML0101EN-Coursera/labs/Data_files/cell_samples.csv"

df = pd.read_csv(URL)

df.shape
df.columns
df.info()
df.describe(include='all').T

ax = df.loc[:,['Clump','UnifSize']][df.Class==2].plot.scatter('Clump','UnifSize',color='b',label='benign',alpha=.2)
df.loc[:,['Clump','UnifSize']][df.Class==4].plot.scatter('Clump','UnifSize',color='r',ax=ax,label='malignant',alpha=.2)

df.BareNuc.values
df.BareNuc.unique()
df[df.BareNuc == "?"].shape

pd.to_numeric(df.BareNuc,errors='coerce').notnull()

df.BareNuc = pd.to_numeric(df.BareNuc,errors='coerce')
df = df[df.BareNuc.notnull()]

X = df.drop(columns=['ID','Class'],axis=1).values
y = df.Class

from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=0.2)

from sklearn import svm
clf = svm.SVC(kernel = 'poly', degree=3,C=.1)
clf.fit(X_train,y_train)
y_hat_train = clf.predict(X_train)
y_hat_test = clf.predict(X_test)

from sklearn.metrics import confusion_matrix, classification_report, f1_score
confusion_matrix(y_test,y_hat_test)
f1_score(y_test,y_hat_test,average='weighted')
print(classification_report(y_test,y_hat_test))

# %%
# =============================================================================
# COURSE 4- MODULE 4 - CLUSTERING
# =============================================================================

# K-means clustering

%reset -f

import random
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans
from sklearn.datasets.samples_generator import make_blobs

%matplotlib qt

np.random.seed(0)

X,y = make_blobs(n_samples=5000,centers=[[4,4],[-2,-1],[2,-3],[1,1]],cluster_std=0.9)

k_means = KMeans(init='k-means++',n_clusters=4,n_init=12)
k_means.fit(X)
k_means_labels = k_means.labels_
k_means_cluster_centers = k_means.cluster_centers_

colors = plt.cm.Spectral(np.linspace(0, 1, len(set(k_means_labels))))
for i,clust in enumerate(colors): plt.plot(X[y==i,0],X[y==i,1],'bo',color=colors[i])

URL = "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-ML0101EN-Coursera/labs/Data_files/Cust_Segmentation.csv"

df = pd.read_csv(URL)
df
df.columns
df.index = df.loc[:,'Customer Id']
df.drop('Customer Id',axis=1,inplace=True)
df.info()
df.drop('Address',axis=1,inplace=True)
df.info()

from sklearn.preprocessing import StandardScaler
X = df.values
X = np.nan_to_num(X)
X = StandardScaler().fit_transform(X)
X.std(axis=0)
X.mean(axis=0)

k_means = KMeans(init='k-means++',n_clusters=3,n_init=12)
k_means.fit(X)
labels = k_means.labels_
df['Cluster'] = labels

df.groupby('Cluster').mean().T

plt.scatter(X[:,0],X[:,3], c = labels.astype(np.float), alpha = .5)

# Hierarchical clustering

%reset -f

import numpy as np
import pandas as pd
from sklearn.datasets import make_blobs
import matplotlib.pyplot as plt

%matplotlib qt

X1,y1 = make_blobs(n_samples=50, centers=[[4,4],[-2,-1],[1,1],[10,4]],cluster_std=0.9)
plt.figure()
for cl in np.unique(y1): plt.scatter(X1[y1==cl,0],X1[y1==cl,1],s=100,marker=('$'+str(cl)+'$'))

from sklearn.cluster import AgglomerativeClustering

agg = AgglomerativeClustering(n_clusters = 4, linkage = 'complete')
clust = agg.fit_predict(X1)

colors = ['blue','red','green','black']
colors = plt.cm.Spectral(np.linspace(0, 1, len(set(clust))))
plt.figure()
for cl_or in np.unique(y1): 
    for cl_cl in np.unique(clust):
        plt.scatter(X1[(y1==cl_or) * (clust==cl_cl),0],
                    X1[(y1==cl_or) * (clust==cl_cl),1],
                    s=100,
                    marker='$'+str(cl_or)+'$',
                    color=colors[cl_cl])

from scipy.spatial import distance_matrix
dist = distance_matrix(X1,X1)

from scipy.cluster import hierarchy
Z = hierarchy.linkage(dist,'complete')
hierarchy.dendrogram(Z)

# Hierarchical clustering - cars dataset

URL = "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-ML0101EN-Coursera/labs/Data_files/cars_clus.csv"

cars = pd.read_csv(URL)

cars
cars.info()
cars.partition.describe()
cars.partition.value_counts()
cars.describe(include='all').T
cars.sales
cars.fuel_cap
cars.lnsales

cars_reduced = cars.drop(['manufact','model'],axis=1).apply(pd.to_numeric, errors='coerce').dropna()
cars_reduced.shape
cars_reduced.info()

X = cars_reduced[['engine_s','horsepow','wheelbas','width','length','curb_wgt','fuel_cap','mpg']]

from sklearn.preprocessing import MinMaxScaler
X = MinMaxScaler().fit_transform(X)
pd.DataFrame(X).describe()

from scipy.spatial import distance_matrix
dist = distance_matrix(X,X)

import scipy.cluster.hierarchy
clust = hierarchy.linkage(dist,'complete')
hierarchy.dendrogram(clust)
clusters = hierarchy.fcluster(clust,3,criterion='distance')

import pylab

def llf(id): return '[%s %s %s]' % (cars['manufact'][id], cars['model'][id], int(float(cars['type'][id])) )
    
dendro = hierarchy.dendrogram(clust,  leaf_label_func=llf, leaf_rotation=0, leaf_font_size =12, orientation = 'right')

from sklearn.cluster import AgglomerativeClustering
aggcl = AgglomerativeClustering(n_clusters=6,linkage='complete')
aggcl.fit_predict(X)
labels = aggcl.labels_
labels

import matplotlib.cm as cm
colors = cm.rainbow(np.linspace(0,1,max(labels)+1))
cluster_labels = list(np.unique(labels))

plt.figure(figsize=(16,14))

for color,label in zip(colors,cluster_labels):
    subset = cars_reduced[labels == label]
    for i in subset.index:
        plt.text(subset.horsepow[i],subset.mpg[i],str(subset.model[i]),rotation=25)
    plt.scatter(subset.horsepow,subset.mpg,s=subset.price*10,c=color,label='cluster'+str(label),alpha=.5)
    
# DBSCAN    

%reset -f    

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.datasets import make_blobs
from sklearn.preprocessing import StandardScaler

X,y = make_blobs(n_samples = 1500,
                 centers = [[4,3],[2,-1],[-1,4]],
                 cluster_std = 0.5)

X = StandardScaler().fit_transform(X)

from sklearn.cluster import DBSCAN
db = DBSCAN(eps = 0.3, min_samples = 7).fit(X)
labels = db.labels_

db.core_sample_indices_
core_samples_mask = [i in db.core_sample_indices_ for i in np.arange(0,len(X))]
outlier_samples_mask = (labels == -1)
border_samples_mask = ~(core_samples_mask | outlier_samples_mask)
np.sum(core_samples_mask)
np.sum(outlier_samples_mask)
np.sum(border_samples_mask)

color = {-1:'black',0:'blue',1:'red',2:'green'}
color_mask = [color[lab] for lab in labels]
size_mask = [100*s+40 for s in core_samples_mask]
plt.scatter(X[:,0],X[:,1],s=size_mask,c=color_mask,alpha=0.5)

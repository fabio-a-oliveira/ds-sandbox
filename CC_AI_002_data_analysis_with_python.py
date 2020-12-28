# %%
# =============================================================================
# COURSE 2 - MODULE 1 - IMPORTING DATA
# =============================================================================

import pandas as pd
import numpy as np

URL_data = "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-DA0101EN-SkillsNetwork/labs/Data%20files/auto.csv"

df = pd.read_csv(URL_data, header = None)

cols = ("symboling","normalized-losses","make","fuel-type","aspiration",
        "num-of-doors","body-style","drive-wheels","engine-location",
        "wheel-base","length","width","height","curb-weight",
        "engine-type","num-of-cylinders","engine-size","fuel-system",
        "bore","stroke","compression-ratio","horsepower",
        "peak-rpm","city-mpg","highway-mpg","price")

df.columns = cols

df = df.replace("?",np.NaN)
df.dropna(subset = ["price"], inplace = True)

df.columns
df.to_excel("autos.xlsx")

df.dtypes
df.describe()
df.describe(include = "all")
df[["length","compression-ratio"]].describe()
df.info()

# %%
# =============================================================================
# COURSE 2 - MODULE 2 - CLEANING AND PREPARING DATA
# =============================================================================

pd.get_dummies(df["make"])

import matplotlib.pylab as plt

filename = "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-DA0101EN-SkillsNetwork/labs/Data%20files/auto.csv"

headers = ["symboling","normalized-losses","make","fuel-type","aspiration", "num-of-doors","body-style",
         "drive-wheels","engine-location","wheel-base", "length","width","height","curb-weight","engine-type",
         "num-of-cylinders", "engine-size","fuel-system","bore","stroke","compression-ratio","horsepower",
         "peak-rpm","city-mpg","highway-mpg","price"]

df = pd.read_csv(filename, names = headers)

df.replace("?", np.NaN, inplace = True)

for column in df.columns.values.tolist():
    print(column)
    print(df[column].isna().value_counts())
    print("")

for column in ["normalized-losses","stroke","bore","horsepower","peak-rpm"]:
    df[column][df[column].isna()] = df[column][df[column].isna()==False].astype("float").mean()

df["num-of-doors"][df["num-of-doors"].isna()] = 4

df.dropna(axis=0, subset = ["price"], inplace = True)

df.reset_index(drop = True, inplace = True)

df.dtypes

df["bore"]
df[["bore","stroke","price","peak-rpm"]] = df[["bore","stroke","price","peak-rpm"]].astype("float")
df[["normalized-losses"]] = df[["normalized-losses"]].astype("int")

df["city-L/100km"] = 235/df["city-mpg"]
df["highway-L/100km"] = 235/df["highway-mpg"]

for column in ["wheel-base","length","width","height"]:
    df[column] = df[column]/df[column].max()

df = df.astype({"horsepower":"int"})

import matplotlib as plt
from matplotlib import pyplot
%matplotlib inline

plt.pyplot.hist(df["horsepower"])
plt.pyplot.xlabel("horsepower")
plt.pyplot.ylabel("count")
plt.pyplot.title("horsepower bins")

bins = np.linspace(df["horsepower"].min(),
                   df["horsepower"].max(),
                   4)

names = ["Low","Medium","High"]

df["binned-horsepower"] = pd.cut(df["horsepower"],
                                 bins,
                                 labels = names,
                                 include_lowest = True)

df["binned-horsepower"]
df["binned-horsepower"].value_counts()

plt.pyplot.hist(df["binned-horsepower"])

pd.get_dummies(df["fuel-type"])

df = pd.concat([df, pd.get_dummies(df["fuel-type"])], axis = 1)
df.drop("fuel-type", axis = 1, inplace = True)
df.columns

df = pd.concat([df, pd.get_dummies(df["aspiration"])], axis = 1)
df.drop("aspiration", axis = 1, inplace = True)
df.columns

df.to_csv("clean_df.csv")

# %%
# =============================================================================
# COURSE 2 - MODULE 3 - EXPLORATORY DATA ANALYSIS
# =============================================================================

import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
%matplotlib inline

from scipy import stats



filepath = 'https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-DA0101EN-SkillsNetwork/labs/Data%20files/automobileEDA.csv'
df = pd.read_csv(filepath)

df.head()
df.dtypes
df[['bore','compression-ratio','horsepower']].corr()

sns.regplot(x = 'engine-size', y = 'price', data = df), 
plt.ylim(0,),
plt.title("price vs engine size")

df[['engine-size','price']].corr()

sns.regplot(x = 'highway-mpg', y = 'price', data = df), 
plt.title("price vs highway-mpg")
df[['highway-mpg','price']].corr()

df[['stroke','price']].corr()
sns.regplot(x = 'stroke', y = 'price', data = df)

sns.boxplot(x = 'body-style', y = 'price', data = df)

sns.boxplot(x = 'drive-wheels', y = 'price', data = df)

df.describe()
df.describe(include=['object'])

df['drive-wheels'].value_counts().to_frame()

drive_wheels_counts = df['drive-wheels'].value_counts().to_frame()
drive_wheels_counts.rename(columns={'drive-wheels':'counts'},inplace=True)
drive_wheels_counts.index.name = 'drive-wheels'
drive_wheels_counts

eng_location_counts = df['engine-location'].value_counts().to_frame()
eng_location_counts.rename(columns = {'engine-location':'counts'},inplace=True)
eng_location_counts.index.name = 'engine-location'
eng_location_counts

df['drive-wheels'].unique()

df_group_one = df[['drive-wheels','body-style','price']]
df_group_one = df_group_one.groupby(['drive-wheels'], as_index=False).mean()

df_group_two = df[['drive-wheels','body-style','price']]
df_group_two = df_group_two.groupby(['drive-wheels','body-style'], as_index=False).mean()
grouped_pivot = df_group_two.pivot(index='drive-wheels',columns='body-style').fillna(0)

df_group_body_style = df[['body-style','price']]
df_group_body_style = df_group_body_style.groupby(['body-style'], as_index=False).mean()

plt.pcolor(grouped_pivot,cmap="RdBu"),
plt.colorbar(),
plt.show()

fig, ax = plt.subplots()
im = ax.pcolor(grouped_pivot, cmap='RdBu')
row_labels = grouped_pivot.columns.levels[1]
col_labels = grouped_pivot.index

ax.set_xticks(np.arange(grouped_pivot.shape[1])+0.5, minor=False)
ax.set_yticks(np.arange(grouped_pivot.shape[0])+0.5, minor=False)

ax.set_xticklabels(row_labels, minor=False)
ax.set_yticklabels(col_labels, minor=False)

plt.xticks(rotation=90)

fig.colorbar(im)
plt.show()

df.corr()

pearson_coef, p_value = stats.pearsonr(df['wheel-base'],df['price'])

for col in ['wheel-base','horsepower','length','width','curb-weight']:
    print(col)
    pearson_coef, p_value = stats.pearsonr(df[col],df['price'])
    print('coef:',pearson_coef,'/ p-value:',p_value,'\n')

dir(df[['drive-wheels','price']].groupby(['drive-wheels']))
df[['drive-wheels','price']].groupby(['drive-wheels']).groups
df[['drive-wheels','price']].groupby(['drive-wheels']).get_group('4wd')
grouped_df = df[['drive-wheels','price']].groupby(['drive-wheels'])

stats.f_oneway(grouped_df.get_group('4wd')['price'],
               grouped_df.get_group('fwd')['price'],
               grouped_df.get_group('rwd')['price'])

stats.f_oneway(grouped_df.get_group('4wd')['price'],
               grouped_df.get_group('fwd')['price'])

stats.f_oneway(grouped_df.get_group('4wd')['price'],
               grouped_df.get_group('rwd')['price'])

stats.f_oneway(grouped_df.get_group('fwd')['price'],
               grouped_df.get_group('rwd')['price'])

# %%
# =============================================================================
# COURSE 2- MODULE 4 - MODEL DEVELOPMENT
# =============================================================================

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

path = 'https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-DA0101EN-SkillsNetwork/labs/Data%20files/automobileEDA.csv'
df = pd.read_csv(path)

from sklearn.linear_model import LinearRegression

lm = LinearRegression()

lm.fit(df[['highway-mpg']],df['price'])

lm.intercept_
lm.coef_
lm.score(df[['highway-mpg']],df['price'])

sns.regplot(x=df['highway-mpg'],y=df['price'])

y_hat = lm.predict(df[['highway-mpg']])

plt.scatter(df['highway-mpg'],y_hat)

lm_eng = LinearRegression()
lm_eng.fit(df[['engine-size']],df['price'])
lm_eng.intercept_
lm_eng.coef_
sns.regplot(x = df['engine-size'], y = df['price'])

X1 = df[['horsepower','curb-weight','engine-size','highway-mpg']]
y = df['price']

lm_multi1 = LinearRegression()
lm_multi1.fit(X1,y)
lm_multi1.intercept_
lm_multi1.coef_

X2 = df[['normalized-losses','highway-mpg']]
lm_multi2 = LinearRegression()
lm_multi2.fit(X2,y)
lm_multi2.intercept_
lm_multi2.coef_

lm_multi1.score(X1,y)
lm_multi2.score(X2,y)

sns.regplot(x = 'highway-mpg', y = 'price', data = df)
sns.regplot(x = 'peak-rpm', y = 'price', data = df)

df[['highway-mpg','peak-rpm','price']].corr()

sns.residplot(x=df['highway-mpg'],y=df['price'],lowess=True)

lm.fit(X1,y)
dir(lm)
lm._decision_function(X1)
lm.get_params(True)
lm._get_tags()

y_hat = lm.predict(X1)

def PlotPolly(model, independent_variable, dependent_variabble, Name):
    x_new = np.linspace(15, 55, 100)
    y_new = model(x_new)

    plt.plot(independent_variable, dependent_variabble, '.', x_new, y_new, '-')
    plt.title('Polynomial Fit with Matplotlib for Price ~ Length')
    ax = plt.gca()
    ax.set_facecolor((0.898, 0.898, 0.898))
    fig = plt.gcf()
    plt.xlabel(Name)
    plt.ylabel('Price of Cars')

    plt.show()
    plt.close()

x = df['highway-mpg']
y = df['price']

f = np.polyfit(x,y,3)
p = np.poly1d(f)
print(p)
PlotPolly(p, x, y, 'highway-mpg')

f = np.polyfit(x,y,10)
p = np.poly1d(f)
print(p)
PlotPolly(p, x, y, 'highway-mpg')

from sklearn.preprocessing import PolynomialFeatures

pr = PolynomialFeatures(degree=2)
pr

Z = df[['horsepower','curb-weight','engine-size','highway-mpg']]
Z_pr = pr.fit_transform(Z)

Z
Z_pr.shape

lm.fit(Z,y)
lm.score(Z,y)

lm.fit(Z_pr,y)
lm.score(Z_pr,y)

from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

Input = [('scale',StandardScaler()),
         ('polynomial',PolynomialFeatures(include_bias=False)),
         ('model',LinearRegression())]

pipe = Pipeline(Input)
pipe

pipe.fit(Z,y)

ypipe = pipe.predict(Z)
ypipe

Input = [('scale',StandardScaler()),
         ('linear_regression', LinearRegression())]

pipe = Pipeline(Input)
pipe.fit(Z,y)

ypipe = pipe.predict(Z)
ypipe

lm = LinearRegression()
x = df[['horsepower']]
y = df['price']
lm.fit(df[['horsepower']],df['price'])
lm.score(df[['horsepower']],df['price'])

y_hat = lm.predict(x)

from sklearn.metrics import mean_squared_error

mse = mean_squared_error(y,y_hat)

import matplotlib.pyplot as plt
import numpy as np

%matplotlib inline

new_input = np.arange(1,100,1).reshape(-1,1)

X = df[['highway-mpg']]
y = df['price']
lm.fit(X,y)
y_hat = lm.predict(new_input)
plt.plot(new_input,y_hat)

# %%
# =============================================================================
#  COURSE 2 - MODULE 5 - MODEL EVALUATION
# =============================================================================

# Housekeeping

import numpy as np
import pandas as pd

%%capture
! pip install ipywidgets

from ipywidgets import interact, interactive, fixed, interact_manual

path = 'https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-DA0101EN-SkillsNetwork/labs/Data%20files/module_5_auto.csv'

df = (pd.read_csv(path)
        ._get_numeric_data())

def DistributionPlot(RedFunction, BlueFunction, RedName, BlueName, Title):
    width = 12
    height = 10
    plt.figure(figsize=(width, height))

    ax1 = sns.distplot(RedFunction, hist=False, color="r", label=RedName)
    ax2 = sns.distplot(BlueFunction, hist=False, color="b", label=BlueName, ax=ax1)

    plt.title(Title)
    plt.xlabel('Price (in dollars)')
    plt.ylabel('Proportion of Cars')

    plt.show()
    plt.close()

def PollyPlot(xtrain, xtest, y_train, y_test, lr,poly_transform):
    width = 12
    height = 10
    plt.figure(figsize=(width, height))
    
    
    #training data 
    #testing data 
    # lr:  linear regression object 
    #poly_transform:  polynomial transformation object 
 
    xmax=max([xtrain.values.max(), xtest.values.max()])

    xmin=min([xtrain.values.min(), xtest.values.min()])

    x=np.arange(xmin, xmax, 0.1)

    plt.plot(xtrain, y_train, 'ro', label='Training Data')
    plt.plot(xtest, y_test, 'go', label='Test Data')
    plt.plot(x, lr.predict(poly_transform.fit_transform(x.reshape(-1, 1))), label='Predicted Function')
    plt.ylim([-10000, 60000])
    plt.ylabel('Price')
    plt.legend()

# Training and testing

y_data = df['price']
x_data = df.drop('price',axis=1)

from sklearn.model_selection import train_test_split

x_train, x_test, y_train, y_test = train_test_split(x_data,y_data,test_size=0.1,random_state=1)

x_test
x_train

x_train_1, x_test_1, y_train_1, y_test_1 = train_test_split(x_data,y_data,test_size=0.40,random_state=0)

x_train_1
x_test_1

from sklearn.linear_model import LinearRegression

lre = LinearRegression()

lre.fit(x_train[['horsepower']],y_train)
lre.score(x_train[['horsepower']],y_train)
lre.score(x_test[['horsepower']],y_test)

from sklearn.model_selection import cross_val_score

Rcross = cross_val_score(lre, x_data[['horsepower']], y_data, cv=4)
Rcross
Rcross.mean()
Rcross.std()

Rcross = cross_val_score(lre, x_data[['horsepower']], y_data, cv=4, scoring = 'neg_mean_squared_error')
Rcross
Rcross.mean()
Rcross.std()

Rcross = cross_val_score(lre, x_data[['horsepower']], y_data, cv = 2)
Rcross.mean()
Rcross.std()

from sklearn.model_selection import cross_val_predict

y_hat = cross_val_predict(lre, x_data[['horsepower']], y_data, cv = 4)

# Model selection

lr = LinearRegression()
lr.fit(x_train[['horsepower','curb-weight','engine-size','highway-mpg']], y_train)
y_hat_train = lr.predict(x_train[['horsepower','curb-weight','engine-size','highway-mpg']])
y_hat_test = lr.predict(x_test[['horsepower','curb-weight','engine-size','highway-mpg']])

import matplotlib.pyplot as plt
%matplotlib inline
import seaborn as sns

Title = 'Distribution  Plot of  Predicted Value Using Training Data vs Training Data Distribution'
DistributionPlot(y_train, y_hat_train, "Actual Values (Train)", "Predicted Values (Train)", Title)

Title='Distribution  Plot of  Predicted Value Using Test Data vs Data Distribution of Test Data'
DistributionPlot(y_test,y_hat_test,"Actual Values (Test)","Predicted Values (Test)",Title)

from sklearn.preprocessing import PolynomialFeatures

x_train,x_test,y_train,y_test = train_test_split(x_data,y_data,test_size=0.45, random_state=0)

pr = PolynomialFeatures(degree=5)
x_train_pr = pr.fit_transform(x_train[['horsepower']])
x_test_pr = pr.fit_transform(x_test[['horsepower']])
x_train_pr.shape

poly = LinearRegression()
poly.fit(x_train_pr, y_train)
y_hat = poly.predict(x_test_pr)

PollyPlot(x_train[['horsepower']], x_test[['horsepower']], y_train, y_test, poly, pr)

poly.score(x_train_pr, y_train)
poly.score(x_test_pr, y_test)

R2 = []
order = [1, 2, 3, 4]

for n in order:
    pr = PolynomialFeatures(degree = n)
    x_train_pr = pr.fit_transform(x_train[['horsepower']])
    x_test_pr = pr.fit_transform(x_test[['horsepower']])
    lr.fit(x_train_pr,y_train)
    R2.append(lr.score(x_test_pr,y_test))
    
plt.plot(order,R2)

def f(order, test_data):
    x_train, x_test, y_train, y_test = train_test_split(x_data, y_data, test_size=test_data, random_state=0)
    pr = PolynomialFeatures(degree=order)
    x_train_pr = pr.fit_transform(x_train[['horsepower']])
    x_test_pr = pr.fit_transform(x_test[['horsepower']])
    poly = LinearRegression()
    poly.fit(x_train_pr,y_train)
    PollyPlot(x_train[['horsepower']], x_test[['horsepower']], y_train,y_test, poly, pr)

interact(f, order=(0, 6, 1), test_data=(0.05, 0.95, 0.05))

pf = PolynomialFeatures(degree=2)
x_train = pf.fit_transform(x_train[['horsepower','curb-weight','engine-size','highway-mpg']])
x_test = pf.fit_transform(x_test[['horsepower','curb-weight','engine-size','highway-mpg']])
poly = LinearRegression()
poly.fit(x_train,y_train)
poly.score(x_train,y_train)
poly.score(x_test,y_test)
poly.predict(x_test)
DistributionPlot(y_test, poly.predict(x_test), "true price", "prediction", "distribution plot")

# Ridge regression

pr = PolynomialFeatures(degree=2)

x_train,x_test,y_train,y_test = train_test_split(x_data,y_data,test_size=0.20)
x_train.shape
x_test.shape
y_train.shape
y_test.shape

x_train_pr = pr.fit_transform(x_train[['horsepower','curb-weight','engine-size','highway-mpg','normalized-losses','symboling']])
x_test_pr = pr.fit_transform(x_test[['horsepower','curb-weight','engine-size','highway-mpg','normalized-losses','symboling']])
x_train_pr.shape
x_test_pr.shape

from sklearn.linear_model import Ridge

RidgeModel = Ridge(alpha = 0.1)
RidgeModel.fit(x_train_pr,y_train)

y_hat = RidgeModel.predict(x_test_pr)
y_hat[0:4]
y_test[0:4].values

R2_train = []
R2_test = []
Alpha = np.array(range(0,50))

for alpha in Alpha:
    RidgeModel = Ridge(alpha = alpha)
    RidgeModel.fit(x_train_pr, y_train)
    R2_train.append(RidgeModel.score(x_train_pr, y_train))
    R2_test.append(RidgeModel.score(x_test_pr, y_test))

plt.plot(Alpha, R2_train), plt.plot(Alpha, R2_test)

# Grid search

from sklearn.model_selection import GridSearchCV

parameters1 = [{'alpha':[0.001,0.1,1,10,100,1000,10000,100000,1000000]}]
RR = Ridge()
grid1 = GridSearchCV(RR, parameters1, cv=4)
grid1.fit(x_data[['horsepower','curb-weight','engine-size','highway-mpg']], y_data)
grid1.best_estimator_.score(x_test[['horsepower','curb-weight','engine-size','highway-mpg']], y_test)

parameters2 = {'alpha':[0.001,0.01,0.1,1,10,100,1000,10000,100000,1000000], 'normalize':[True,False]}
RR = Ridge()
grid2 = GridSearchCV(RR, parameters2, cv=4)
grid2.fit(x_data[['horsepower','curb-weight','engine-size','highway-mpg']], y_data)
grid2.best_estimator_
grid2.cv_results_
grid2.best_score_

# -*- coding: utf-8 -*-
"""
Sandbox script used to go through the courses for the Python Data Science program
"""

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

# %%
# =============================================================================
# COURSE 3 - MODULE 1 - INTRO TO VISUALIZATION TOOLS
# =============================================================================

%reset -f

import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
%matplotlib inline

URL = "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-DV0101EN-SkillsNetwork/Data%20Files/Canada.xlsx"
sheet = "Canada by Citizenship"

df = pd.read_excel(URL, 
                   sheet_name = sheet,
                   skiprows = range(20),
                   skipfooter = 2)

df
df.columns
df.info
df.describe
df.index.values
df.columns.tolist()
df.index.tolist()
df.shape
df.dtypes

df.drop(['AREA','REG','DEV','Type','Coverage'], axis = 1, inplace = True)
df.columns.tolist()

df.rename(columns = {'OdName':'Country',
                     'AreaName':'Continent',
                     'RegName':'Region'},
          inplace = True)

df['Total'] = df.sum(axis = 1)

df.isnull().sum()
df['Total']

df.describe()

df[df['Country'] == 'Brazil']

df.set_index(df['Country'], inplace = True)
df.index.values
df.loc['Brazil']
df

df.loc['Brazil']
df.loc['Brazil']

df.columns = list(map(str,df.columns))
years = list(map(str,range(1980,2014)))

plt.style.available
mpl.style.use(['ggplot'])

(df.loc["Haiti",years].plot(kind='line'), 
    plt.title('immigration from Haiti'),
    plt.xlabel('year'),
    plt.ylabel('total'),
    plt.text(20,5000, "2010 earthquake"))

df_ch_in = df.loc[["China","India"],years]
df_ch_in

(df_ch_in.T.plot(kind='line'),
 plt.title('immigration from China & India'),
 plt.xlabel('year'),
 plt.ylabel('# of immigrants'))

df_top5 = df.nlargest(5,'Total')
df_top5
df_top5[years].T.plot(kind='line')


# %%
# =============================================================================
# COURSE 3 - MODULE 2 - BASIC VISUALIZATION TOOLS
# =============================================================================

%reset -f

import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
%matplotlib inline

df_can = pd.read_excel('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-DV0101EN-SkillsNetwork/Data%20Files/Canada.xlsx',
                       sheet_name='Canada by Citizenship',
                       skiprows=range(20),
                       skipfooter=2)

df_can.rename(columns = {'OdName':'Country','AreaName':'Continent','RegName':'Region'}, inplace=True)
df_can.drop(['AREA','REG','DEV','Type','Coverage'],axis=1,inplace=True)
df_can.columns = list(map(str, df_can.columns))
df_can.set_index('Country',inplace=True)
df_can["Total"] = df_can.sum(axis=1)

years = list(map(str,range(1980,2014)))
df_top5 = df_can.sort_values(by = 'Total',ascending=False)[0:5]
df_top5 = df_top5[years].T
df_top5.index.name = 'year'
df_top5.columns = ['India','China','UK','Philippines','Pakistan']

# plots with scripting layer

(df_top5.plot(kind='area',
              figsize=(10,5)),
 plt.title('immigration from top 5 countries'),
 plt.xlabel('year'),
 plt.ylabel('# of immigrants'))

df_top5.plot(kind='area',stacked=False)

# plots with artist layer

ax = df_top5.plot(kind='area',alpha=.7,figsize=(10,5))
ax.set_title('immigration from top 5 countries')
ax.set_ylabel('# of immigrants')
ax.set_xlabel('years')

df_2013 = df_can['2013']
count, bin_edges = np.histogram(df_2013, bins = 5)
df_2013.plot(kind='hist',xticks=bin_edges)
    
df_can.loc[['Denmark','Norway','Sweden'],years].T.plot.area()    
df_can.loc[['Denmark','Norway','Sweden'],years].T.plot.hist(stacked=True,alpha=.8)    
df_can.loc[['Denmark','Norway','Sweden'],years].T.plot.bar()    

ax = df_can.loc[['Greece','Albania','Bulgaria'],years].T.plot.area()
ax.set_title('immigration from baltic countries')  
    
df_can.loc[['Greece','Albania','Bulgaria'],years].T.plot.barh(stacked=True,figsize=(5,10))
    
(df_can.loc['Iceland',years].plot.bar(figsize=(10,10)),
 plt.title("immigration from Iceland"),
 plt.xlabel('year'),
 plt.ylabel('# of immigrants'),
 plt.annotate('',
              xy=(32,70),
              xytext=(28,20),
              xycoords='data',
              arrowprops=dict(arrowstyle='->',connectionstyle='arc3',color='blue',lw=2)),
 plt.annotate('2008-2011 finantial crisis',
              xy=(28,30),
              rotation=80,
              va='bottom',
              ha='left'))

df_top15 = df_can.sort_values('Total',ascending=False)[['Total']][0:15].plot.barh()

# %%
# =============================================================================
# COURSE 3 - MODULE 3 - SPECIALIZED VISUALIZATION TOOLS
# =============================================================================

%reset -f

import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt

%matplotlib inline

df = pd.read_excel('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-DV0101EN-SkillsNetwork/Data%20Files/Canada.xlsx',
                   sheet_name='Canada by Citizenship',
                   skiprows=range(20),
                   skipfooter=2
                  )

df.drop(['AREA','REG','DEV','Type','Coverage'], axis=1, inplace=True)
df.rename(columns = {'OdName':'Country', 'AreaName':'Continent','RegName':'Region'}, inplace=True)
df.columns = list(map(str, df.columns))
df.index = df['Country']
df.index.name = 'Country'
df['Total'] = df.sum(axis=1)

(df.groupby('Continent').sum()['Total'].plot(kind='pie',
                                             autopct='%1.1f%%',
                                             startangle=90,
                                             shadow=True,
                                             labels=None,
                                             figsize=(15,12),
                                             pctdistance=1.12,
                                             explode=[0.1,0,0,0,0.1,0.1]),
 plt.title('immigration by continent',  y=1.05),
 plt.axis('equal'),
 plt.legend(loc='lower left'))

years = list(map(str,range(1980,2014)))
df_japan = df.loc[['Japan'],years].T

type(df_japan)
df_japan.describe()

df_japan.plot(kind='box')

df_ChIn = df.loc[['China','India'],years].T
df_ChIn.describe()
df_ChIn.plot(kind='box')
df_ChIn.plot(kind='box',vert=False,color='red')

fig = plt.figure()
ax0 = fig.add_subplot(1,2,1)
ax1 = fig.add_subplot(1,2,2)

df_ChIn.plot(kind='box',vert=False,ax=ax0)
df_ChIn.plot(kind='line',ax=ax1)
plt.show()

decade_80s = list(map(str,range(1980,1990)))
decade_90s = list(map(str,range(1990,2000)))
decade_00s = list(map(str,range(2000,2010)))

df['Total_80s'] = df.loc[:,decade_80s].sum(axis=1)
df['Total_90s'] = df.loc[:,decade_90s].sum(axis=1)
df['Total_00s'] = df.loc[:,decade_00s].sum(axis=1)

(df[['Total','Total_80s','Total_90s','Total_00s']]
    .sort_values(by='Total',axis=0,ascending=False)[0:15]
    .drop('Total',axis=1)
    .plot
    .box(vert=False))

df_total = pd.DataFrame(df[years].sum(axis=0))
df_total.index = list(map(int,df_total.index))
df_total.index.name = 'Year'
df_total.reset_index(inplace=True)
df_total.rename(columns = {0:'Immigrants'},inplace=True)
df_total

df_total.plot.scatter(x='Year',y='Immigrants')

fit = np.polyfit(x = df_total['Year'],
                 y = df_total['Immigrants'],
                 deg = 1)

(df_total.plot.scatter(x='Year',y='Immigrants'),
 plt.plot(df_total['Year'], fit[1] + fit[0]*df_total['Year'], color='red'))

df_nordic = df.loc[['Norway','Sweden','Denmark','Finland','Iceland'],years].T
df_nordic['Total'] = df_nordic.sum(axis=1)
df_nordic.index.name = 'Year'
df_nordic.reset_index(inplace=True)
df_nordic = df_nordic.loc[:,['Year','Total']]
df_nordic.plot.scatter(x='Year',y='Total')

df_BrAr = df.loc[['Brazil','Argentina'],years].T
df_BrAr.index.name = 'Year'
df_BrAr.reset_index(inplace=True)
df_BrAr['norm_Brazil'] = (df_BrAr.Brazil - df_BrAr.Brazil.min()) / (df_BrAr.Brazil.max() - df_BrAr.Brazil.min())
df_BrAr['norm_Argentina'] = (df_BrAr.Argentina - df_BrAr.Argentina.min()) / (df_BrAr.Argentina.max() - df_BrAr.Argentina.min())

ax_br = df_BrAr.plot.scatter(x = 'Year',
                             y = 'Brazil',
                             s = 200*df_BrAr['norm_Brazil']+10,
                             alpha = 0.5, 
                             color = 'green')

ax_ar = df_BrAr.plot.scatter(x = 'Year',
                             y = 'Argentina',
                             s = 200*df_BrAr['norm_Argentina']+10,
                             alpha = 0.5, 
                             color = 'blue',
                             ax = ax_br)

ax_br.legend(['Brazil','Argentina'],loc='upper left')
dir(ax_br)


# %%
# =============================================================================
# COURSE 3 - MODULE 4 - ADVANCED VISUALIZATION TOOLS
# =============================================================================

%reset -f

import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from PIL import Image

df = pd.read_excel('https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-DV0101EN-SkillsNetwork/Data%20Files/Canada.xlsx',
                   sheet_name='Canada by Citizenship',
                   skiprows=range(20),
                   skipfooter=2)

df.drop(['AREA','REG','DEV','Type','Coverage'],axis=1,inplace=True)
df.rename(columns={'OdName':'Country','AreaName':'Continent','RegName':'Region'},inplace=True)
df.columns = list(map(str,df.columns))
df.set_index('Country',inplace=True)
df['Total'] = df.sum(axis=1)

years = list(map(str,range(1980,2014)))

# Waffle chart

df_scandinavia = df.loc[['Denmark','Norway','Sweden']]

total = df_scandinavia.Total.sum()
proportion = list(df_scandinavia.Total / total)

for i,prop in enumerate(proportion):
    print(df_scandinavia.index[i] + ' : ' + str(prop))

width = 40
height = 10
num_tiles = width * height
num_tiles_country = [round(proportion * num_tiles) for proportion in proportion]

waffle_chart = np.zeros((height,width))

category_index = 0
tile_index = 0

for col in range(width):
    for row in range(height):
        tile_index += 1
        if tile_index > sum(num_tiles_country[0:category_index]):
            category_index += 1
        waffle_chart[row,col] = category_index

fig = plt.figure()
colormap = plt.cm.coolwarm
plt.matshow(waffle_chart, cmap=colormap)
ax = plt.gca()
ax.set_xticks(np.arange(-.5,(width), 1), minor=True)
ax.set_yticks(np.arange(-.5,(height),1), minor=True)
ax.grid(which='minor', color='w', linestyle='-', linewidth=2)
plt.xticks([])
plt.yticks([])
values_cumsum = np.cumsum(df_scandinavia['Total'])
total_values = values_cumsum[len(values_cumsum) - 1]
legend_handles = []
for i, category in enumerate(df_scandinavia.index.values):
    label_str = category + ' (' + str(df_scandinavia['Total'][i]) + ')'
    color_val = colormap(float(values_cumsum[i])/total_values)
    legend_handles.append(mpatches.Patch(color=color_val, label=label_str))
plt.legend(handles = legend_handles,
           loc = 'lower center',
           ncol = len(df_scandinavia.index.values),
           bbox_to_anchor = (0., -0.2, 0.95, .1))

def create_waffle_chart(categories, values, height, width, colormap, value_sign=''):

    # compute the proportion of each category with respect to the total
    total_values = sum(values)
    category_proportions = [(float(value) / total_values) for value in values]

    # compute the total number of tiles
    total_num_tiles = width * height # total number of tiles
    
    # compute the number of tiles for each catagory
    tiles_per_category = [round(proportion * total_num_tiles) for proportion in category_proportions]
    
    # initialize the waffle chart as an empty matrix
    waffle_chart = np.zeros((height, width))

    # define indices to loop through waffle chart
    category_index = 0
    tile_index = 0

    # populate the waffle chart
    for col in range(width):
        for row in range(height):
            tile_index += 1

            # if the number of tiles populated for the current category 
            # is equal to its corresponding allocated tiles...
            if tile_index > sum(tiles_per_category[0:category_index]):
                # ...proceed to the next category
                category_index += 1       
            
            # set the class value to an integer, which increases with class
            waffle_chart[row, col] = category_index
    
    # instantiate a new figure object
    fig = plt.figure()

    # use matshow to display the waffle chart
    colormap = plt.cm.coolwarm
    plt.matshow(waffle_chart, cmap=colormap)
    plt.colorbar()

    # get the axis
    ax = plt.gca()

    # set minor ticks
    ax.set_xticks(np.arange(-.5, (width), 1), minor=True)
    ax.set_yticks(np.arange(-.5, (height), 1), minor=True)
    
    # add dridlines based on minor ticks
    ax.grid(which='minor', color='w', linestyle='-', linewidth=2)

    plt.xticks([])
    plt.yticks([])

    # compute cumulative sum of individual categories to match color schemes between chart and legend
    values_cumsum = np.cumsum(values)
    total_values = values_cumsum[len(values_cumsum) - 1]

    # create legend
    legend_handles = []
    for i, category in enumerate(categories):
        if value_sign == '%':
            label_str = category + ' (' + str(values[i]) + value_sign + ')'
        else:
            label_str = category + ' (' + value_sign + str(values[i]) + ')'
            
        color_val = colormap(float(values_cumsum[i])/total_values)
        legend_handles.append(mpatches.Patch(color=color_val, label=label_str))

    # add legend to chart
    plt.legend(
        handles=legend_handles,
        loc='lower center', 
        ncol=len(categories),
        bbox_to_anchor=(0., -0.2, 0.95, .1)
    )

create_waffle_chart(categories = df_scandinavia.index.values,
                    values = df_scandinavia['Total'],
                    height = 10,
                    width = 40,
                    colormap = plt.cm.coolwarm)

# Word clouds

%%capture
!pip install wordcloud

from wordcloud import WordCloud, STOPWORDS

import requests
URL = 'https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-DV0101EN-SkillsNetwork/Data%20Files/alice_novel.txt'
r = requests.get(URL)
alice = str(r.content)
alice = alice.replace('\\n','')
alice = alice.replace("\\\'s","")
alice = alice.replace("\\\'","")

stopwords = set(STOPWORDS)
for word in ['said','Ill','Ive','Im']: stopwords.add(word)

alice_wc = WordCloud(background_color = 'white',
                     max_words = 2000,
                     stopwords = stopwords)

alice_wc.generate(alice)

fig = plt.figure()
fig.set_figwidth(14)
fig.set_figheight(18)
plt.imshow(alice_wc, interpolation='bilinear')
plt.axis('off')
plt.show()

# Regression plot

import seaborn as sns

df_year = pd.DataFrame(df.loc[:,years].sum(axis = 0))
df_year.index = map(float,df_year.index)
df_year.reset_index(inplace=True)
df_year.columns = ['year','total']

%matplotlib inline

plt.figure(figsize=(15,10))
sns.set(font_scale=1.5)
sns.set_style('ticks')
ax = sns.regplot(x='year',
                 y='total',
                 data=df_year,
                 color='green',
                 marker='+', 
                 scatter_kws={'s':200})
ax.set(xlabel='Year',
       ylabel='Total Immigration',
       title='Total Immigration to Canada')

df_new = df_scandinavia.loc[:,years].T.reset_index().rename(columns = {'index':'year'})
df_new['year'] = [int(year) for year in df_new['year']]

ax_nor = sns.regplot(x = 'year',
                     y = 'Norway',
                     data = df_new)

ax_den = sns.regplot(x = 'year',
                     y = 'Denmark',
                     data = df_new,
                     ax = ax_nor)

ax_sw = sns.regplot(x = 'year',
                    y = 'Sweden',
                    data = df_new,
                    ax = ax_nor)

ax_nor.set(xlabel = 'year',
           ylabel = 'total immigrants',
           title = 'immigrants to Canada from Scandinavian countries')

ax_nor.legend(['Norway','Denmark','Sweden'], loc = 'upper right')


# %%
# =============================================================================
# COURSE 3 - MODULE 5 - MAPS AND GEOSPATIAL DATA
# =============================================================================

%reset -f

import numpy as np
import pandas as pd
import matplotlib as mpl


$$capture
!pip install folium

import folium

%matplotlib inline

world_map = folium.Map()
world_map



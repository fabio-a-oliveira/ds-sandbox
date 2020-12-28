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



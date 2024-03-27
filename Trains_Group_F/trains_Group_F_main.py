#!/usr/bin/env python
# coding: utf-8

# In[19]:



# In[20]:


import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestClassifier
from sklearn.pipeline import Pipeline
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split, GridSearchCV
from xgboost import XGBRegressor
from sklearn.metrics import mean_squared_error
from sklearn.decomposition import PCA
from sklearn.linear_model import Lasso



congestion_df = pd.read_csv("historicalCongestion.csv")
testDataConArr_df = pd.read_csv("testDataconarr.csv")
trainingDataConArr_df = pd.read_csv("trainingDataconarr.csv")
testDataTimings_df = pd.read_csv("testDatatimings.csv")
trainingDataTimings_df = pd.read_csv("trainingDatatimings.csv")

# In[21]:


def drop_first_column(df):

  return df.drop(df.columns[:1], axis=1, inplace=False)


def text_to_seconds(text_time):
  hours, minutes, seconds = map(int, text_time.split(":"))
  return hours * 3600 + minutes * 60 + seconds


# In[38]:






# In[45]:


# Ridge chart to see train delays based on the day of the week.
# It can be seen that in most cases, arrivals are early, but there may be more long delays on Tuesdays and Saturdays, but these are only a few cases.
# There will be very few demonstrations on Sunday. It may be that there will be much fewer trains on Sundays in the UK, and drivers will want to go home early.

# Define the order of days of the week

days_order = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']

trainingDataConArr_df['week.day'] = pd.Categorical(trainingDataConArr_df['week.day'], categories=days_order, ordered=True)


sns.set_theme(style="white", rc={"axes.facecolor": (0, 0, 0, 0), 'xtick.labelsize': 30})
plt.figure(figsize=(100, 60))
palette = sns.color_palette("husl", len(trainingDataConArr_df['week.day'].unique()))
g = sns.FacetGrid(trainingDataConArr_df, row='week.day', hue='week.day', aspect=15, height=4, palette=palette)
g.map(sns.kdeplot, 'delay.secs', clip_on=False, fill=True, alpha=0.7, lw=3, bw=.2)

def label(x, color, label):
    ax = plt.gca()

    ax.text(0, .2, label, fontweight="bold", color=color, ha="left", va="center", transform=ax.transAxes, fontsize=50)

g.map(label, "delay.secs")

g.fig.subplots_adjust(hspace=-.250)
g.set_titles("")
g.set(yticks=[])
g.despine(bottom=True, left=True)


plt.show()


# In[23]:


# Count the number of different values in the 'train.code' column
# trainingDataConArr_df['train_code'].nunique()
# unique_train_codes_count_test = testDataConArr_df['train_code'].nunique()

# trainingDataTimings_df.columns#Check column names
# print(trainingDataTimings_df.columns)#Check column names
#congestion_df.columns
# print(trainingDataTimings_df.dtypes)#Print the data type of each column


# In[24]:

# #Make Scatterplot Matrix scatter plot matrix
# sns.set_theme(style="ticks")

# # trainingDataTimings_df = sns.load_dataset("penguins")
# sns.pairplot(trainingDataTimisngs_df, hue="day.week")


# In[25]:

#Various functional functions
#Function used to convert the values of a column into numerical features.
def encode_discrete_values(df, column_name):
     # Get the unique value of the specified column and sort it (if it is non-numeric data, it will be sorted in lexicographic order)
     unique_values = sorted(df[column_name].unique())
     # Generate a mapping dictionary from these unique values to 1~n
     value_to_num = {value: i + 1 for i, value in enumerate(unique_values)}
    
     if column_name not in df.columns:
         print(f"Column '{column_name}' not found in DataFrame.")
         return None, None
    
     df[column_name + '_encoded'] = df[column_name].map(value_to_num)

     return df, value_to_num


# Used to convert day of the week to numeric type
def encode_weekdays(df, column_name):
    weekday_to_num = {
        'Monday': 1,
        'Tuesday': 2,
        'Wednesday': 3,
        'Thursday': 4,
        'Friday': 5,
        'Saturday': 6,
        'Sunday': 7,
    }
    
    if column_name not in df.columns:
        print(f"Column '{column_name}' not found in DataFrame.")
        return None, None
    
    df[column_name + '_encoded'] = df[column_name].map(weekday_to_num)
    return df, weekday_to_num

#Change station
def encode_common_discrete_values(df, columns):
     # Get the union of all unique values in two columns and sort them
     # unique_values = sorted(set(df[columns[0]].unique()).union(set(df[columns[1]].unique())))
     value_to_num = {
         'LEEDS': 1,
         'NORMNTN':2,
         'WKFLDKG':3,
         'WKFLDWG':4,
         'BNSLY':5,
         'MEADWHL':6,
         'SHEFFLD':7,
     }
     # Generate mapping dictionary
     # value_to_num = {value: i + 1 for i, value in enumerate(unique_values)}
    
     # Perform mapping conversion on the specified two columns
     for column in columns:
         if column not in df.columns:
             print(f"Column '{column}' not found in DataFrame.")
             continue
         df[column + '_encoded'] = df[column].map(value_to_num)
    
     # Return the updated DataFrame and mapping dictionary
     return df, value_to_num

# Calculate time difference
def calculate_time_difference(df, column1, column2, new_column_name='time_difference'):

    if column1 not in df.columns or column2 not in df.columns:
        raise ValueError(f"One or both of the specified columns '{column1}', '{column2}' do not exist in the DataFrame.")
    
    seconds1 = df[column1].apply(text_to_seconds)
    seconds2 = df[column2].apply(text_to_seconds)
    df[new_column_name] = (seconds1 - seconds2)
    
    return df
# Subtract two columns
def subtract_columns(df, col1, col2, new_col_name):
     df[new_col_name] = df[col1] - df[col2]
     return df

#Subtract all waiting stations
def subtract_columns_all(df, cols1, cols2, new_col_names):
     if not (len(cols1) == len(cols2) == len(new_col_names)):
         raise ValueError("The lists of columns and new column names must have the same length.")
     for col1, col2, new_col_name in zip(cols1, cols2, new_col_names):
         df[new_col_name] = df[col1] - df[col2]

     return df

# Average the two columns
def add_average_column(df, col1, col2, new_col_name):
     df[new_col_name] = (df[col1] + df[col2]) / 2
     return df

#Generate five column averages at the same time
def add_average_columns(df, cols1, cols2, new_col_names):
    if not (len(cols1) == len(cols2) == len(new_col_names)):
        raise ValueError("The lists of columns and new column names must have the same length.")
    for col1, col2, new_col_name in zip(cols1, cols2, new_col_names):
        df[new_col_name] = (df[col1] + df[col2]) / 2

    return df


# View all station combinations. This function has been implemented using the next function. You don’t need to use it.
def count_onehot_combinations(df, columns):
     subset = df[columns]
     unique_combinations = subset.drop_duplicates().reset_index(drop=True)
     return unique_combinations


#Statistics on station combinations, generate new columns, and classify station combinations
def add_combination_column_and_get_combinations(df, columns):

    subset = df[columns]
    unique_combinations = subset.drop_duplicates().reset_index(drop=True).reset_index()
    unique_combinations.rename(columns={'index': 'combination_id'}, inplace=True)
    
    df_combined = df.merge(unique_combinations, on=columns, how='left')

    return df_combined, unique_combinations

#Check whether the station combination and train number are related
def analyze_relationship(df, category_column, onehot_columns):

    df['onehot_combination'] = df[onehot_columns].apply(lambda x: '_'.join(str(int(v)) for v in x), axis=1)
    
    relationship_summary = df.groupby([category_column, 'onehot_combination']).size().reset_index(name='count')
    
    relationship_summary = relationship_summary.sort_values(by=[category_column, 'count'], ascending=[True, False])
    
    return relationship_summary



# Generate a statistical table to see the relationship between vehicle numbers and routes
def analyze_relationship_with_combinations(df, category_column, onehot_columns):

    unique_combinations = df[onehot_columns].drop_duplicates().reset_index(drop=True)
    unique_combinations['combination_id'] = range(len(unique_combinations))
    # print(unique_combinations)


    
    relationship_summary = df.groupby([category_column, 'combination_id']).size().reset_index(name='count')
    # print(relationship_summary)
    relationship_summary = relationship_summary.merge(unique_combinations, on='combination_id', how='left')
    
    relationship_summary = relationship_summary.sort_values(by=[category_column, 'count'], ascending=[True, False])
    
    return relationship_summary


#View column names, but wrap each line
def print_column_names_new_line(df):
     # Loop through the column names and print each column name
     for column in df.columns:
         print(f'\'{column}\',')
# print_column_names_new_line(trainmatrix)

#PCAmethod
def pca_reduce_dimension(df, target_column=None, variance_threshold=0.95):

    if target_column:
        features = df.drop(target_column, axis=1)
        target = df[target_column]
    else:
        features = df
    

    features_standardized = StandardScaler().fit_transform(features)
    

    pca = PCA(n_components=variance_threshold)
    principal_components = pca.fit_transform(features_standardized)
    
    columns = [f'Principal Component {i+1}' for i in range(principal_components.shape[1])]
    df_pca = pd.DataFrame(data=principal_components, columns=columns)
    
    if target_column:
        df_pca[target_column] = target.reset_index(drop=True)

    return df_pca





# In[26]:


# For the processing of small stations: first use 0 and 1 to represent whether you have passed this station (existence coding), and then use the average value to represent the total average delay of these stations (aggregation)
def datapreprcession(targetdatatimings = trainingDataTimings_df,dataconarr= trainingDataConArr_df,historicalcon=congestion_df):

     #First delete the extra unmaned columns in the first column
     matrixdata =drop_first_column(targetdatatimings)
     dataconarr = drop_first_column(dataconarr)
     historicalcon = drop_first_column(historicalcon)
     #Reset index column
     dataconarr = dataconarr.reset_index()
     #Merge tables
     matrixdata = matrixdata.merge(dataconarr, left_on='id', right_on='index', how='left').drop('index',axis=1)
     #Merge history delays
     matrixdata = matrixdata.merge(historicalcon,left_on=['week.day', 'hour'],right_on=['Day','Hour'],how='left')
     #Delete a duplicate column
     matrixdata = matrixdata.drop(['day.week','week.day','hour','train_code','arrival.to_y'],axis=1)
     #Convert the values of a column into numerical features
     matrixdata, traincode_to_num = encode_discrete_values(matrixdata,column_name='train.code')
     # print(traincode_to_num)
     # Convert the days of the week into numerical features in order
     matrixdata, day_to_num = encode_weekdays(matrixdata,column_name='Day')
     #Convert the station to a numerical feature
     matrixdata,station_to_num=encode_common_discrete_values(matrixdata,['departure.from','arrival.to_x'])
     # print(station_to_num)
     # Calculate time difference
     matrixdata = calculate_time_difference(matrixdata,column1='departure.time',column2='departure.schedule',new_column_name='departure.delay')
     matrixdata = calculate_time_difference(matrixdata,column1='arrival.time_x',column2='arrival.schedule_x',new_column_name='arrival.delay')
     return matrixdata
#Apply preprocessing to obtain data that can be trained on the target
# trainmatrix = datapreprcession(targetdatatimings=trainingDataTimings_df,dataconarr=trainingDataConArr_df,historicalcon=congestion_df)
# testmatrix = datapreprcession(targetdatatimings=testDataTimings_df,dataconarr=testDataConArr_df,historicalcon=congestion_df)



# In[27]:


# print(trainmatrix['departure.from'].unique())
# print(trainmatrix['arrival.to_x'].unique())
# print(trainmatrix.columns)
# print(trainmatrix.head(10))
# trainmatrix.to_csv('trainmatrix.csv',index=False)


# In[28]:


#Create a histogram of binary variable columns, not used
def plot_bar_for_binary_columns(df, target_columns):
    sns.set_style("whitegrid")
    
    num_cols = len(target_columns)
    if num_cols == 1:
        fig, axes = plt.subplots(nrows=1, ncols=1, figsize=(6, 4))
        axes = [axes]
    else:
        fig, axes = plt.subplots(nrows=num_cols, ncols=1, figsize=(6, 4 * num_cols))
    
    for i, col in enumerate(target_columns):
        count_data = df[col].value_counts().sort_index()
        sns.barplot(x=count_data.index, y=count_data.values, ax=axes[i], palette="viridis")
        axes[i].set_title(f'Distribution of {col}')
        axes[i].set_ylabel('Count')
        axes[i].set_xlabel(col)
    
    plt.tight_layout()
    plt.show()

# Histogram of binary variable columns, with their statistical numbers
def plot_bar_for_binary_columns_with_counts(df, target_columns):

    sns.set_style("whitegrid")
    

    num_cols = len(target_columns)
    if num_cols == 1:
        fig, axes = plt.subplots(nrows=1, ncols=1, figsize=(6, 4))
        axes = [axes]
    else:
        fig, axes = plt.subplots(nrows=num_cols, ncols=1, figsize=(6, 4 * num_cols))
    
    for i, col in enumerate(target_columns):

        count_data = df[col].value_counts().sort_index()

        ax = sns.barplot(x=count_data.index, y=count_data.values, ax=axes[i], palette="viridis")
        axes[i].set_title(f'Distribution of {col}')
        axes[i].set_ylabel('Count')
        axes[i].set_xlabel(col)


        for p in ax.patches:
            ax.annotate(f'{int(p.get_height())}', (p.get_x() + p.get_width() / 2., p.get_height()),
                        ha='center', va='center', fontsize=10, color='black', xytext=(0, 5),
                        textcoords='offset points')

    plt.tight_layout()
    plt.show()


#Distribution relationship between classification column and target column Violin plot
def plot_violin_by_group(df, group_column, target_column):

    plt.figure(figsize=(10, 6))
    sns.violinplot(x=group_column, y=target_column, data=df)
    plt.title(f'Distribution of {target_column} by {group_column}')
    plt.xlabel(group_column)
    plt.ylabel(target_column)
    plt.show()



# Distribution relationship between classification columns and target columns Density map
def plot_density_by_group(df, group_column, target_column):
    plt.figure(figsize=(10, 6))
    
    
    unique_groups = df[group_column].unique()
    for group in unique_groups:
        subset = df[df[group_column] == group]
        sns.kdeplot(subset[target_column], label=f'{group}', shade=True)
    
    plt.title(f'Density Distribution of {target_column} by {group_column}')
    plt.xlabel(target_column)
    plt.ylabel('Density')
    plt.legend(title=group_column)
    plt.show()


#Distribution relationship between classification columns and target columns Histogram
def plot_histograms_by_group(df, group_column, value_column):

    unique_groups = df[group_column].unique()
    

    n_groups = len(unique_groups)
    n_rows = (n_groups // 2) + (n_groups % 2)
    
    fig, axes = plt.subplots(nrows=n_rows, ncols=2, figsize=(12, 6 * n_rows), constrained_layout=True)

    if n_groups > 2:
        axes = axes.flatten()
    else:
        axes = axes.reshape(-1) 
    

    for i, group in enumerate(unique_groups):
        ax = axes[i]
        subset = df[df[group_column] == group][value_column]
        sns.histplot(subset, ax=ax, kde=False, binwidth=20)#binwidth adjust the column width or set the number of columns in bin
        ax.set_title(f'{value_column} distribution in {group}')
        ax.set_xlabel(value_column)
        ax.set_ylabel('Frequency')
    

    if n_groups % 2 != 0:
        axes[-1].axis('off')

    plt.show()


#Draw the columnar distribution of delayNotts
def plot_bar_for_one_var(df,col):

    sns.set_style("whitegrid")
    plt.figure(figsize=(10, 6))  
    sns.histplot(df[col], bins=50, kde=False, color='skyblue')  #bins specifies the number of columns of the histogram, kde is whether to draw kernel density estimation
    plt.title(f'Distribution of {col}')  
    plt.xlabel('Delay (Notts)')  
    plt.ylabel('Frequency')  
    plt.show()
    return 0


#Observe the target variable distribution according to the classification column, box plot or violin plot
def plot_distribution(data, category_col, target_col, plot_type="boxplot"):

    plt.figure(figsize=(10, 6)) 
    if plot_type == "boxplot":
        sns.boxplot(x=category_col, y=target_col, data=data)
    elif plot_type == "violinplot":
        sns.violinplot(x=category_col, y=target_col, data=data)
    else:
        raise ValueError("plot_type must be 'boxplot' or 'violinplot'")
    
    plt.title(f'Distribution of {target_col} by {category_col}') 
    plt.xticks(rotation=45) 
    plt.show()


# Draw a heat map, to be precise, it is a hierarchical aggregation heat map
def plot_heatmap(df, cols):
    corr = df[cols].corr()
    f, ax = plt.subplots(figsize=(9, 6))
    sns.heatmap(corr, annot=False, ax=ax, cmap='coolwarm', fmt='.2f',
                linewidths=.5, cbar_kws={"shrink": .5})#annot Used to control whether to display numbers on each grid
    ax.tick_params(axis='y', labelsize=10)
    plt.tight_layout()
    plt.show()


#Hierarchical aggregation heat map
def plot_clustermap(df,cols):
    corr = df[cols].corr()
    
    corr_filled = corr.fillna(0)
    sns.clustermap(corr_filled, cmap='coolwarm', linewidths=.5, annot=False,figsize=(9, 6))
    plt.show()
    
#Draw a pairplot, which is very time-consuming to generate.
def generate_pairplot(df, columns):
    sns.pairplot(df[columns])
    plt.show()
    
#A categorical column, multiple numerical columns, generate a violin plot
def plot_violin_by_category(df, category_column, value_columns):
    plt.figure(figsize=(12, len(value_columns) * 4))
    for i, value_column in enumerate(value_columns, start=1):
        plt.subplot(len(value_columns), 1, i)
        sns.violinplot(x=category_column, y=value_column, data=df)
        plt.title(f'Violin plot of {value_column} by {category_column}')
    plt.tight_layout()
    plt.show()

# Output violin plots of all numeric columns
def plot_violin_for_numerical_columns(df):

    numerical_cols = df.select_dtypes(include=['float64', 'int64']).columns

    fig, axes = plt.subplots(nrows=len(numerical_cols), ncols=1, figsize=(8, len(numerical_cols) * 4))

    if len(numerical_cols) == 1:
        axes = [axes]
    
    for i, col in enumerate(numerical_cols):
        sns.violinplot(x=df[col], ax=axes[i],fill= False)
        axes[i].set_title(col)
        axes[i].set_ylabel('Density')
    
    plt.tight_layout()
    plt.show()


# In[29]:


# Considering that it is better for one row to represent one data point, the key point is that the conarr table cannot be diffused into matrixdata.
def rebuild_matrixdata(df):
    stations_order = [
        'LEEDS', 'NORMNTN', 'WKFLDKG', 'WKFLDWG', 'BNSLY', 'MEADWHL', 'SHEFFLD'
    ]
    
    new_columns = []
    for station in stations_order:
        if station not in ['LEEDS', 'SHEFFLD']:  # No existence encoding is required for LEEDS and SHEFFLD
            new_columns.append(f"{station}_presence")
        new_columns.extend([
            f"{station}_dep_time", f"{station}_dep_schedule",
            f"{station}_arr_time", f"{station}_arr_schedule"
        ])
        
    restructured_data_list = []

    for train_id in df['id'].unique():
        train_data = df[df['id'] == train_id]
        row_data = {col: None for col in new_columns}
        
        for station in stations_order:

            if station not in ['LEEDS', 'SHEFFLD']:
                row_data[f"{station}_presence"] = int(station in train_data['departure.from'].values or station in train_data['arrival.to'].values)

            dep_station_data = train_data[train_data['departure.from'] == station]
            if not dep_station_data.empty:
                row_data[f"{station}_dep_time"] = dep_station_data.iloc[0]['departure.time']
                row_data[f"{station}_dep_schedule"] = dep_station_data.iloc[0]['departure.schedule']
            
            arr_station_data = train_data[train_data['arrival.to'] == station]
            if not arr_station_data.empty:
                row_data[f"{station}_arr_time"] = arr_station_data.iloc[0]['arrival.time']
                row_data[f"{station}_arr_schedule"] = arr_station_data.iloc[0]['arrival.schedule']
                
        restructured_data_list.append(row_data)
    
    restructured_data = pd.DataFrame(restructured_data_list)
    restructured_data['id'] = df['id'].unique()
    restructured_data.set_index('id', inplace=True)

    return restructured_data

# Used to convert the reconstructed station status into seconds using the countdown function
def calculate_delays_and_stays(df):

    for index, row in df.iterrows():
        for col in df.columns:
            if '_dep_time' in col or '_arr_time' in col:
                station = col.split('_')[0]
                time_type = 'dep' if 'dep' in col else 'arr'  # Determine whether it is departure time or arrival time

                
                actual_col = f'{station}_{time_type}_time'
                schedule_col = f'{station}_{time_type}_schedule'
                
                # Calculate delay time
                if pd.notna(row[actual_col]) and pd.notna(row[schedule_col]):
                    actual_seconds = text_to_seconds(row[actual_col])
                    schedule_seconds = text_to_seconds(row[schedule_col])
                    df.at[index, f'{station}_{time_type}_delay'] = actual_seconds - schedule_seconds

            # For intermediate stations, calculate the dwell time and planned dwell time
            if '_dep_time' in col and station != 'LEEDS' and station != 'SHEFFLD':
                arr_col = f'{station}_arr_time'
                dep_col = f'{station}_dep_time'
                arr_schedule_col = f'{station}_arr_schedule'
                dep_schedule_col = f'{station}_dep_schedule'

                # Calculate the actual stay time and planned stay time, assuming that both time and planned time exist
                if pd.notna(row[arr_col]) and pd.notna(row[dep_col]) and pd.notna(row[arr_schedule_col]) and pd.notna(row[dep_schedule_col]):
                    arr_seconds = text_to_seconds(row[arr_col])
                    dep_seconds = text_to_seconds(row[dep_col])
                    arr_schedule_seconds = text_to_seconds(row[arr_schedule_col])
                    dep_schedule_seconds = text_to_seconds(row[dep_schedule_col])
                    
                    df.at[index, f'{station}_stay'] = dep_seconds - arr_seconds
                    df.at[index, f'{station}_scheduled_stay'] = dep_schedule_seconds - arr_schedule_seconds

    return df


#New preprocessing step based on rebuild_matrixdata function
def datapreprcession2(targetdatatimings = trainingDataTimings_df,dataconarr= trainingDataConArr_df,historicalcon=congestion_df):
#Put all the previously written content here later.
    #Put out the extra unmaned columns in the first column first.
    datatimings =drop_first_column(targetdatatimings)
    # print(datatimings.head())
    dataconarr = drop_first_column(dataconarr)
    historicalcon = drop_first_column(historicalcon)
    #Reset index column
    dataconarr = dataconarr.reset_index()

    #crucialtwosteps
    # Reconstruct the feature set, each row represents a train
    matrixdata =rebuild_matrixdata(datatimings)
    #Statistical feature engineering such as delay and stay
    matrixdata = calculate_delays_and_stays(matrixdata)

    #Merge tables
    matrixdata = matrixdata.merge(dataconarr, left_on='id', right_on='index', how='left').drop('index',axis=1)
    #Merge history delays
    matrixdata = matrixdata.merge(historicalcon,left_on=['week.day', 'hour'],right_on=['Day','Hour'],how='left')
    #Delete the duplicate columns. You can also extract them directly at the end without deleting them.
    matrixdata = matrixdata.drop(['week.day','hour'],axis=1)
    # Convert train.code into numerical features
    # matrixdata, traincode_to_num = encode_discrete_values(matrixdata,column_name='train_code')
    # Another way to convert train.code into numerical features: uniques is the only truth value comparison table
    codes, uniques = pd.factorize(matrixdata['train_code'])
    matrixdata['train_code.code'] = codes
    #Convert Day into numerical features
    days, uniques2 = pd.factorize(matrixdata['Day'])
    matrixdata['Day.code'] = days
    #Convert day of the week to onehot feature
    matrixdata = pd.get_dummies(matrixdata, columns=['Day'])
    #Take an average of the congestion values
    matrixdata = add_average_columns(matrixdata,
                                     cols1=['Leeds.trains_x', 'Leeds.av.delay_x',
       'Sheffield.trains_x', 'Sheffield.av.delay_x', 'Nottingham.trains_x',
       'Nottingham.av.delay_x'],
       cols2=['Leeds.trains_y', 'Leeds.av.delay_y', 'Sheffield.trains_y',
       'Sheffield.av.delay_y', 'Nottingham.trains_y', 'Nottingham.av.delay_y'],
       new_col_names=['Leeds.trains', 'Leeds.av.delay',
       'Sheffield.trains', 'Sheffield.av.delay', 'Nottingham.trains',
       'Nottingham.av.delay'])
    # Count the combination of small stations and generate classification columns
    matrixdata, unique_combinations = add_combination_column_and_get_combinations(matrixdata,['NORMNTN_presence','WKFLDKG_presence','WKFLDWG_presence','BNSLY_presence','MEADWHL_presence'])
    # print(unique_combinations)
    #Average the delay of small stations
    matrixdata['station_ave'] = matrixdata[['WKFLDKG_dep_delay', 'WKFLDKG_arr_delay', 'BNSLY_dep_delay',
            'BNSLY_arr_delay','MEADWHL_dep_delay',
    'MEADWHL_arr_delay', 'NORMNTN_dep_delay',
    'NORMNTN_arr_delay',
    'WKFLDWG_dep_delay',
    'WKFLDWG_arr_delay']].mean(axis=1,skipna=True)

    # #Subtract the waiting time at the station
    matrixdata = subtract_columns_all(matrixdata,
                                    cols1=['WKFLDKG_stay',
    'BNSLY_stay', 'MEADWHL_stay',
    'NORMNTN_stay', 'WKFLDWG_stay'],
    cols2=[
    'WKFLDKG_scheduled_stay','BNSLY_scheduled_stay','MEADWHL_scheduled_stay',
    'NORMNTN_scheduled_stay','WKFLDWG_scheduled_stay'],
    new_col_names=['WKFLDKG_stay_subtract',
    'BNSLY_stay_subtract', 'MEADWHL_stay_subtract',
    'NORMNTN_stay_subtract', 'WKFLDWG_stay_subtract'])

    #Make congestion a subtraction and learn from the teacher’s suggestions
    matrixdata = subtract_columns_all(matrixdata,
                                    cols1=['Leeds.trains_x', 'Leeds.av.delay_x',
    'Sheffield.trains_x', 'Sheffield.av.delay_x', 'Nottingham.trains_x',
    'Nottingham.av.delay_x'],
    cols2=['Leeds.trains_y', 'Leeds.av.delay_y', 'Sheffield.trains_y',
    'Sheffield.av.delay_y', 'Nottingham.trains_y', 'Nottingham.av.delay_y'],
    new_col_names=['Leeds.trains_sub', 'Leeds.av.delay_sub',
    'Sheffield.trains_sub', 'Sheffield.av.delay_sub', 'Nottingham.trains_sub',
    'Nottingham.av.delay_sub'])

    #Remove some useless columns
    matrixdata = matrixdata.drop(['arrival.to','LEEDS_arr_time','LEEDS_arr_schedule','SHEFFLD_dep_time','SHEFFLD_dep_schedule'],axis=1)
    
    return matrixdata


# In[30]:


#  The following is the code framework of the pipeline
def process_features(df):
    
    '''
    # 没有修改数值的版本
    df = df[['WKFLDKG_presence','NORMNTN_presence','WKFLDWG_presence','BNSLY_presence','MEADWHL_presence',
            'LEEDS_dep_delay', 'WKFLDKG_dep_delay', 'WKFLDKG_stay',
       'WKFLDKG_scheduled_stay', 'WKFLDKG_arr_delay', 'BNSLY_dep_delay',
       'BNSLY_stay', 'BNSLY_scheduled_stay', 'BNSLY_arr_delay',
       'MEADWHL_dep_delay', 'MEADWHL_stay', 'MEADWHL_scheduled_stay',
       'MEADWHL_arr_delay', 'SHEFFLD_arr_delay', 'NORMNTN_dep_delay',
       'NORMNTN_stay', 'NORMNTN_scheduled_stay', 'NORMNTN_arr_delay',
       'WKFLDWG_dep_delay', 'WKFLDWG_stay', 'WKFLDWG_scheduled_stay',
       'WKFLDWG_arr_delay', 'Leeds.trains_x', 'Leeds.av.delay_x',
       'Sheffield.trains_x', 'Sheffield.av.delay_x', 'Nottingham.trains_x',
       'Nottingham.av.delay_x', 'delay.secs', 'Hour',
       'Leeds.trains_y', 'Leeds.av.delay_y', 'Sheffield.trains_y',
       'Sheffield.av.delay_y', 'Nottingham.trains_y', 'Nottingham.av.delay_y',
       'train_code.code', 'Day_Friday', 'Day_Monday', 'Day_Saturday',
       'Day_Sunday', 'Day_Thursday', 'Day_Tuesday', 'Day_Wednesday']]
    '''
 
    # Version that turns congestion into an average #score 33000
    # df = df[['NORMNTN_presence','WKFLDKG_presence','WKFLDWG_presence','BNSLY_presence','MEADWHL_presence',
             
    #         'LEEDS_dep_delay', 'WKFLDKG_dep_delay', 'WKFLDKG_stay',
    #    'WKFLDKG_scheduled_stay', 'WKFLDKG_arr_delay', 'BNSLY_dep_delay',
    #    'BNSLY_stay', 'BNSLY_scheduled_stay', 'BNSLY_arr_delay',
    #    'MEADWHL_dep_delay', 'MEADWHL_stay', 'MEADWHL_scheduled_stay',
    #    'MEADWHL_arr_delay', 'SHEFFLD_arr_delay', 'NORMNTN_dep_delay',
    #    'NORMNTN_stay', 'NORMNTN_scheduled_stay', 'NORMNTN_arr_delay',
    #    'WKFLDWG_dep_delay', 'WKFLDWG_stay', 'WKFLDWG_scheduled_stay',
    #    'WKFLDWG_arr_delay',  
    #    'delay.secs', 
    #    'Hour',
    #    'Leeds.trains', 'Leeds.av.delay',
    #    'Sheffield.trains', 'Sheffield.av.delay', 'Nottingham.trains',
    #    'Nottingham.av.delay',
    #    'train_code.code', 'Day.code']]
    
    #score 33200/29000
    # df = df[['NORMNTN_presence','WKFLDKG_presence','WKFLDWG_presence','BNSLY_presence','MEADWHL_presence',
    #         'LEEDS_dep_delay',  'WKFLDKG_stay',
    #    'WKFLDKG_scheduled_stay',
    #    'BNSLY_stay', 'BNSLY_scheduled_stay', 'MEADWHL_stay', 'MEADWHL_scheduled_stay', 'SHEFFLD_arr_delay',
    #    'station_ave',
    #    'NORMNTN_stay', 'NORMNTN_scheduled_stay', 'WKFLDWG_stay', 'WKFLDWG_scheduled_stay',
    #     'delay.secs', 'Hour',
    #    'Leeds.trains', 'Leeds.av.delay',
    #    'Sheffield.trains', 'Sheffield.av.delay', 'Nottingham.trains',
    #    'Nottingham.av.delay',
    #    'train_code.code', 'Day.code']]

    #     #score 29576.1508
    #     df = df[[
    #         'delay.secs','Hour',
    #                         'Day_Friday',
    #         'Day_Monday',
    #         'Day_Saturday',
    #         'Day_Sunday',
    #         'Day_Thursday',
    #         'Day_Tuesday',
    #         'Day_Wednesday',
    #         # 'Leeds.trains',
    # # 'Leeds.av.delay',
    # # 'Sheffield.trains',
    # # 'Sheffield.av.delay',
    # # 'Nottingham.trains',
    # # 'Nottingham.av.delay',# The value with congestion is even worse than without it.
    #         'SHEFFLD_arr_delay',
    #         'LEEDS_dep_delay'        
    #     ]]

    
   #Score 29000. Average delay using small stations, difference between small station stopping plan and actual
    df = df[['NORMNTN_presence','WKFLDKG_presence','WKFLDWG_presence','BNSLY_presence','MEADWHL_presence',
            'LEEDS_dep_delay',  'SHEFFLD_arr_delay',
       'station_ave',
       'WKFLDKG_stay_subtract',
       'BNSLY_stay_subtract', 'MEADWHL_stay_subtract', 
       'NORMNTN_stay_subtract', 'WKFLDWG_stay_subtract',
        'delay.secs', 'Hour',
       'Leeds.trains', 'Leeds.av.delay',
       'Sheffield.trains', 'Sheffield.av.delay', 'Nottingham.trains',
       'Nottingham.av.delay',
       'train_code.code', 'Day.code']]
    
 #  # 
    # df = df[['NORMNTN_presence','WKFLDKG_presence','WKFLDWG_presence','BNSLY_presence','MEADWHL_presence',
    #         'LEEDS_dep_delay',  'SHEFFLD_arr_delay',
    #    'station_ave',
    #     'delay.secs', 'Hour',
    #    'Leeds.trains', 'Leeds.av.delay',
    #    'Sheffield.trains', 'Sheffield.av.delay', 'Nottingham.trains',
    #    'Nottingham.av.delay',
    #    'train_code.code', 'Day.code']]
   

       
    # #A version that changes the station stay time into a subtracted difference #Score 34000
    # df = df[['WKFLDKG_presence','NORMNTN_presence','WKFLDWG_presence','BNSLY_presence','MEADWHL_presence',
    #         'LEEDS_dep_delay', 'WKFLDKG_dep_delay', 'WKFLDKG_arr_delay', 'BNSLY_dep_delay', 
    #         'BNSLY_arr_delay','MEADWHL_dep_delay', 
    #    'MEADWHL_arr_delay', 'SHEFFLD_arr_delay', 'NORMNTN_dep_delay',
    #    'NORMNTN_arr_delay',
    #    'WKFLDWG_dep_delay', 
    #    'WKFLDWG_arr_delay', 'WKFLDKG_stay_subtract',
    #    'BNSLY_stay_subtract', 'MEADWHL_stay_subtract', 
    #    'NORMNTN_stay_subtract', 'WKFLDWG_stay_subtract',
    #     'delay.secs', 'Hour',
    #    'Leeds.trains', 'Leeds.av.delay',
    #    'Sheffield.trains', 'Sheffield.av.delay', 'Nottingham.trains',
    #    'Nottingham.av.delay',
    #    'train_code.code', 'Day_Friday', 'Day_Monday', 'Day_Saturday',
    #    'Day_Sunday', 'Day_Thursday', 'Day_Tuesday', 'Day_Wednesday']] 
    
    # df = df.drop(['train.code', 'departure.from', 'departure.time', 'departure.schedule',
    #    'arrival.to_x', 'arrival.time_x','arrival.schedule_x','arrival.time_y', 'arrival.schedule_y','Day'],axis=1)
    return df


# Get features and target variables
def get_features_and_target(df, target_column_name):
     X = df.drop(target_column_name, axis=1)
     y = df[target_column_name]
     return

# Create pipeline
def create_pipeline():
     pipeline = Pipeline([
         ('scaler', StandardScaler()),
         ('regressor', XGBRegressor(objective='reg:squarederror'))
     ])
     return pipeline

# Set parameters for grid search
params = {
    'regressor__max_depth': [2,3,5],
    'regressor__n_estimators': [50,90,100,110],
    'regressor__learning_rate': [0.1,0.15,0.2]
}


def train_lasso_model(df, target_column, alpha=1.0):
    X = df.drop(target_column, axis=1)
    y = df[target_column]
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.1, random_state=42)
    lasso = Lasso(alpha=alpha)
    lasso.fit(X_train, y_train)
    return lasso




def main():
#Declare global variables
    global trainmatrix
    global testmatrix
    global featurematrix

    #Perform preprocessing
    # trainmatrix = datapreprcession(targetdatatimings=trainingDataTimings_df,dataconarr=trainingDataConArr_df,historicalcon=congestion_df)
    trainmatrix = datapreprcession2(targetdatatimings=trainingDataTimings_df,dataconarr=trainingDataConArr_df,historicalcon=congestion_df)
    testmatrix = datapreprcession2(targetdatatimings=testDataTimings_df,dataconarr=testDataConArr_df,historicalcon=congestion_df)

    #Special processing trainmatrix
    trainmatrix = trainmatrix[~trainmatrix['combination_id'].isin(['3','5','8','9'])]


    # First delete the unusable feature columns
    featurematrix = process_features(trainmatrix)
    featurematrix_test = process_features(testmatrix)
    # print(trainmatrix.columns)

    # Execute PCA method, temporarily unavailable
    # featurematrix = pca_reduce_dimension(featurematrix,target_column='delay.secs',variance_threshold=0.95)

    # Get features and target variables
    X, y = get_features_and_target(featurematrix, 'delay.secs')
    X_test,y_test = get_features_and_target(featurematrix_test,'delay.secs')

    # Create pipeline
    pipeline = create_pipeline()

    #Create GridSearchCV object
    grid_search = GridSearchCV(pipeline, param_grid=params, cv=10, scoring='neg_mean_squared_error', verbose=1)

    # # Perform a grid search
    grid_search.fit(X, y)

    # # Print the best parameters and best score (MSE)
    print(f"Best parameters: {grid_search.best_params_}")
    print(f"Best cross-validation score (MSE): {-grid_search.best_score_}")

    # Use the best model for prediction
    y_pred = grid_search.predict(X_test)

    return y_pred



if __name__ == "__main__":
    predicted_values = main()
    


# In[31]:


# generate csv file
# predicted_df = pd.DataFrame(predicted_values,columns=['x'])
# predicted_df.to_csv('trains_group_F.csv',index=False)

#Make a heat map, that is, a correlation coefficient map

cols = [ 'delay.secs','LEEDS_dep_delay', 'WKFLDKG_dep_delay', 'WKFLDKG_stay',
        'WKFLDKG_arr_delay', 'BNSLY_dep_delay',
       'BNSLY_stay',  'BNSLY_arr_delay',
       'MEADWHL_dep_delay', 'MEADWHL_stay', 
       'MEADWHL_arr_delay', 'SHEFFLD_arr_delay', 'NORMNTN_dep_delay',
       'NORMNTN_stay',  'NORMNTN_arr_delay',
       'WKFLDWG_dep_delay', 'WKFLDWG_stay',
       'WKFLDWG_arr_delay', 'Leeds.trains_x', 'Leeds.av.delay_x',
       'Sheffield.trains_x', 'Sheffield.av.delay_x', 'Nottingham.trains_x',
       'Nottingham.av.delay_x','Hour',
       'Leeds.trains_y', 'Leeds.av.delay_y', 'Sheffield.trains_y',
       'Sheffield.av.delay_y', 'Nottingham.trains_y', 'Nottingham.av.delay_y']
       
plot_heatmap(trainmatrix,cols)

#pair-column relationship
# generate_pairplot(trainmatrix, cols)

# print_column_names_new_line(trainmatrix)
# trainmatrix.info()


# In[32]:

# The function has actually been replaced
# count_onehot_combinations(trainmatrix,['NORMNTN_presence','WKFLDKG_presence','WKFLDWG_presence','BNSLY_presence','MEADWHL_presence'])

# Very important analysis: the relationship between station combination and frequency
train_station_set = analyze_relationship_with_combinations(trainmatrix,category_column='train_code.code',onehot_columns=['NORMNTN_presence','WKFLDKG_presence','WKFLDWG_presence','BNSLY_presence','MEADWHL_presence'])
# print(train_station_set)
# train_station_set.to_csv('train_station_set.csv',index=False)
test_station_set = analyze_relationship_with_combinations(testmatrix,category_column='train_code.code',onehot_columns=['NORMNTN_presence','WKFLDKG_presence','WKFLDWG_presence','BNSLY_presence','MEADWHL_presence'])
# print(test_station_set)
# test_station_set.to_csv('test_station_set.csv',index=False)



# Calculate correlation
def explore_relationship(df, category_col, onehot_cols):
     # Find the unique one-hot encoding combination
     # unique_combinations = df[onehot_cols].drop_duplicates()
     unique_combinations = count_onehot_combinations(df,onehot_cols)
     # Calculate the distribution of categorical variables for each combination
     for index, row in unique_combinations.iterrows():
         subset = df[(df[onehot_cols] == row.values).all(axis=1)]
         distribution = subset[category_col].value_counts(normalize=True)
         print(f"Combination {index} distribution:\n{distribution}\n")
# relationship = explore_relationship(trainmatrix, 'train_code.code', ['NORMNTN_presence','WKFLDKG_presence','WKFLDWG_presence','BNSLY_presence','MEADWHL_presence'])


# In[33]:


# print(trainmatrix2.columns)
selected_columns = ['LEEDS_dep_delay', 'WKFLDKG_dep_delay', 'WKFLDKG_stay',
       'WKFLDKG_scheduled_stay', 'WKFLDKG_arr_delay', 'BNSLY_dep_delay',
       'BNSLY_stay', 'BNSLY_scheduled_stay', 'BNSLY_arr_delay',
       'MEADWHL_dep_delay', 'MEADWHL_stay', 'MEADWHL_scheduled_stay',
       'MEADWHL_arr_delay', 'SHEFFLD_arr_delay', 'NORMNTN_dep_delay',
       'NORMNTN_stay', 'NORMNTN_scheduled_stay', 'NORMNTN_arr_delay',
       'WKFLDWG_dep_delay', 'WKFLDWG_stay', 'WKFLDWG_scheduled_stay',
       'WKFLDWG_arr_delay']  
selected_columns2 = ['NORMNTN_presence','WKFLDKG_presence','WKFLDWG_presence','BNSLY_presence','MEADWHL_presence']
# plot_ridge(trainmatrix3, selected_columns)
# plot_violin_for_numerical_columns(trainmatrix3)
# plot_bar_for_binary_columns_with_counts(trainmatrix3,selected_columns2)
# print(trainingDataConArr_df.columns)

# plot_histograms_by_group(trainmatrix,group_column='combination_id',value_column='delay.secs')
# plot_distribution(trainmatrix,category_col='combination_id',target_col='delay.secs',plot_type='boxplot')
# plot_distribution(trainmatrix,category_col='train_code.code',target_col='delay.secs',plot_type='boxplot')

#Count the number of each category in a category column
# print(trainmatrix['combination_id'].value_counts())
# print(testmatrix['combination_id'].value_counts())


# In[34]:


#I realize that the station combination truth table is only useful to myself, 
#and I do not include it in my machine learning because the existence encoding of the passing stations is more important.
def create_truth_table(train_df, test_df, onehot_columns):

    train_df['combination_id'] = train_df[onehot_columns].apply(lambda x: '_'.join(x.astype(str)), axis=1)
    test_df['combination_id'] = test_df[onehot_columns].apply(lambda x: '_'.join(x.astype(str)), axis=1)
    
    common_combinations = pd.merge(
        train_df[['combination_id']],
        test_df[['combination_id']],
        on='combination_id',
        how='inner'
    )['combination_id'].unique()
    
    
    truth_table = train_df[train_df['combination_id'].isin(common_combinations)]
    
    return truth_table

onehot_columns = ['NORMNTN_presence','WKFLDKG_presence','WKFLDWG_presence','BNSLY_presence','MEADWHL_presence']


truth_table = create_truth_table(train_station_set,test_station_set, onehot_columns)



# #Observe the fiddle relationship between hour and historical delay data, the effect is very poor.
# plot_violin_by_category(trainmatrix,category_column='Hour',value_columns=[
# 'Leeds.trains', 'Leeds.av.delay',
# 'Sheffield.trains', 'Sheffield.av.delay', 'Nottingham.trains',
# 'Nottingham.av.delay',
# ])
#The observation of observation hours and current delay time does not have the same observation effect as group C.
# plot_distribution(trainmatrix,'Hour','delay.secs','boxplot')


# In[35]:


#Try to verify the significant difference between the categorical column and the target numerical column
from scipy.stats import f_oneway


df = trainmatrix
# Clean up groups containing NaN values
groups = [df[df['Hour'] == hour]['delay.secs'].dropna() for hour in range(7, 21)]

# ANOVA including only non-empty groups
non_empty_groups = [group for group in groups if not group.empty]
if len(non_empty_groups) > 1: # At least two groups are required for ANOVA
     f_statistic, p_value = f_oneway(*non_empty_groups)
     print(f"F Statistic: {f_statistic}, p-value: {p_value}")
else:
     print("Not enough non-empty groups for ANOVA")
#F Statistic: 6.573105233571209, p-value: 2.1355187679532672e-12
#If the p value is less than the significance level (such as 0.05), it means that there are significant differences between different groups, 
#that is, the categorical column has significant classification for the target variable.


# In[36]:


trainmatrix['delay.secs'].describe()


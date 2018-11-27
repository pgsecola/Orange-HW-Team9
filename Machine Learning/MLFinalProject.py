import pandas as pd
import os
import numpy as np
from sklearn import tree
from sklearn.model_selection import train_test_split, RandomizedSearchCV, GridSearchCV
from sklearn import preprocessing
from sklearn.preprocessing import LabelEncoder
from scipy.stats import randint as sp_randint
from sklearn.metrics import classification_report,confusion_matrix
from sklearn.metrics import mean_absolute_error
from sklearn.neural_network import MLPRegressor
from sklearn.ensemble.gradient_boosting import GradientBoostingRegressor
from sklearn.ensemble import RandomForestRegressor
from sklearn import linear_model
from sklearn.linear_model import LassoCV
from sklearn.linear_model import Ridge
from sklearn.decomposition import PCA
import xgboost as xgb
import random

#read in data   
raw_data = pd.read_csv("MLProjectData.csv")
target = raw_data['target']

#convert categorical variables to dummies
cat1dummies = (pd.get_dummies(raw_data['cat1']))
cat2dummies = (pd.get_dummies(raw_data['cat2']))
together = pd.concat([raw_data, cat1dummies, cat2dummies], axis = 1)
features = together.drop(['target', 'cat1', 'cat2'], axis = 1)

#standardize the data
x = features.values
min_max_scaler = preprocessing.MinMaxScaler()
x_scaled = min_max_scaler.fit_transform(x)
features = pd.DataFrame(x_scaled)


#perform same preprocessing as above on the holdout test dataset
test_data = pd.read_csv("testData.csv")
cat1dummies_test = (pd.get_dummies(test_data['cat1']))
cat2dummies_test = (pd.get_dummies(test_data['cat2']))
together_test = pd.concat([test_data, cat1dummies_test, cat2dummies_test], axis = 1)
test_features = together_test.drop(['cat1', 'cat2', 'Unnamed: 0'], axis = 1)
xtest = test_features.values
min_max_scaler = preprocessing.MinMaxScaler()
x_scaled_test = min_max_scaler.fit_transform(xtest)
features_test = pd.DataFrame(x_scaled_test)

#Use all data for fitting final model
X_train, X_test, y_train, y_test = train_test_split(features, target,test_size=0.0)

############# Run selected model on test data to generate predictions #####################
gb = GradientBoostingRegressor(max_depth = 3, max_features = 5, n_estimators = 10, loss = 'lad')
gb.fit(X_train, y_train)
pred_gb = gb.predict(features_test)
rows = np.arange(1,78)
testing = pd.DataFrame({'Row':rows, 'Prediction':pred_gb[0:77]})
testing.to_csv('Orange9.csv')

#Tried performing pca but did not work
#features.replace(False, 0, inplace=True)
#pca = PCA(n_components=25)
#new_features = pca.fit_transform(features)

#Split data into training and test
X_train, X_test, y_train, y_test = train_test_split(features, target,test_size=0.2)

#Get results for baseline model
print (np.mean(abs(y_test.subtract(np.mean(y_train)))))


#CV for Gradient Boosting
gb = GradientBoostingRegressor()

params = {'n_estimators':[5,10,20,25,50,100],
          'max_depth':sp_randint(1,5),
          'max_features':sp_randint(2, 8),
          'loss': ['lad']}

#generate random search cv
random_search = RandomizedSearchCV(gb, param_distributions=params,
                                   cv=10, n_iter = 1000, scoring = 'neg_mean_absolute_error')
random_search.fit(X_train, y_train)
cv_res = pd.DataFrame(random_search.cv_results_).sort_values(by=['rank_test_score'])
print(cv_res[['rank_test_score','mean_test_score']].head(10))
print(cv_res[['param_max_depth', 'param_n_estimators', 'param_max_features']].head(10))


#fit Gradient Boosting model
gb = GradientBoostingRegressor(max_depth = 3, max_features = 5, n_estimators = 10, loss = 'lad')
gb.fit(X_train, y_train)
pred_gb = gb.predict(X_test)
print (max(pred_gb))
print (min(pred_gb))
print ("random gb", mean_absolute_error(y_test,pred_gb))
#print (gb.feature_importances_)

#Fit Neural Network model
nn = MLPRegressor(max_iter=100000, hidden_layer_sizes= random.randint(0,100))
nn.fit(X_train, y_train)
pred_nn = nn.predict(X_test)
print ("nn", mean_absolute_error(y_test,pred_nn))


#Fit Lasso Regression model
las = linear_model.Lasso(alpha = 0.1, max_iter = 1000000)
las.fit(X_train, y_train)
pred_las = las.predict(X_test)
print (pred_las)
print (max(pred_las))
print (min(pred_las))
print ("lasso", mean_absolute_error(y_test,pred_las))

#Fit Ridge Regression model
rid = linear_model.Ridge()
rid.fit(X_train, y_train)
pred_rid = rid.predict(X_test)
print (pred_rid)
print (max(pred_rid))
print (min(pred_rid))
print ("ridge", mean_absolute_error(y_test,pred_rid))

#Fit Random Forest model
rf = RandomForestRegressor(criterion='mse', n_estimators = 10)
rf.fit(X_train, y_train)
pred_rf = rf.predict(X_test)
print ("rf", mean_absolute_error(y_test,pred_rf))



# loop used to check which of the 3 best models worked best
lasgooo = 0
las_arr = []
gbooom = 0
gb_arr = []
avg = 0
avg_arr = []
diff = 0
for i in range(1000):
    raw_data = pd.read_csv("MLProjectData.csv")
    target = raw_data['target']
    cat1dummies = (pd.get_dummies(raw_data['cat1']))
    cat2dummies = (pd.get_dummies(raw_data['cat2']))
    together = pd.concat([raw_data, cat1dummies, cat2dummies], axis = 1)
    #print (together)
    #print (pd.to_numeric(raw_data['cat1'], errors = 'coerce'))
    #raw_data['cat1'] = raw_data['cat1'].map(lambda x: ord(x) - 64)
    #raw_data['cat2'] = raw_data['cat2'].map(lambda x: ord(x) - 64)
    features = together.drop(['target', 'cat1', 'cat2'], axis = 1)
    x = features.values #returns a numpy array
    min_max_scaler = preprocessing.MinMaxScaler()
    x_scaled = min_max_scaler.fit_transform(x)
    features = pd.DataFrame(x_scaled)
    
    #features.replace(False, 0, inplace=True)
    #pca = PCA(n_components=25)
    #new_features = pca.fit_transform(features)
    #print (new_features)
    ##new_features = features.applymap(lambda x: 0 if x == False else x)
    ##print (new_features)
    
    X_train, X_test, y_train, y_test = train_test_split(features, target,test_size=0.2)
    average = np.mean(abs(y_test.subtract(np.mean(y_train))))
    avg_arr.append(average)
    gb = GradientBoostingRegressor(max_depth = 3, max_features = 5, n_estimators = 10, loss = 'lad')
    gb.fit(X_train, y_train)
    pred_gb = gb.predict(X_test)
    gberror = mean_absolute_error(y_test,pred_gb)
    gb_arr.append(gberror)
    
    las = linear_model.Lasso(alpha = 0.1, max_iter = 1000000)
    las.fit(X_train, y_train)
    pred_las = las.predict(X_test)
    laserror = mean_absolute_error(y_test,pred_las)
    
    las_arr.append(laserror)
    
    if gberror> laserror:
        if average> laserror:
            lasgooo+=1
        else:
            avg+=1
    else:
        if average> gberror:
            gbooom+=1
            diff+=(average-gberror)
            avgdiff = (diff/gbooom)
        else:
            avg+=1
    print (i)
    print ("avg", avg)
    print("avg mae", np.mean(avg_arr))
    print ("gboooom", gbooom)
    print("gb mae", np.mean(gb_arr))
    print ("lasgooo", lasgooo)
    print("las mae", np.mean(las_arr))
    print("avgdiff", avgdiff)
    
    
import ncaa2018_model
import numpy as np
from keras.models import Sequential
from keras.layers.core import Dense, Activation, Dropout
from keras.optimizers import SGD
from keras.models import load_model
from keras.utils import np_utils
import os

from sklearn.cross_validation import train_test_split
from sklearn.metrics import roc_curve, auc
import pandas as pd

import matplotlib.pyplot as plt

np.random.seed(1671)

def plot_roc(y_test, y_score, title):
    fpr, tpr, _ = roc_curve(y_test, y_score)
    roc_auc = auc(fpr, tpr)
    plt.figure()
    plt.plot(fpr, tpr, label='ROC curve (AUC = %0.2f)' % roc_auc)
    plt.plot([0, 1], [0, 1], 'k--')
    plt.xlim([0.0, 1.05])
    plt.ylim([0.0, 1.05])
    plt.xlabel('False Positive Rate')
    plt.ylabel('True Positive Rate')
    plt.title(title + ' - (AUC = %0.2f)' % roc_auc)
    plt.show()
    print('AUC: %f' % roc_auc)

ncaa_train = np.loadtxt('./modeldata/train2015.csv', delimiter = ',', skiprows = 1) #skip header/label row
ncaa_test = np.loadtxt('./modeldata/test2015.csv', delimiter = ',', skiprows = 1)

ncaa_train_Predictors = ncaa_train[:,1:ncaa_train.shape[1]] # get all rows, all columns except the last column
ncaa_train_class = ncaa_train[:,0]

ncaa_test_Predictors = ncaa_test[:,1:ncaa_test.shape[1]] # get all rows, all columns except the last column
ncaa_test_class = ncaa_test[:,0]

CLASS_NUM = 2
NUM_PREDICTORS = ncaa_train_Predictors.shape[1]

ncaa_train_class= np_utils.to_categorical(ncaa_train_class, CLASS_NUM)
ncaa_test_class = np_utils.to_categorical(ncaa_test_class, CLASS_NUM)

ncaaDNN, ncaaDNN_history = ncaa2018_model.ncaaDNN(ncaa_train_Predictors, ncaa_train_class, NUM_PREDICTORS, CLASS_NUM)
ncaaDNN.save('ncaaDNN.h5')
ncaa2018_model.SaveHistory(ncaaDNN_history, 'ncaaDNN_hist.txt')
ncaa2018_model.plotTrainingAcc(ncaaDNN_history, 'ncaaDNN Accuracy')
ncaa2018_model.plotTrainingLoss(ncaaDNN_history, 'ncaaDNN Loss')

ncaa2018_model.deepPredict(ncaaDNN, ncaa_test_Predictors, ncaa_test_class, NUM_PREDICTORS, CLASS_NUM)
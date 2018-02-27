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

modelpath = './ckpts/'
modelfile2014 = 'ncaa2014_categorical_crossentropy_50__20180226_1926_ep-115_va-0.94.ckpt'
modelfile2015 = 'ncaa2015_categorical_crossentropy_50__20180226_1936_ep-140_va-0.94.ckpt'
modelfile2016 = 'ncaa2016_categorical_crossentropy_50__20180226_1941_ep-70_va-0.94.ckpt'
modelfile2017 = 'ncaa2017_categorical_crossentropy_50__20180226_2000_ep-76_va-0.94.ckpt'

tourney2014 = np.loadtxt('./modeldata/tourney2014.csv', delimiter = ',', skiprows = 1)
tourney2015 = np.loadtxt('./modeldata/tourney2015.csv', delimiter = ',', skiprows = 1)
tourney2016 = np.loadtxt('./modeldata/tourney2016.csv', delimiter = ',', skiprows = 1)
tourney2017 = np.loadtxt('./modeldata/tourney2017.csv', delimiter = ',', skiprows = 1)

model2014 = load_model(modelpath+modelfile2014)
model2015 = load_model(modelpath+modelfile2015)
model2016 = load_model(modelpath+modelfile2016)
model2017 = load_model(modelpath+modelfile2017)

results2014 = model2014.predict(tourney2014)
results2015 = model2015.predict(tourney2015)
results2016 = model2016.predict(tourney2016)
results2017 = model2017.predict(tourney2017)

np.savetxt('./output/pred2014.csv', results2014[:,1], delimiter=',')
np.savetxt('./output/pred2015.csv', results2015[:,1], delimiter=',')
np.savetxt('./output/pred2016.csv', results2016[:,1], delimiter=',')
np.savetxt('./output/pred2017.csv', results2017[:,1], delimiter=',')
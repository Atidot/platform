import requests
from keras.models import Sequential
from tensorflow import keras
import ggplot #!platform ggplot
import decorators #!platform

@entrypoint
def process_table(df):
    return [[0,1,2],[1,2,3],[3,4,5]]

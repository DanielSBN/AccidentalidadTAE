import pandas as pd
import numpy as np
from sklearn.preprocessing import MinMaxScaler
from sklearn.cluster import KMeans
import matplotlib.pyplot as plt
import time


data=pd.read_excel("data_seg.xlsx", sheet_name='Hoja3')
data=data.fillna(0)
datan=data.drop(["Barrio","NOMBRE","CODIGO"],axis=1)

scaler = MinMaxScaler(feature_range=(0, 1))
data_norm= scaler.fit_transform(datan)


kmeans = KMeans(n_clusters=5).fit(data_norm)
centroids = kmeans.cluster_centers_
cc=kmeans.labels_

cltr={'cluster':cc}
cluster=pd.DataFrame(data=cltr)
data_fin=datan
data_fin['cluster']=cluster.loc[:,"cluster"]
data_fin['Barrio']=data.loc[:,"Barrio"]
data_fin['Codigo']=data.loc[:,"CODIGO"]
data_fin.to_excel("segemn.xlsx")
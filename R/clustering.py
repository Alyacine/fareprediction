import pandas as pd, numpy as np, matplotlib.pyplot as plt
from sklearn.cluster import DBSCAN
from geopy.distance import great_circle
from shapely.geometry import MultiPoint

df = pd.read_csv('data/save_train.csv')
to = 0.04 # input for DBSCAN max distance
coords = df.as_matrix(columns=['starting_latitude.1',
                               'starting_longitude.1'])
coords_tmp = df.as_matrix(columns=['ID.1'])
#, coords_tmp_9, coords_tmp_10, coords_tmp_11, coords_tmp_12, coords_tmp_13, coords_tmp_14, coords_tmp_15
coords_tmp_1, coords_tmp_2, coords_tmp_3, coords_tmp_4, coords_tmp_5, coords_tmp_6, coords_tmp_7, coords_tmp_8 = \
coords_tmp[:54717, :], coords_tmp[54717:109435, :], coords_tmp[109435:164153, :], \
coords_tmp[164153:218871, :], coords_tmp[218871:273589, :], coords_tmp[273589:328307, :], \
coords_tmp[328307:383025, :], coords_tmp[383025:, :]
#, coords_tmp[437743:492460, :], coords_tmp[492460:547177, :]\
#        , coords_tmp[547177:601894, :], coords_tmp[601894:656611, :], coords_tmp[656611:711328, :], coords_tmp[711328:766045, :], coords_tmp[766045:, :]

#, coords9, coords10, coords11, coords12, coords13, coords14, coords15
coords1, coords2, coords3, coords4, coords5, coords6, coords7, coords8 = \
coords[:54717, :], coords[54717:109435, :], coords[109435:164153, :],\
coords[164153:218871, :], coords[218871:273589, :], coords[273589:328307, :],\
coords[328307:383025, :], coords[383025:, :]
#, coords[437743:492460, :], coords[492460:547177, :]\
#        , coords[547177:601894, :], coords[601894:656611, :], coords[656611:711328, :], coords[711328:766045, :], coords[766045:, :]

a = [coords1, coords2,coords3, coords4,coords5 , coords6, coords7, coords8]
#, coords9, coords10, coords11, coords12, coords13, coords14, coords15]
b = [coords_tmp_1, coords_tmp_2,coords_tmp_3, coords_tmp_4,coords_tmp_5, coords_tmp_6,coords_tmp_7, coords_tmp_8]
#,coords_tmp_9,coords_tmp_10, coords_tmp_11,coords_tmp_12,coords_tmp_13, coords_tmp_14,coords_tmp_15]

for i, val in enumerate(a):
  coords = val
  kms_per_radian = 6371.0088
  epsilon = to / kms_per_radian
  print(epsilon)
  db = DBSCAN(eps=epsilon, min_samples=1, algorithm='ball_tree', metric='haversine').fit(np.radians(coords))
  cluster_labels = db.labels_
  num_clusters = len(set(cluster_labels))
  clusters = pd.Series([coords[cluster_labels == n] for n in range(num_clusters)])
  print('Number of clusters: {}'.format(num_clusters))
  
  coords_tmp = b[i]
  coords = pd.DataFrame(coords)
  coords_tmp = pd.DataFrame(coords_tmp)
  
  se = pd.Series(cluster_labels)
  coords['Cluster'] = se.values
  coords['ID'] = coords_tmp
  
  def get_centermost_point(cluster):
    centroid = (MultiPoint(cluster).centroid.x, MultiPoint(cluster).centroid.y)
  centermost_point = min(cluster, key=lambda point: great_circle(point, centroid).m)
  return tuple(centermost_point)
  centermost_points = clusters.map(get_centermost_point)
  
  lats, lons = zip(*centermost_points)
  rep_points = pd.DataFrame({'starting_longitude.1':lons, 'starting_latitude.1':lats})
  rs = rep_points.apply(lambda row: df[(df['starting_latitude.1']==row['starting_latitude.1']) & (df['starting_longitude.1']==row['starting_longitude.1'])].iloc[0], axis=1)
  
  coords.columns = ['starting_latitude.1', 'starting_longitude.1','Cluster','ID']
  rep_points.index.name = 'Cluster'
  rep_points.reset_index(inplace=True)
  coords = pd.merge(coords, rep_points, on=['Cluster'], how='left')
  
  if i == 0:
    my_data = pd.DataFrame(coords)
    res = pd.DataFrame(rs)
  else:
    res = pd.concat([res, rs])
    my_data = pd.concat([my_data, coords])

coords = res.as_matrix(columns=['starting_latitude.1', 'starting_longitude.1'])
coords2 = res.as_matrix(columns=['ID.1'])
kms_per_radian = 6371.0088
epsilon = to / kms_per_radian
db = DBSCAN(eps=epsilon, min_samples=1, algorithm='ball_tree', metric='haversine').fit(np.radians(coords))
cluster_labels = db.labels_
num_clusters = len(set(cluster_labels))
clusters = pd.Series([coords[cluster_labels == n] for n in range(num_clusters)])
print('Number of clusters: {}'.format(num_clusters))

coords = pd.DataFrame(coords)
coords2 = pd.DataFrame(coords2)
coords['ID'] = coords2
se = pd.Series(cluster_labels)
coords['Cluster'] = se.values

coords.columns = ['starting_latitude.1_y', 'starting_longitude.1_y','Cluster','ID']

Data = pd.merge(my_data, coords, on=['starting_latitude.1_y', 'starting_longitude.1_y'], how='left')
Data.to_csv('data/Clustering_%s.csv' %(to), index=False, header=True)



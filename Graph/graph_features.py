# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import numpy as npy
from array import array
import networkx as nx
import matplotlib.pyplot as plt
import networkx.algorithms.isomorphism as iso
import os 
import time
import multiprocessing as mp


def feature_peer(graph):
    return(list(nx.connected_component_subgraphs(graph)))


def feature_eigenvector(graph):
    y=nx.eigenvector_centrality(graph, max_iter=1000, tol=.009)
    return(y)

def feature_degree(graph):
    y=nx.degree_centrality(graph)
    return(y)

edge1 = pd.read_csv('TEST_GR.csv')
node1 =  pd.read_csv('TEST_GR1.csv')

G=nx.Graph()

for i in range(0,edge1['NODE_FROM'].count()):
    G.add_edge(edge1['NODE_FROM'].iloc[i],edge1['NODE_TO'].iloc[i])

#nx.draw(G,with_labels=True)
    
##Getting connected subgraphs    
graphs = feature_peer(G)

tmp=pd.DataFrame(columns=["i","len_i"])

##Getting length of connected subgraphs
for i in range(0,len(graphs)):
    tmp=tmp.append(pd.DataFrame([[i,len(graphs[i])]],columns=["i","len_i"]))
    

no_cores = mp.cpu_count()
p = mp.Pool(no_cores)

len_graph = list(range(0,len(graphs)))

###Eigen feature
dic_feature_eigen = [p.map(feature_eigenvector, [graphs[item]]) for item in len_graph ]
dic_feature_eigen1 = [item for sublist in dic_feature_eigen for item in sublist]


finalMap = {}
for d in dic_feature_eigen1:
    finalMap.update(d)

eigen = pd.DataFrame.from_dict(finalMap, orient = 'index').reset_index()
eigen.columns  = ['Node', 'eigen_vector']

###Deggre feature
dic_feature_degree = [p.map(feature_degree, [graphs[item]]) for item in len_graph ]
dic_feature_degree1 = [item for sublist in dic_feature_degree for item in sublist]


finalMap = {}
for d in dic_feature_degree1:
    finalMap.update(d)

degree = pd.DataFrame.from_dict(finalMap, orient = 'index').reset_index()
degree.columns  = ['Node', 'degree']


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

def feature_betweeness(graph):
    y=nx.betweenness_centrality(graph, k=1, normalized=True, weight=None, endpoints=True, seed=None)
    return(y)




edge1 = pd.read_csv('TEST_GR.csv')
edge = pd.read_csv('TEST_GR.csv')
edge.columns = ['from', 'to', 'wt']
node1 =  pd.read_csv('TEST_GR1.csv')
node = pd.read_csv('TEST_GR1.csv')
node.columns = ['id', 'flag']

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

###Degree feature
dic_feature_degree = [p.map(feature_degree, [graphs[item]]) for item in len_graph ]
dic_feature_degree1 = [item for sublist in dic_feature_degree for item in sublist]


finalMap = {}
for d in dic_feature_degree1:
    finalMap.update(d)

degree = pd.DataFrame.from_dict(finalMap, orient = 'index').reset_index()
degree.columns  = ['Node', 'degree']


###Betweeness feature
dic_feature_bet = [p.map(feature_betweeness, [graphs[item]]) for item in len_graph ]
dic_feature_bet1 = [item for sublist in dic_feature_bet for item in sublist]


finalMap = {}
for d in dic_feature_bet1:
    finalMap.update(d)

bet = pd.DataFrame.from_dict(finalMap, orient = 'index').reset_index()
bet.columns  = ['Node', 'bet']

##### Add shortest path segment##########



##Cycle (heuristic) - WIP ##########
k = 0
c = []
d = []
j=0
all_cycle = []
aks = 0

while(aks<10):
    edge2 = edge[['from','to']]
    node2 = node[['id']]
    edge2=edge2.loc[:,('from','to','flag')]
    edge1 = edge2
    node2=node2.loc[:,('id','flag')]
    if (aks == 0):
        node1 = node2
    else:
        node1 = node2.iloc[npy.random.permutation(len(node2))]
    for q in range(0,node1['id'].count()):
        if node1['flag'].iloc[q] != 1:
            l = node1['id'].iloc[q]
            c = c + [l]
            j=0
            while(1):
                while (j<edge1['from'].count()):
                    if ((((l == edge1['from'].iloc[j]) and (edge1['flag'].iloc[j] !=1)) or ((l == edge1['to'].iloc[j]) and (edge1['flag'].iloc[j] !=1))) and node1['flag'].iloc[q]!=1):
                        if ((l == edge1['from'].iloc[j]) and (edge1['flag'].iloc[j] !=1)):
                            l = edge1['to'].iloc[j]
                        else:
                            l = edge1['from'].iloc[j]
                        c = c + [l]       
                        edge1.loc[j,('flag')]=1
                        k = k + 1
                        j = 0
                        if len(c) != len(set(c)):
                            print("cycle : ")
                            print(c)
                            all_cycle.append(list(c)) 
                            del c[-1]
                            k = k - 1
                            l = c[-1]
                    else:
                        j = j+1
                if ((k-1)>=0):
                    d = list( set (c + d))
                    del c[-1]
                    l = c[k-1]                
                    k = k - 1                
                else:
                    break
        for i in range(0,node1['id'].count()):
            for m in range(0,len(d)):
                if (d[m] == node1['id'].iloc[i]):
                    node1.set_value(i, 'flag', 1)
        for i in range(0,len(c)):
            del c[-1]
    aks = aks + 1
    
l = []
flag = 1
for i in range(0,len(all_cycle)):
    l.append(-1)
    
for i in range(0,len(all_cycle)):
    for j in range(i+1,len(all_cycle)):
        if (len(all_cycle[i]) == len(all_cycle[j])):
            for k in range(0,len(all_cycle[i])):
                if (sorted(all_cycle[i])[k] != sorted(all_cycle[j])[k]):
                    flag = 0
                    break
                else:
                    flag = 1
            if (flag == 1):
                l[j] = j
for i in sorted(l, reverse=True):
    if (i != -1):
        del all_cycle[i]
all_cycle
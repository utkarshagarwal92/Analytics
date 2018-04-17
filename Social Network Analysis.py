# -*- coding: utf-8 -*-
"""
Created on Feb 16 2018
SNA - Product Recommender System
##Update Log:
~~~~~~~~~~~~~
## 02/16/2018 - Utkarsh Agarwal - Inserted code for giving out Top-5 recommendations for a given product

"""
print ()

import networkx
from operator import itemgetter
import matplotlib.pyplot

# Read the data from the amazon-books.txt;
# populate amazonProducts nested dicitonary;
# key = ASIN; value = MetaData associated with ASIN
fhr = open('******/amazon-books.txt', 'r', encoding='utf-8', errors='ignore')
amazonBooks = {}
fhr.readline()
for line in fhr:
    cell = line.split('\t')
    MetaData = {}
    MetaData['Id'] = cell[0].strip() 
    ASIN = cell[1].strip()
    MetaData['Title'] = cell[2].strip()
    MetaData['Categories'] = cell[3].strip()
    MetaData['Group'] = cell[4].strip()
    MetaData['SalesRank'] = int(cell[5].strip())
    MetaData['TotalReviews'] = int(cell[6].strip())
    MetaData['AvgRating'] = float(cell[7].strip())
    MetaData['DegreeCentrality'] = int(cell[8].strip())
    MetaData['ClusteringCoeff'] = float(cell[9].strip())
    amazonBooks[ASIN] = MetaData
fhr.close()
#print (amazonBooks)
# node = ASIN, edge= copurchase, edge weight = category similarity
fhr=open("******/amazon-books-copurchase.edgelist", 'rb')
copurchaseGraph=networkx.read_weighted_edgelist(fhr)
fhr.close()

# Now let's assume a person is considering buying the following book;
# what else can we recommend to them based on copurchase behavior 
# we've seen from other users?
print ("Looking for Recommendations for Customer Purchasing this Book:")
print ("--------------------------------------------------------------")
purchasedAsin = '0805047905'

# Let's first get some metadata associated with this book
print ("ASIN = ", purchasedAsin) 
print ("Title = ", amazonBooks[purchasedAsin]['Title'])
print ("SalesRank = ", amazonBooks[purchasedAsin]['SalesRank'])
print ("TotalReviews = ", amazonBooks[purchasedAsin]['TotalReviews'])
print ("AvgRating = ", amazonBooks[purchasedAsin]['AvgRating'])
print ("DegreeCentrality = ", amazonBooks[purchasedAsin]['DegreeCentrality'])
print ("ClusteringCoeff = ", amazonBooks[purchasedAsin]['ClusteringCoeff'])
# Now let's look at the ego network associated with purchasedASIN in the
# copurchaseGraph - which is esentially comprised of all the books 
# that have been copurchased with this book in the past
#purchasedAsinEgoGraph = networkx.Graph(copurchaseGraph, radius = 1)
purchasedAsinEgoGraph = networkx.ego_graph(copurchaseGraph, purchasedAsin, radius=1)

#pos = networkx.spring_layout(copurchaseGraph)  
#matplotlib.pyplot.figure(figsize=(10,10))
#networkx.draw_networkx_labels(copurchaseGraph,pos,font_size=20)
#networkx.draw(copurchaseGraph, pos=pos, node_size=15, node_color='r', edge_color='r', style='dashed')
#networkx.draw(purchasedAsinEgoGraph, pos=pos, node_size=10, node_color='b', edge_color='b', style='solid')
#matplotlib.pyplot.show()

threshold = 0.5
purchasedAsinEgoTrimGraph = networkx.Graph()
#gIslands = networkx.Graph()
for f, t, e in purchasedAsinEgoGraph.edges(data=True):
#    print ('f',f,'t', t, 'e',e)
    if e['weight'] >= threshold:
        purchasedAsinEgoTrimGraph.add_edge(f,t,**e)

#purc_asin_neighbors = {}
purchasedAsinNeighbors = []
for f, t, e in purchasedAsinEgoTrimGraph.edges(data = True):
    if f == purchasedAsin:
        purchasedAsinNeighbors.append(t)
#        purc_asin_neighbors[t] = e
#print (purchasedAsinNeighbors, len(purchasedAsinNeighbors))
#print (purc_asin_neighbors)
#purchasedAsinNeighbors = [purchasedAsinEgoTrimGraph.adj.items()]
#print (purchasedAsinNeighbors)

#print (amazonBooks[purchasedAsin])
#['0399230130', '0688148999', '0399226842', '0694006246']
#NestedDICT
#{'0152010661': {'weight': 0.5},
#main DICT
#{'Id': '502784', 'Title': 'Brown Bear, Brown Bear, What Do You See?',
# 'Categories': 'general author color babi staff specialti illustr children carl
# subject concept book fiction z pick basic board c bear eric anim music store',
# 'Group': 'Book', 'SalesRank': 171, 'TotalReviews': 172, 'AvgRating': 5.0,
# 'DegreeCentrality': 216, 'ClusteringCoeff': 0.65}
ranker = {}
rev_rate = {}
composite_measure = {}
for i in purchasedAsinNeighbors:
    if i in amazonBooks.keys():
#        print (amazonBooks[i])
#        break
#        ranker[i] = amazonBooks[i]['SalesRank']
        composite_measure[i] = (((1000 *(1/(amazonBooks[i]['SalesRank']))) * (pow(((amazonBooks[i]['DegreeCentrality']) * 0.6) * ((amazonBooks[i]['ClusteringCoeff']) * 0.4),2)) * pow((amazonBooks[i]['TotalReviews']) / ((amazonBooks[i]['AvgRating']) + 1),2)))
#        rev_rate[i] = (((amazonBooks[i]['TotalReviews']) * (amazonBooks[i]['AvgRating']))+1)
#composite_measure[i] = (((1/(amazonBooks[i]['SalesRank'])) 
#+ (((amazonBooks[i]['DegreeCentrality'])/100)*(amazonBooks[i]['ClusteringCoeff']))) 
#* ((amazonBooks[i]['AvgRating']) * (amazonBooks[i]['TotalReviews'])))
#for i in composite_measure.keys():
#    print (composite_measure[i])
#print ()
#print (ranker)

pointers = {}
sorted_measures = sorted (composite_measure.items(), key = itemgetter(1), reverse = True)
ordered_Measures = dict((x, y) for x, y in sorted_measures)
max_measure = 0
min_measure = 0
print ()
top5_measures = {}
top = 5
#print ('---------------------------------------------------------------------')
for measures in ordered_Measures.keys():
    if top > 0:
        top5_measures[measures] = ordered_Measures[measures]
        top = top - 1
    
#print (top5_measures.values())
#print ('---------------------------------------------------------------------')
for i in top5_measures.keys():
    max_measure = top5_measures[i]
    break
for i in top5_measures.keys():
#    print (ordered_Measures[i])
    min_measure = top5_measures[i]
print ()
#print (max_measure, 'max measure')
#print ()
#print (min_measure, 'min measure')

#for i in ordered_Measures.keys():
#    print (i, ordered_Measures[i])

normalized_measures = {}

counter = 1
print ('--------------------------------------------------------------')
print ('Recommendations for Customer purchasing:', purchasedAsin,'are as follows \n')
print ('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
for i in top5_measures.keys():
    if i in amazonBooks.keys():
        print ('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
        print ('RECOMMENDATION:',counter)
        print ('PRODUCT ASIN:',i)
        print ('BOOK TITLE:',amazonBooks[i]['Title'])
        print ('SALES-RANK',  amazonBooks[i]['SalesRank'])
        print ('TOTAL REVIEWS:', amazonBooks[i]['TotalReviews'])
        print ('AVERAGE RATING:', amazonBooks[i]['AvgRating'])
        print ('DEGREE-CENTRALITY:', amazonBooks[i]['DegreeCentrality'])
        print ('CLUSTERING COEFFICIENT:', amazonBooks[i]['ClusteringCoeff'], '\n')
        print ('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
    counter += 1
print ('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
print ('--------------------------------------------------------------')



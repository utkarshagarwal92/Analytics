# -*- coding: utf-8 -*-
"""
Assignment 3
"""

import math

from operator import itemgetter



#################################################
# recommender class does user-based filtering and recommends items 
class UserBasedFilteringRecommender:
    
    # class variables:    
    # none
    
    ##################################
    # class instantiation method - initializes instance variables
    #
    # usersItemRatings:
    # users item ratings data is in the form of a nested dictionary:
    # at the top level, we have User Names as keys, and their Item Ratings as values;
    # and Item Ratings are themselves dictionaries with Item Names as keys, and Ratings as values
    # Example: 
    #     {"Angelica":{"Blues Traveler": 3.5, "Broken Bells": 2.0, "Norah Jones": 4.5, "Phoenix": 5.0, "Slightly Stoopid": 1.5, "The Strokes": 2.5, "Vampire Weekend": 2.0},
    #      "Bill":{"Blues Traveler": 2.0, "Broken Bells": 3.5, "Deadmau5": 4.0, "Phoenix": 2.0, "Slightly Stoopid": 3.5, "Vampire Weekend": 3.0}}
    #
    # k:
    # the number of nearest neighbors
    # defaults to 1
    #
    # m:
    # the number of recommedations to return
    # defaults to 10
    #
    def __init__(self, usersItemRatings, metric='pearson', k=1, m=10):
        
        # set self.usersItemRatings
        self.usersItemRatings = usersItemRatings
            
        # set self.k
        if k > 0:   
            self.k = k
        else:
            print ("    (FYI - invalid value of k (must be > 0) - defaulting to 1)")
            self.k = 1
         
        # set self.m
        if m > 0:   
            self.m = m
        else:
            print ("    (FYI - invalid value of m (must be > 0) - defaulting to 10)")
            self.m = 10
            

    #################################################
    # pearson correlation similarity
    # notation: if UserX is Angelica and UserY is Bill, then:
    # userXItemRatings = {"Blues Traveler": 3.5, "Broken Bells": 2.0, "Norah Jones": 4.5, "Phoenix": 5.0, "Slightly Stoopid": 1.5, "The Strokes": 2.5, "Vampire Weekend": 2.0}
    # userYItemRatings = {"Blues Traveler": 2.0, "Broken Bells": 3.5, "Deadmau5": 4.0, "Phoenix": 2.0, "Slightly Stoopid": 3.5, "Vampire Weekend": 3.0}
    def pearsonFn(self, userXItemRatings, userYItemRatings):
        
        sum_xy = 0
        sum_x = 0
        sum_y = 0
        sum_x2 = 0
        sum_y2 = 0
        
        n = len(userXItemRatings.keys() & userYItemRatings.keys())
        
        for item in userXItemRatings.keys() & userYItemRatings.keys():
            x = userXItemRatings[item]
            y = userYItemRatings[item]
            sum_xy += x * y
            sum_x += x
            sum_y += y
            sum_x2 += pow(x, 2)
            sum_y2 += pow(y, 2)
       
        if n == 0:
            print ("    (FYI - personFn n==0; returning -2)")
            return -2
        
        denominator = math.sqrt(sum_x2 - pow(sum_x, 2) / n) * math.sqrt(sum_y2 - pow(sum_y, 2) / n)
        if denominator == 0:
            print ("    (FYI - personFn denominator==0; returning -2)")
            return -2
        else:
            return round((sum_xy - (sum_x * sum_y) / n) / denominator, 2)
            

    #################################################
    # make recommendations for userX from the most similar k nearest neigibors (NNs)
    def recommendKNN(self, userX):
        
        # YOUR CODE HERE
        # for given userX, get the sorted list of users - by most similar to least similar        
        weights = {}
        recommendations = {}
        recommenders = {}
        adjustedWeights = {}
        divisor = 0
        j = 0
        new_ratings = 0
        songsX = []
        knn_recommenders = {}
        rate_songs = {}
#        recommended_user = ''
        for i in self.usersItemRatings.keys():
            if i != userX:
                dict1 = self.usersItemRatings[userX]
                dict2 = self.usersItemRatings[i]
                correl = self.pearsonFn(dict1, dict2)
                weights[i] = correl
#        print (weights)
#        
        sorted_weights = sorted (weights.items(), key = itemgetter(1), reverse = True)
        orderedWeights = dict((x, y) for x, y in sorted_weights)
#        print (adjustedWeights)
        k = self.k - 1
        if self.k == 1:
            for recommender in orderedWeights.keys():
                recommended_user = recommender
                break
        if self.k == 1:
            for key in self.usersItemRatings[recommended_user].keys():
                if key not in self.usersItemRatings[userX].keys():
                    recommendations[key] = self.usersItemRatings[recommended_user][key]
            for user in recommenders.keys():
                for key in self.usersItemRatings[user].keys():
                    if key not in self.usersItemRatings[userX].keys():
                        recommendations[key] = self.usersItemRatings[user].key()  
        
        ###################################################################################
        ## NEW BEGINNINGS.... K NEAREST NEIGHBORS...
        for recommender in orderedWeights.keys():
            if j <= k:
#                print (orderedWeights[recommender])
                recommenders[recommender] = orderedWeights[recommender]
#                print ('post assignment:', recommenders[recommender])
                j = j + 1
#                recommender in adjustedWeights.keys():
#                recommenders.append(recommender)
#                continue
                
#        print ('____________________')
#        print ('REC', recommenders)
#        print ('____________________')
##### UPDATING THE PEARSON CORRELATION VALUES TO PC' AND CALCULATIONS PER THE ADJUSTED WEIGHTS
        for users in recommenders.keys():
            recommenders[users] = (recommenders[users] + 1) / 2
#        print ('____________________')
#        print (recommenders)
#        print ('____________________')
        for users in recommenders.keys():
            divisor = divisor + recommenders[users]
#        print ('____________________')
#        print (divisor)
#        print ('____________________')
        for users in recommenders.keys():
            adjustedWeights[users] = recommenders[users] / divisor
#        print ('____________________')
#        print ('WEIGHTS:',adjustedWeights)
#        print ('____________________')
##### NOW WE HAVE TO MULTIPLY THE RATINGS FOR EACH OF THESE USERS WITH THEIR ADJ_WEIGHTS FOR THE userX!!!
        for users in self.usersItemRatings.keys():
            for songs in self.usersItemRatings[users].keys():
                if songs not in songsX:
                    songsX.append(songs)
#        print (songsX)
        if self.k > 1:
            for songs in songsX:
                temp=0
                for users in adjustedWeights.keys():
                    if songs in self.usersItemRatings[users].keys():
                        if songs not in self.usersItemRatings[userX].keys():
                            
#                            print ('_____________',songs, temp)
                            temp = temp + (adjustedWeights[users] * self.usersItemRatings[users][songs])
#                            print ('_____________',songs, temp)
                if (temp!=0):
                    recommendations[songs] = round(temp,2)            
                
                    
#                        print (songs)
                

#        if self.k > 1:
#            for users in self.usersItemRatings.keys(): #LIST OF ALL USERS
#                if users in adjustedWeights.keys(): #ALL USERS THAT ARE TOP RECOMMENDERS
##                for common_users in adjustedWeights.keys(): 
#                    for songs in self.usersItemRatings[users].keys(): #ALL SONGS RATED BY COMMON USERS
#                        if songs not in self.usersItemRatings[userX].keys(): #SONGS THAT ARE NOT RATED BY userX
##                            knn_recommenders[users] = adjustedWeights[users] * self.usersItemRatings[users][songs]
##                            if songs not in knn_recommenders.keys():
##                                knn_recommenders[songs] = self.usersItemRatings[users][songs]
##                            if users not in rate_songs.keys():
##                                rate_songs[users] = knn_recommenders
##                            if songs not in songsX:
##                                songsX.append(songs)
##                            for songs in songsX:
##                                new_ratings = new_ratings + (adjustedWeights[users] * self.usersItemRatings[users][songs])
##                                recommendations[songs] = new_ratings
##                            print ('----------------')
##                            print (songsX)
##                            print (knn_recommenders)
##                            rate_songs[songs] = knn_recommenders
##                            for songs in self.usersItemRatings[users].keys():
##                            print (users, adjustedWeights[users], self.usersItemRatings[users][songs])
##                            continue
#                            new_ratings = new_ratings + (adjustedWeights[users] * self.usersItemRatings[users][songs])
#                            knn_recommenders[users] = new_ratings
#                            rate_songs[songs] = knn_recommenders
##                            print (rate_songs)
##                            print (users, songs, new_ratings)
##                            print ('----------------')
#                            recommendations[songs] = new_ratings
##            print('yo baby',knn_recommenders)
##                        print ('--------------------------',rate_songs)
##            print (knn_recommenders)
##            print (rate_songs)
##            for users in knn_recommenders.keys():
                
                    
                    
#                new_ratings = new_ratings + (adjustedWeights[common_users] * self.usersItemRatings[common_users][]
                
        
        
#        print ('____________________')
#        print ('USR', userX)
#        print ('ORD', orderedWeights)
#        print ('ADJ', adjustedWeights)
#        print ('____________________')
#        if self.k == 1:
#            for user in recommenders.keys():
#                for key in self.usersItemRatings[user].keys():
#                    if key not in self.usersItemRatings[userX].keys():
#                        recommendations[key] = self.usersItemRatings[user].key()                       
        return recommendations     
#
##y = 'Veronica'
###x = 'Angelica'
##k = 1
##if k == 1:
##    for i in songData.keys():
##                recommended_user = i
##                break
##print (recommended_user)
##
##dictX = {}
###for i in songData[y].keys():
###    print (i)
##
##for key in songData[recommended_user].keys(): #and songData[y].keys():
###    print (i)
##    if key not in songData[y].keys():
##        dictX[key] = songData[recommended_user][key]
##print (dictX)

##        for i in adjustedWeights.keys() and self.usersItemRatings.keys():
#            print ('in the first for loop:',i)
#            if self.k == 1:
#                print ('accessd the if statement successfully...')
#                for j in self.userItemRatings[i].keys():
#                    print ('inside the inner most for loop...')
#                    recommendations[i] = self.usersItemRatings[i][j]
#            break
#        print (recommendations)

#        for i,j in sorted_weights:
#            if self.k == 1:
#                for i in userX.keys():
#                    return (sorted_weights[i][j])
#                break
                    
#        return (sorted_weights[i])
            
#        print (weights)
        # calcualte the weighted average item recommendations for userX from userX's k NNs
        
        # return sorted list of recommendations (sorted highest to lowest ratings)
        # example: [('Broken Bells', 2.64), ('Vampire Weekend', 2.2), ('Deadmau5', 1.71)]
        
        # once you are done coding this method, delete the pass statement below
#        pass
#songData = {"Angelica": {"Blues Traveler": 3.5, "Broken Bells": 2.0, "Norah Jones": 4.5, "Phoenix": 5.0, "Slightly Stoopid": 1.5, "The Strokes": 2.5, "Vampire Weekend": 2.0},
#         "Bill":{"Blues Traveler": 2.0, "Broken Bells": 3.5, "Deadmau5": 4.0, "Phoenix": 2.0, "Slightly Stoopid": 3.5, "Vampire Weekend": 3.0},
#         "Chan": {"Blues Traveler": 5.0, "Broken Bells": 1.0, "Deadmau5": 1.0, "Norah Jones": 3.0, "Phoenix": 5, "Slightly Stoopid": 1.0},
#         "Dan": {"Blues Traveler": 3.0, "Broken Bells": 4.0, "Deadmau5": 4.5, "Phoenix": 3.0, "Slightly Stoopid": 4.5, "The Strokes": 4.0, "Vampire Weekend": 2.0},
#         "Hailey": {"Broken Bells": 4.0, "Deadmau5": 1.0, "Norah Jones": 4.0, "The Strokes": 4.0, "Vampire Weekend": 1.0},
#         "Jordyn":  {"Broken Bells": 4.5, "Deadmau5": 4.0, "Norah Jones": 5.0, "Phoenix": 5.0, "Slightly Stoopid": 4.5, "The Strokes": 4.0, "Vampire Weekend": 4.0},
#         "Sam": {"Blues Traveler": 5.0, "Broken Bells": 2.0, "Norah Jones": 3.0, "Phoenix": 5.0, "Slightly Stoopid": 4.0, "The Strokes": 5.0},
#         "Veronica": {"Blues Traveler": 3.0, "Norah Jones": 5.0, "Phoenix": 4.0, "Slightly Stoopid": 2.5, "The Strokes": 3.0}
#        }
#y = 'Veronica'
##x = 'Angelica'
#k = 1
#if k == 1:
#    for i in songData.keys():
#                recommended_user = i
#                break
#print (recommended_user)
#
#dictX = {}
##for i in songData[y].keys():
##    print (i)
#
#for key in songData[recommended_user].keys(): #and songData[y].keys():
##    print (i)
#    if key not in songData[y].keys():
#        dictX[key] = songData[recommended_user][key]
#print (dictX)
#     if i not in songData[y].keys():
#        dictX[i] = songData[i]
#print (dictX)

#import math
#def pearsonF(userXItemRatings, userYItemRatings):
#    sum_xy = 0
#    sum_x = 0
#    sum_y = 0
#    sum_x2 = 0
#    sum_y2 = 0
#    n = len(userXItemRatings.keys() & userYItemRatings.keys())
#    for item in userXItemRatings.keys() & userYItemRatings.keys():
#        x = userXItemRatings[item]
#        y = userYItemRatings[item]
#        sum_xy += x * y
#        sum_x += x
#        sum_y += y
#        sum_x2 += pow(x, 2)
#        sum_y2 += pow(y, 2)
#       
#    if n == 0:
#        print ("    (FYI - personFn n==0; returning -2)")
#        return -2
#        
#    denominator = math.sqrt(sum_x2 - pow(sum_x, 2) / n) * math.sqrt(sum_y2 - pow(sum_y, 2) / n)
#    if denominator == 0:
#        print ("    (FYI - personFn denominator==0; returning -2)")
#        return -2
#    else:
#        return round((sum_xy - (sum_x * sum_y) / n) / denominator, 2)
#
#
#
#user = 'Angelica'
#weights = {}
#Nweights = {}
##dict1 = {}
##dict2 = {}
#for i in songData.keys():
#    if i != user:
#        dict1 = songData[user]
#        dict2 = songData[i]
##        print (dict1)
##        print ()
##        print (dict2)
##        break
##        print (dict1.keys())
##        print (dict2.keys())
##        break
#        correl = pearsonF(dict1, dict2)
#        weights[i] = correl
#        Nweights[user] = weights
#print (weights)
#print ()
#print (Nweights)
##        print (i)
##        print (songData[i])
###        print (songData[user])
#ubf = UserBasedFilteringRecommender(songData)
#print ("------------------------------")
#print ("UBF NN Pearson Recommendations")
#print ("------------------------------")
#for user in songData.keys():
#    print(user, ":", ubf.recommendKNN(user))
#
#print ()
#
##dict1 = {'a':1,'b':2}
##dict1['c'] = 2
##print(dict1)
#        

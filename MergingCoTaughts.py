# -*- coding: utf-8 -*-
"""
Created on Sat Sep 12 17:25:52 2015

@author: Mom
"""
import os
import pandas as pd
os.chdir("C:\\Users\\Mom\\Documents\\Junior 1st sem\\STAT 399 V2")
# seems useful at some point: http://www.datacarpentry.org/python-ecology/04-merging-data
# also good: http://manishamde.github.io/blog/2013/03/07/pandas-and-python-top-10/
# real, real good count basie: https://www.youtube.com/watch?v=5XDGIT-djGo
#%%
os.chdir("C:\\Users\\Mom\\Documents\\Junior 1st sem\\STAT 399 V2\\Co-Taughts")
dfOfDuplicateListings=pd.read_csv('dfOfDuplicateListings20150907.csv')
dfOfDuplicateListings=dfOfDuplicateListings.drop('Unnamed: 0',1)
df20150912=pd.read_csv("df20150912.csv")
df20150912=df20150912.drop('Unnamed: 0',1)
FinaldfV1=df20150912[-df20150912['Unique.ID'].isin(dfOfDuplicateListings['Unique.ID'])]
#%%
os.chdir("C:\\Users\\Mom\\Documents\\Junior 1st sem\\STAT 399 V2")
FinaldfV1.to_excel('FinaldfV1.xlsx')
#%%
def Find_Dual_CoTaughts_Condense_To_One(dfOfDuplicateListings):
    IDFrame=dfOfDuplicateListings['Unique.ID']
    repeatedIDs=set(IDFrame)
    dfOfCoTaughts=pd.DataFrame()
    counter=0
    for ID in repeatedIDs:
        ListofOnes=[] #a one will be appended if it's different
        dfToCheck=dfOfDuplicateListings.loc[(dfOfDuplicateListings['Unique.ID']==ID)]
        if len(dfToCheck)==2:
            for j in range(len(dfToCheck.columns)-1):
                if dfToCheck.iloc[0,j]==dfToCheck.iloc[1,j]:
                    ListofOnes.append(0)
                else:
                    ListofOnes.append(1)
        if sum(ListofOnes)==5:
            '''
            for j in range(len(dfToCheck.columns)-1):
                dfToConcat=pd.DataFrame()
                if type(dfToCheck.iloc[0,j])&type(dfToCheck.iloc[1,j])==str:
                    dfToConcat[0,j]=dfToCheck[0,j]
            '''
            dfToConcat=pd.DataFrame(index=[counter], columns=dfToCheck.columns)
            for j in dfToCheck.columns:
                if type(dfToCheck[[j]].iloc[0][0])==str and type(dfToCheck[[j]].iloc[1][0])==str:
                    if j == 'instructor':
                        dfToConcat[j].loc[counter]=dfToCheck[[j]].iloc[0][0] + '/' + dfToCheck[[j]].iloc[1][0]
                    else:
                        dfToConcat[j].loc[counter]=dfToCheck[[j]].iloc[1][0]            
            ########except the prof name column
                
                else:
                    try:
                        dfToConcat[j].loc[counter]=(dfToCheck[[j]].iloc[0][0]+dfToCheck[[j]].iloc[1][0])/2        
                    except: Exception
            dfOfCoTaughts=pd.concat([dfOfCoTaughts,dfToConcat],axis=0)
            counter += 1 
    return dfOfCoTaughts
#%%
def Find_Triple_CoTaughts_Condense_To_One(dfOfDuplicateListings):
    IDFrame=dfOfDuplicateListings['Unique.ID']
    repeatedIDs=set(IDFrame)
    dfOfCoTaughts=pd.DataFrame()
    counter=0
    for ID in repeatedIDs:
        ListofOnes=[] #a one will be appended if it's different
        dfToCheck=dfOfDuplicateListings.loc[(dfOfDuplicateListings['Unique.ID']==ID)]
        if len(dfToCheck)==3:
            for j in range(len(dfToCheck.columns)-1):
                if dfToCheck.iloc[0,j]==dfToCheck.iloc[1,j]:
                    ListofOnes.append(0)
                else:
                    ListofOnes.append(1)
        if sum(ListofOnes)==5:
            '''
            for j in range(len(dfToCheck.columns)-1):
                dfToConcat=pd.DataFrame()
                if type(dfToCheck.iloc[0,j])&type(dfToCheck.iloc[1,j])==str:
                    dfToConcat[0,j]=dfToCheck[0,j]
            '''
            dfToConcat=pd.DataFrame(index=[counter], columns=dfToCheck.columns)
            for j in dfToCheck.columns:
                if type(dfToCheck[[j]].iloc[0][0])==str and type(dfToCheck[[j]].iloc[1][0])==str and type(dfToCheck[[j]].iloc[2][0])==str:
                    if j == 'instructor':
                        dfToConcat[j].loc[counter]=dfToCheck[[j]].iloc[0][0] + '/' + dfToCheck[[j]].iloc[1][0] + '/' + dfToCheck[[j]].iloc[2][0]
                    else:
                        dfToConcat[j].loc[counter]=dfToCheck[[j]].iloc[2][0]            
            ########except the prof name column
                
                else:
                    try:
                        dfToConcat[j].loc[counter]=(dfToCheck[[j]].iloc[0][0]+dfToCheck[[j]].iloc[1][0]+dfToCheck[[j]].iloc[2][0])/3        
                    except: Exception
            dfOfCoTaughts=pd.concat([dfOfCoTaughts,dfToConcat],axis=0)
            counter += 1 
    return dfOfCoTaughts

#%% Executing the two functions
dfofdualcotaughts=Find_Dual_CoTaughts_Condense_To_One(dfOfDuplicateListings)
dfoftriplecotaughts=Find_Triple_CoTaughts_Condense_To_One(dfOfDuplicateListings)
#%%
dfofdualcotaughts.to_excel('dfofdualcotaughts.xlsx')
dfoftriplecotaughts.to_excel('dfoftriplecotaughts.xlsx')
#%%
dfOfSeeminglyErroneous=dfOfDuplicateListings[-dfOfDuplicateListings['Unique.ID'].isin(dfofdualcotaughts['Unique.ID'])]
dfOfSeeminglyErroneous=dfOfSeeminglyErroneous[-dfOfDuplicateListings['Unique.ID'].isin(dfoftriplecotaughts['Unique.ID'])]
#%%
dfOfSeeminglyErroneous.to_excel('dfOfSeeminglyErroneous.xlsx')

#-dfOfDuplicateListings['Unique.ID'].isin(dfoftriplecotaughts['Unique.ID'])]
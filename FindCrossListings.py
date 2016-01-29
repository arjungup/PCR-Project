# -*- coding: utf-8 -*-
"""
Created on Wed Sep 16 20:33:13 2015

@author: Mom
"""
import pandas as pd
import numpy as np
import os
os.chdir("C:\\Users\\Mom\\Documents\\Junior 1st sem\\STAT 399 V2")
# side note, writing columns by formulas in pandas http://stackoverflow.com/questions/7837722/what-is-the-most-efficient-way-to-loop-through-dataframes-with-pandas
#%% cross-listings reference sheet
crosslistingreference=pd.read_excel('247classes20150905.xlsx',sheetname='cross-listings from 247')
#testframe=crosslistingreference.iloc[0:3,] # test iterating through rows
#%% Collapse rows into ones only with crosslistings
def findRowsWithCrossListings(crosslistingreference):
    IDsWithCrossListings=[]
    for i in range(len(crosslistingreference)):
        if pd.isnull(crosslistingreference.iloc[i,1]) and pd.isnull(crosslistingreference.iloc[i,2]) and pd.isnull(crosslistingreference.iloc[i,3]) and pd.isnull(crosslistingreference.iloc[i,4]):
            pass
        else:
            IDsWithCrossListings.append(crosslistingreference.iloc[i,0])
    return IDsWithCrossListings,crosslistingreference[crosslistingreference['Primary Alias'].isin(IDsWithCrossListings)]

IDsWithCrosslistings,dfOfCrossListedsAndNans=findRowsWithCrossListings(crosslistingreference)
#%%
dfOfCrossListedsAndNans.to_excel('dfOfCrossListedsAndNans.xlsx') 
#%%
os.chdir("C:\\Users\\Mom\\Documents\\Junior 1st sem\\STAT 399 V2\\Cross-Listings")
dfOfCrossListedsAndNansV2=pd.read_excel('dfOfCrossListedsAndNansV2.xlsx')
#%% wants hardcoded numbers won't take the range(len(df.columns)) idk
listofcrosslistedIDs=[]
for i in range(28):
    for j in range(3):
        if pd.isnull(dfOfCrossListedsAndNansV2.iloc[i,j]):
            pass
        else:
            listofcrosslistedIDs.append(dfOfCrossListedsAndNansV2.iloc[i,j])
#%%
os.chdir("C:\\Users\\Mom\\Documents\\Junior 1st sem\\STAT 399 V2")
#enrollmentData = pd.read_csv('EnrollmentData20150906.csv')
#PCRData = pd.read_csv('247classes20150905.csv')
#dfWithDuplicates=pd.merge(enrollmentData,PCRData)
#df=dfWithDuplicates.drop_duplicates()
df=pd.read_excel('FinaldfV1.xlsx')
df=df.drop('Unnamed: 0',1)
#%% Find out how many cross-listeds we have 
crosslistedsfromdf=df[df['class.name'].isin(listofcrosslistedIDs)]
#crosslistedsfromdfduplicate=dfWithDuplicates[dfWithDuplicates['class name'].isin(listofcrosslistedIDs)]
#jim=crosslistedsfromdfduplicate.drop_duplicates()
setofcrosslistedprofs=set(list(crosslistedsfromdf['instructor']))
#%% the excel export
#crosslistedsfromdfduplicate.to_excel("crosslistedsfromdfduplicate.xlsx")
crosslistedsfromdf.to_excel("crosslistedsfromdfFinal.xlsx")

#%% space to check before you leave it
checkdfpcr=PCRData[PCRData['instructor']=="JULIA MINSON"]
#%% This was to check how often a prof was teaching a class not in the same department as he's in, like shell teaching mgmt 291
checkdf=FinaldfV2[FinaldfV2['instructor']=="STEVEN O KIMBROUGH"]

#%% Create a df where the class is not listed as that of the prof's department
os.chdir("C:\\Users\\Mom\\Documents\\Junior 1st sem\\STAT 399 V2\\Cross-Listings")
ProfessorDictFrame=pd.read_excel('ProfessorDict.xlsx')
#%%
def TwoColumnDfToDict(ProfessorDictFrame):
    FinalProfDict={}
    for i in range(len(ProfessorDictFrame)):
        FinalProfDict[ProfessorDictFrame.iloc[i,0]]=ProfessorDictFrame.iloc[i,1]
    return FinalProfDict
    
FinalProfDict=TwoColumnDfToDict(ProfessorDictFrame)
#%% Find how often a cross-listed class is listed as different from the prof's department
#important note. When we've been putting dataframes together pandas seems to concatenate the columns in different orders.
#Keep that in mind when you're putting the final thing together.
#need subject and instructor

def FindProfNotInHisDept(crosslistedsfromdf,FinalProfDict):
    listofsections=[]    
    for i in range(len(crosslistedsfromdf)):
        if crosslistedsfromdf.iloc[i,20]!=FinalProfDict[crosslistedsfromdf.iloc[i,6]]:
            listofsections.append(crosslistedsfromdf.iloc[i,0])
    return listofsections

listofsections=FindProfNotInHisDept(crosslistedsfromdf,FinalProfDict)
#%% get this last bit to excel
FinaldfV2=df[-df['Unique.ID'].isin(listofsections)]
dfofcrosslistedstocorrect=df[df['Unique.ID'].isin(listofsections)]
#%%
os.chdir("C:\\Users\\Mom\\Documents\\Junior 1st sem\\STAT 399 V2")
FinaldfV2.to_excel("FinaldfV2.xlsx")
dfofcrosslistedstocorrect.to_excel('dfofcrosslistedstocorrect.xlsx')
    
    

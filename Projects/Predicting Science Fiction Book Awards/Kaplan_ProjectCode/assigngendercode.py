# -*- coding: utf-8 -*-
#pip install gender_guesser
import gender_guesser.detector as gender
import pandas as pd
dataframe = pd.read_csv('/Users/johnfields/Library/Mobile Documents/com~apple~CloudDocs/Syracuse/IST718/Assignments/Project/UCSD Book Graph/booksFPawardsReviewsFPall2.csv',low_memory=False)
d = gender.Detector()
first_names = []
for i in range(0,3590): #the second number is the len() of your dataset
    name = str(dataframe['authorName'].values[i]).split(' ', 1)[0] # input data and name column
    first_names.append(name)
# lowercase everything
first_names = [k.lower() for k in first_names]
# capitalize the first letter of every name
first_names = [i.capitalize() for i in first_names]
#%%time
# add genders
genders = []
for i in first_names[0:len(first_names)]:
    if d.get_gender(i) == 'male':
        genders.append('male')
    elif d.get_gender(i) == 'female':
        genders.append('female')
    else:
        genders.append('unknown')

gender_series = pd.Series(genders)
gender_series.value_counts() #this will get value counts for male, female, and unknown

# assign gender_series to new gender column in dataframe
dataframe['gender'] = gender_series
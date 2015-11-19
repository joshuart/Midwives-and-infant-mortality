import pandas as pd

infantMort = pd.read_csv("/Volumes/Seagate Data Drive/Research/Midwives:Doctors Outcomes/Infant Mortality/Data/raw_data.csv",
    skiprows = [0,1, 2])

infantMort = infantMort.sort('Infant Mortality Rate')
infantMort['Rank'] = list(range(len(infantMort.index)))
infantMort[infantMort['Location'] == 'United States of America'].Rank



#healthcare expenditures per capita
healthExpen = pd.read_csv("/Volumes/Seagate Data Drive/Research/Midwives:Doctors Outcomes/Infant Mortality/Data/raw_data-2.csv",
    skiprows = [0,1, 2])

healthExpen = healthExpen.sort('Health Expenditure Per Capita', ascending = False)
healthExpen['Rank'] = list(range(len(healthExpen.index)))
healthExpen[healthExpen['Location'] == 'United States of America'].Rank



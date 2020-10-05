library(tidyverse)
library(micromapST)

weekend <-read_csv('in_weekend.csv')

colnames(weekend)


# The script below has just two days but adding
#  more "or's" will work.   
### 2. Extra Row by Dates and make separate subsets 
cases <- weekend %>% 
  filter(End_Week =='8/15/2020' | End_Week == '8/29/2020') 
cases

# This works for more days
a<-group_by(cases,State)
a

# Note: For the Covid-19 with dates as rows  
# use select and '9/12/2020':'9/19/2020' will
# will get all 8 days.
b<-subset(a,End_Week=='8/15/2020')
c<-subset(a,End_Week=='8/29/2020')
### 3. Rename subset variables use the date 
# The 4 lines of script could be 
# turned function and
# adapted to run in loop.

tmp <-names(b)
nam <- c('State',paste0(tmp[3:5],'15'))
bfix <- select(b,-1)
names(bfix) <- nam

# view(bfix)

tmp <-names(c)
nam <- c('State',paste0(tmp[3:5],'29'))
cfix <- select(c,-1)
names(cfix) <- nam

### 4. Join the subsets and make a dataframe 

covid <- left_join(bfix,cfix,by='State')
omit <- covid$State %in% c('Puerto Rico','New York City')
covid <- covid[!omit,]


covidDF <- as.data.frame(covid,row.names='State')
row.names(covidDF) <- covid$State
covidDF <- select(covidDF,-State)
colnames(covidDF)

covid1 <-covidDF[c(1,2,3,5,6,4)] #reorder column for normbar
colnames(covid1)
### 5. Make a panel description dataframe 
Desc <- data.frame(
  type = c('mapcum','id','arrow','normbar'),  # 'full' is not a column type   
  lab1 = c('','', 'Covid-19', 'Pneumonia symptom'),
  lab2 = c('','', 'Deaths Count', 'In total COVID-19 Death'),
  lab3 = c('','', 'Count', 'Percent'),
  col1 = c('','', 'Deaths15', 5),
  col2 = c('','', 'Deaths29', 6),
  refVals = c(NA, NA, 200, NA),
  refTexts = c(NA, NA, ' hard-hit state', NA))
### 6. Make a State micromaps
dev.new()

micromapST(
  covid1, panelDesc=Desc, 
  rowNames = 'full',
  sortVar= 'Deaths29',
  ascend=F,
  title='Covid-19 Death for Aug.15 to Aug.29, 2020',
  plotNames='full',
) 

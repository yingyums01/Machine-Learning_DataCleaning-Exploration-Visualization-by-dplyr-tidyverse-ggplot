# Machine-Learning_DataCleaning-Exploration-Visualization-by-dplyr-tidyverse-ggplot
---
title: "Machine Learning_DataCleaning, Exploration, Visualization by dplyr, tidyverse, ggplot"
author: "Doris Ying-Yu Kuo"
date: "11/3/2019"
output:
  html_document:
    toc: true
    toc_float: true
    theme: darkly
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# ● Part1: Collecting the Data

## 1. Install library and read data

```{r}
library(tidyverse)
library(zipcode)
inspection_raw <- read_csv("https://s3.amazonaws.com/notredame.analytics.data/inspections.csv")
```

## 2. Change column name and data type

```{r}
#Change Name of Columns: we still want to have a copy on the raw data in case there is something wrong. Therefore, we assign the raw data to a new variable. By doing this, we don't need to wait for re-downloading the data when we want to run everything from the beginning again.

inspection<-inspection_raw
colnames(inspection)
new_colnames <- c("ID","DBAName","AKAName","License","FacilityType","Risk","Address","City","State","ZIP","InspectionDate","InspectionType","Results","Violations","Latitude","Longitude","Location")
colnames(inspection) <- new_colnames
colnames(inspection)


# Change Datatype: use funtion mutate and as....to change columns type.
inspection <- inspection %>%
  mutate(ID = as.integer(ID)) %>%
  mutate(License = as.character(License)) %>%
  mutate(InspectionDate = as.Date(InspectionDate,"%m/%d/%Y"))%>%
  mutate(FacilityType = as.factor(FacilityType))%>%
  mutate(Risk = as.factor(Risk))%>%
  mutate(City = as.factor(City))%>%
  mutate(State = as.factor(State))%>%
  mutate(ZIP = as.factor(ZIP))%>%
  mutate(InspectionType = as.factor(InspectionType))%>%
  mutate(Results = as.factor(Results))
```

## 3. Summary Statistics
```{r}
head(inspection)
summary(inspection)
```

</br>

# ● Part2: Resolve Data Quality Issues


## 1. Fix City, State, ZipCode

```{r, warning=FALSE}
data(zipcode)
# 1.replace ZIP Code 60627 with 60827

inspection <-inspection%>%
  mutate(ZIP = recode(ZIP,'60627'='60827'))
#Original is 1 row
inspection%>%
  filter(ZIP=="60627")
#Original is 102 row
inspection%>%
  filter(ZIP=="60827")

# 2.replace BrideView Code's from NA to 60455
# We will replace the name with "BRIDGEVIEW" later by using zipcode dataset
# Also, for the typo and the upper/lower case in City- Chicago, we will use the zipcode dataset to correct them later.

inspection <- inspection%>% 
  mutate(ZIP=as.character(ZIP))%>%
  mutate(ZIP = replace(ZIP, City=="BRIDEVIEW","60455"))%>%
  mutate(ZIP=as.factor(ZIP))
#Original is 6 row
inspection%>%
  filter(is.na(ZIP))
inspection%>%
  filter(City=="BRIDEVIEW")%>%
  select(City,ZIP)


# 3.replace ZIP NA: search address to fill the na

# separate the address
zipaddress <- inspection%>%
  select(ZIP,Address)%>%
  separate(Address,c('number','direction','name','AV'),' ')
zipaddress
#filter zip = na
zipna <- zipaddress%>%
  filter(is.na(ZIP))
zipna

# check the close street's ZIP
naaddress <- zipaddress%>%
  filter(name=='FREMONT'|name =='PAULINA')%>%
  unique()%>%
  arrange(name,number)
naaddress


### After checking the naaddress table(closest address), we decided to replace the ZIP Na with value below manually
  #FREMONT 2324 :60642
  #PAULINA 7475: 60626
inspection <- inspection%>%
  mutate(ZIP = replace(ZIP,is.na(ZIP)&(Address == "2324 N FREMONT ST"),'60642'))%>%
  mutate(ZIP = replace(ZIP,is.na(ZIP)&(Address == "7545 N PAULINA ST"),'60626'))
which(is.na(inspection$ZIP))

### left join the City and State
### revise datatype in zipcode for join
zipcode<-zipcode%>%
  mutate(zip=as.factor(zip))%>%
  mutate(city=as.factor(city))%>%
  mutate(state=as.factor(state))

inspection <- left_join(inspection,zipcode,by= c("ZIP"="zip"))
inspection

### select the correct city and state and then rearrange the columns as the original order.
inspection <- inspection%>%
  select(ID, DBAName,AKAName,License,FacilityType,Risk,Address,city,state,ZIP,InspectionDate,InspectionType,Results,Violations,Latitude,Longitude,Location,latitude,longitude)%>%
  rename("City"="city")%>%
  rename("State"="state")%>%
  mutate(ZIP=as.factor(ZIP))
str(inspection)

```

## 2. Output individual lists with distinct values for City, State, ZIP and sorted alphabetically
```{r}
list(sort(unique(inspection$City)))
list(sort(unique(inspection$State)))
list(sort(unique(inspection$ZIP)))
```


## 3. Longitude and Latitude

### Dealing with NAs in Longitude and Latitude


```{r}
length(which(is.na(inspection$Latitude)))
length(which(is.na(inspection$Longitude)))
length(which(is.na(inspection$Location)))
```
There are respectively 494, 494, and 494 missing values in the latitude, longitude, and location columns.
To futher confirm whether these missing values are in the same row, we compare them in pairs:

```{r}
table(which(is.na(inspection$Latitude)) == which(is.na(inspection$Longitude)))
table(which(is.na(inspection$Longitude))== which(is.na(inspection$Location)))
```

Using the simple test above, we are sure that these missing data are the same in three columns.

</br>

Have a look for the index and store the index where there are missing value in a variable:
```{r}
missing_index <- c(which(is.na(inspection$Latitude)))
missing_index
```


</br>

Next, create a location column without NAs:
```{r}
inspection <- inspection %>%
  mutate(location = str_c("(", inspection$latitude," , ", inspection$longitude,")"))

any(is.na(inspection$location))
```

Start to replace NA values:
```{r}
for(i in missing_index){inspection$Location[i] = inspection$location[i]}
for(i in missing_index){inspection$Latitude[i] = inspection$latitude[i]}
for(i in missing_index){inspection$Longitude[i] = inspection$longitude[i]}

# Check:
any(is.na(inspection$Location))
any(is.na(inspection$Latitude))
any(is.na(inspection$Longitude))

# delete the latitude, longitude, and location
inspection<-inspection%>%
  select(-latitude,-longitude,-location)%>%
  mutate(Latitude=as.double(Latitude))%>%
  mutate(Longitude=as.double(Longitude))
str(inspection)
```

### Range for Longitude and Latitude

```{r}
# Output:
# A. Latitude Range
range(inspection$Latitude)


# B. Longtitude Range
range(inspection$Longitude)

```


</br>

# ● Part3: Engineer a New Feature

### 1. Create New Feature - ViolationCount

```{r}
# import library stringr
library(stringr)
```

```{r}
# if there are 2 |, it means the violation items numbers are 3. Therefore, we +1 after count the number of | in string.
inspection <- inspection %>%
  mutate(ViolationCount = str_count(Violations,"\\|")+1)
```

To think about how to deal with NA, we dig into the coulmns' definition and the pattern in related column first. We found the ViolationCount is somehow related to Results from the data description on the Internet. And, even if the result is 'pass', it only means the establishments were found to have no critical or serious violations. It doesn't mean these establishments have 0 violations. Therefore, replacing the NAs in ViolationCount with 0 would be definitely wrong.

Additionally, the violation is related to the Type of inspection as well. Say, if it is for liscence check, it is likely to be passing without violation. In a normal inspection or reinspection, there could be a lot of pass with violation. Yet, in this case, follow the instruction of assignment, we put this aside first.

Take one step further, we analyzed the NAs percentage to see how large the NAs will influence the distibution.
```{r}
see<-inspection %>%
  group_by(Results,ViolationCount)%>%
  summarise(VFrequency=n())
see

see%>%
  group_by(Results)%>%
  mutate(Percentage=VFrequency/sum(VFrequency)*100)%>%
  filter(is.na(ViolationCount))
```

(1) Remain NAs as NAs value: 
For "Business Not Located","No Entry","Not Ready",and "Out of Business" categories in Results column, since the NAs percentage is above 90%, if we replace any value for these NAs in these categories, the transformed/assumed value will highly screwed the distibution in the category. Therefore, we decide to leave these NAs there, not doing any imputation. In the future, if people want to see the ViolationCount's statistics, these NAs won't affect the statistics of other known values. People will clearly understand these NAs are just NAs.  


(2) Replace NAs with median value: 
For "Fail","Pass",and "Pass w/ Conditions" categories in Results column, since the NAs percentage is around or below 10%, though it is still a little bit big, according to rule of thumbs, they are still acceptable. Therefore, we decide to replace these NAs with median value, but add another column(OrigIsNa-VCount) for noting these rows are original NAs. In the future, if people want to see the ViolationCount's statistics in these categories, they can the imputation NAs value directly, it they want to see the statistics for original known values, they can use the new column(OrigIsNa-VCount) to filter out the original NAs row. 


```{r}
# Add the new column first: set the original value as "N". for those NAs, set the original value as "NA".
inspection <- inspection%>%
  mutate(OrigIsNaVCount=ifelse(!is.na(ViolationCount),"N",ViolationCount))
#take a look
inspection%>%
  select(Results,ViolationCount,OrigIsNaVCount)

# Then, for those rows we are going to impute NA values, set "Y" to OrigIsNaVCount. 
inspection <- inspection%>%
  mutate(OrigIsNaVCount=ifelse((Results=="Fail"|Results=="Pass"|Results=="Pass w/ Conditions") & (is.na(ViolationCount)),"Y",OrigIsNaVCount))
#take a look
inspection%>%
  select(Results,ViolationCount,OrigIsNaVCount)

```

Then, for the three Risks categories, we are going to replace the NAs in ViolationCount with median values.
```{r}
inspection<-inspection%>%
  group_by(Results) %>%
  mutate(ViolationCount = ifelse((Results=="Fail"|Results=="Pass"|Results=="Pass w/ Conditions") & (is.na(ViolationCount)), median(ViolationCount, na.rm = TRUE), ViolationCount))

#take a look
inspection%>%
  select(Results,ViolationCount,OrigIsNaVCount)
```

### 2. Chart for Inspection Violations and Inspection Results
Code for visualizing ViolationCount contains original NA:
```{r}
a<-inspection %>%
  filter(Results=='Pass'|Results=="Fail"|Results=="Pass w/ Conditions")%>%
  ggplot()+
  geom_histogram(aes(x=ViolationCount,color=Results, fill=Results),alpha=0.8, binwidth = 0.5)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#D04D31")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#D04D31"))+
  theme(panel.background = element_rect(fill = NA),
    panel.grid.major.y = element_line(colour = "#CFCFCF"),
    panel.grid.major.x = element_line(colour = "#CFCFCF"))+
  facet_wrap(~Results)+
  ggtitle("ViolationCount contains original NA")


b<-inspection %>%
  filter((Results=='Pass'|Results=="Fail"|Results=="Pass w/ Conditions"))%>%
  ggplot()+
  geom_boxplot(aes(x=1, y=ViolationCount,color=Results, fill=Results,alpha=0.7))+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#D04D31")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#D04D31"))+
  theme(panel.background = element_rect(fill = NA),
    panel.grid.major.y = element_line(colour = "#CFCFCF"),
    panel.grid.major.x = element_line(colour = "#CFCFCF"))+
  facet_wrap(~Results)+
  coord_flip()
```  

Code for visualizing ViolationCount without original NA:
```{r, message=FALSE}  
c<-inspection %>%
  filter((Results=='Pass'|Results=="Fail"|Results=="Pass w/ Conditions")&(OrigIsNaVCount=='N'))%>%
  ggplot()+
  geom_histogram(aes(x=ViolationCount,color=Results, fill=Results),alpha=0.8, binwidth = 0.5)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#D04D31")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#D04D31"))+
  theme(panel.background = element_rect(fill = NA),
    panel.grid.major.y = element_line(colour = "#CFCFCF"),
    panel.grid.major.x = element_line(colour = "#CFCFCF"))+
  facet_wrap(~Results)+
  ggtitle("ViolationCount without original NA")
  
d<-inspection %>%
  filter((Results=='Pass'|Results=="Fail"|Results=="Pass w/ Conditions")&(OrigIsNaVCount=='N'))%>%
  ggplot()+
  geom_boxplot(aes(x=1, y=ViolationCount,color=Results, fill=Results,alpha=0.7))+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#D04D31")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#D04D31"))+
  theme(panel.background = element_rect(fill = NA),
    panel.grid.major.y = element_line(colour = "#CFCFCF"),
    panel.grid.major.x = element_line(colour = "#CFCFCF"))+
  facet_wrap(~Results)+
  coord_flip()
```


```{r, echo=FALSE,fig.width = 9, fig.height = 3.5}
inspection %>%
  filter(Results=='Pass'|Results=="Fail"|Results=="Pass w/ Conditions")%>%
  ggplot()+
  geom_histogram(aes(x=ViolationCount,color=Results, fill=Results),alpha=0.8, binwidth = 0.5)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#D04D31")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#D04D31"))+
  theme(panel.background = element_rect(fill = NA),
    panel.grid.major.y = element_line(colour = "#CFCFCF"),
    panel.grid.major.x = element_line(colour = "#CFCFCF"))+
  facet_wrap(~Results)+
  ggtitle("ViolationCount contains original NA")+
  coord_flip()

inspection %>%
  filter((Results=='Pass'|Results=="Fail"|Results=="Pass w/ Conditions"))%>%
  ggplot()+
  geom_boxplot(aes(x=1, y=ViolationCount,color=Results, fill=Results,alpha=0.7))+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#D04D31")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#D04D31"))+
  theme(panel.background = element_rect(fill = NA),
    panel.grid.major.y = element_line(colour = "#CFCFCF"),
    panel.grid.major.x = element_line(colour = "#CFCFCF"))+
  facet_wrap(~Results)
```  

```{r, echo=FALSE,fig.width = 9, fig.height = 3.5} 
inspection %>%
  filter((Results=='Pass'|Results=="Fail"|Results=="Pass w/ Conditions")&(OrigIsNaVCount=='N'))%>%
  ggplot()+
  geom_histogram(aes(x=ViolationCount,color=Results, fill=Results),alpha=0.8, binwidth = 0.5)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#D04D31")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#D04D31"))+
  theme(panel.background = element_rect(fill = NA),
    panel.grid.major.y = element_line(colour = "#CFCFCF"),
    panel.grid.major.x = element_line(colour = "#CFCFCF"))+
  facet_wrap(~Results)+
  ggtitle("ViolationCount without original NA")+
  coord_flip()
  
inspection %>%
  filter((Results=='Pass'|Results=="Fail"|Results=="Pass w/ Conditions")&(OrigIsNaVCount=='N'))%>%
  ggplot()+
  geom_boxplot(aes(x=1, y=ViolationCount,color=Results, fill=Results,alpha=0.7))+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#D04D31")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#D04D31"))+
  theme(panel.background = element_rect(fill = NA),
    panel.grid.major.y = element_line(colour = "#CFCFCF"),
    panel.grid.major.x = element_line(colour = "#CFCFCF"))+
  facet_wrap(~Results)
```

### 3. Relation between inspection results and the number of violation
(1) Pass means fewer Violation: The medium numbers of ViolationCount for all reults are within 3-7. For the restaurants with Results:Pass, they maily violate few rules with a median around 3-4 ViolationCount. Subsequently, the restaurants with Results:Pass w/ Condition have a little bit higher median- 5 ViolationCount. The worst performance is in the restaurants who have Result: Fail. Normally, they have more ViolationCount with median at 6-7.

(2) Normal Distibution: The numbers of ViolationCount are generally normal distributed for different Results types. This means restaurants which violated many rules or violated few rules are only a few. However, the distibution of ViolationCount numbers for the Results:Pass is a little bit skewed. This means for the restaurants whose Result is 'pass' is more likely to violate fewer rules.

(3) Concern of Imputation: If we replace the NA value with median, from the two comparison graph, we can see the replaced value for NA do affect a lot on the histogram(let the value in the middle part of histogram become higher). It somehow misleading the viewers with wrong distribution. Therefore, if we really need to deal with the NA value in the future, the median method might not be the best way. We need to further dig into the reason behind the NA value.


# ● Appendix(Self-practicing): Other Exploration for the whole table and other columns

For practicing the theory we learned in class, we explored the whole dataset a little bit deeper and selected some other columns to resolve problems in them:

### 1. Overall Exploration

Check whether there are duplicate inspection ID(duplicate rows). Then we found the output row of duplicated ID is zero. Therefore, we don't need to deal with duplicated inspections.

```{r}
inspection[duplicated(inspection$ID),]
```


Next, check which columns contains NAs for the whole table. From the result, we know if in the future we need to deal with NAs which columns we need to focus on:
```{r}
for (i in colnames(inspection)){
  print(paste(i,"column's NA number:",length(which(is.na(inspection[,i])))))
}
```


### 2. DBAName: We want to unify the form of the DBA names.

```{r, message=FALSE}
library(Hmisc)
head(unique(inspection$DBAName),20)
inspection <- inspection%>%
  mutate(DBA_name = capitalize(tolower(DBAName))) %>%
  select(- DBAName)
head(unique(inspection$DBAName),20)
```
Instead of taking out all the sign, we decied to save it for preventing the confusion due to loss of space.
Say, Eurest @ drinks contains the @ in the name. Therefore, the best treatment is to capitalized the first 
character, but save the special signs."

### 3. FacilityType: Make the data tidier

```{r}
# see the content 
# (The complete numbers of unique items are 434. To prevent exhausting scrolling down on the HTML page, we deducted the result to show only the first 50.)
head(sort(unique(inspection$FacilityType)),20)
# Unified string to lowercase
inspection$FacilityType <- tolower(inspection$FacilityType)
head(sort(unique(inspection$FacilityType)),20)

#deal with unexpected space and unified signs
#(1)delete space before or after /
grep("\\s/|/\\s|\\s/\\s",inspection$FacilityType,value=TRUE)
inspection$FacilityType <- gsub("\\s/|/\\s|\\s/\\s","/",inspection$FacilityType)
grep("\\s/|/\\s|\\s/\\s",inspection$FacilityType,value=TRUE)

#(2)delete space before or after &
grep("\\s&|&\\s|\\s&\\s",inspection$FacilityType,value=TRUE)
inspection$FacilityType <- gsub("\\s&|&\\s|\\s&\\s","&",inspection$FacilityType)
grep("\\s&|&\\s|\\s&\\s",inspection$FacilityType,value=TRUE)

#(3)convert / or and to &
head(unique(grep("/",inspection$FacilityType,value=TRUE)),20)
inspection$FacilityType <- gsub("/","&",inspection$FacilityType)
head(unique(grep("/",inspection$FacilityType,value=TRUE)),20)
head(unique(grep(" and ",inspection$FacilityType,value=TRUE)),20)

for (i in 1:nrow(inspection)){
  if (inspection$FacilityType[i] %in% c("restaurant and liquor","before and after school program","juice and salad bar","grocery and butcher","art gallery w/wine and beer","catering and wholesale","restuarant and bar","boys and girls club")){
    inspection$FacilityType[i] <- gsub(" and ","&",inspection$FacilityType[i])
  }
}
unique(grep(" and ",inspection$FacilityType,value=TRUE))
head(sort(unique(inspection$FacilityType)),20)
```

### 4. Insights from Appendix

A. 3 Most Frequent DBA names: Subway, Dunking Donut, Mcdonload. They have tremendous coverage in the Chicago region.
Based on this fact, we decided to use DBA name as the major label. This label will stand by for future usage. In this part, let us see ten most frequent DBA name.

```{r, warning=FALSE}

viz <- data.frame(table(inspection$DBA_name))
Top10_DBA <- viz %>%
  arrange(desc(Freq)) %>%
  head(n=10)

colnames(Top10_DBA) <- c("DBA_names", "Freqencies")

Top10_DBA %>%
  ggplot(mapping = aes(x=reorder(Top10_DBA$DBA_name, Top10_DBA$Freqencies), y=Top10_DBA$Freqencies)) +
  geom_col() +
  coord_flip() + 
  ylab("Numbers of Shops") +
  xlab("DBA names") + 
  ggtitle("Top 10 DBA Names Given Geological Coverage") +
  theme_minimal()
```

B. Map the Results based on Geographic Information:
The results has been stratified: The major pass and pass w/ conditions occur in downtown region; the no entry is prevelent in the north and south suburban; the inspection in southern Chicago find lot of failure.
Disclaimer: Basic code of map from the tutorial of University of Chicago, by Benjamin Soltoff, Computation for the social science, url: https://cfss.uchicago.edu/notes/raster-maps-with-ggmap/

```{r, warning= FALSE, message=FALSE}

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(RColorBrewer)
library(here)

options(digits = 3)
set.seed(1234)
theme_set(theme_minimal())


chi_bb <- c(left = -87.936287,
            bottom = 41.679835,
            right = -87.447052,
            top = 42.000835)

chicago_stamen <- get_stamenmap(bbox = chi_bb,
                                zoom = 11)
chicago_stamen

any(is.na(inspection$Latitude))

viz <- inspection %>%
  select(DBA_name,Latitude,Longitude,Results)

## Locate these inspections
ggmap(chicago_stamen) + geom_point(data = viz, mapping = aes(x = Longitude,
                           y = Latitude), size =.4, alpha = .01) +
  stat_density_2d(data = viz,
                  aes(x = Longitude,
                      y = Latitude,
                      fill = stat(level)),
                  alpha = 0.7,
                  geom = "polygon") +
  scale_fill_gradientn(colors = brewer.pal(7, "YlOrRd"))


# Pass fail wise
ggmap(chicago_stamen) + 
stat_density_2d(data = viz,
                  aes(x = Longitude,
                      y = Latitude,
                      fill = as.factor(viz$Results)),
                alpha = 0.4,
                  geom = "polygon")

ggmap(chicago_stamen) +
  geom_density_2d(data = viz,
                  aes(x = Longitude,
                      y = Latitude))
```


library(httr)
library(jsonlite)
library(RCurl)
library(ggplot2)
library(dplyr)
library(lubridate)

#log in the system with NASA EarthData account username and password
secret <- base64_enc(paste("henryxie1003", "100394Xie", sep = ":"))
response <- POST("https://lpdaacsvc.cr.usgs.gov/appeears/api/login", 
                 add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
                             "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"), 
                 body = "grant_type=client_credentials")

#check log in status
token_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
token_response

response_p <- GET("https://lpdaacsvc.cr.usgs.gov/appeears/api/product")
product_response <- toJSON(content(response_p), auto_unbox = TRUE)

# create a list indexed by the product name and version (optional)
products <- fromJSON(product_response)
products <- setNames(split(products, seq(nrow(products))), products$ProductAndVersion)

# create a task request
task <- '{
          "task_type": "point",
"task_name": "my-task",
"params":{
"dates": [
{
  "startDate": "01-01-2008",
  "endDate": "11-01-2018"
}],
  "layers": [
  {
"product": "MOD13A2.006",
  "layer": "_1_km_16_days_NDVI"
  },
  {
"product": "MYD13A2.006",
  "layer": "_1_km_16_days_NDVI"
  }],
  "coordinates": [
  {
  "latitude": 43.0444,
  "longitude": -78.7833
  }]
}
}'
task <- fromJSON(task)
task <- toJSON(task, auto_unbox=TRUE)

#submit the task request
token <- paste("Bearer", fromJSON(token_response)$token)
response <- POST("https://lpdaacsvc.cr.usgs.gov/appeears/api/task", body = task, encode = "json", 
                 add_headers(Authorization = token, "Content-Type" = "application/json"))
task_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
task_response

#retrieve the task detail
token <- paste("Bearer", fromJSON(token_response)$token)
task_id <- fromJSON(task_response)$task_id
response <- GET(paste("https://lpdaacsvc.cr.usgs.gov/appeears/api/task/", task_id, sep = ""), add_headers(Authorization = token))
task_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
task_response

#bundle API
token <- paste("Bearer", fromJSON(token_response)$token)
response <- GET(paste("https://lpdaacsvc.cr.usgs.gov/appeears/api/bundle/", task_id, sep = ""), add_headers(Authorization = token))
bundle_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
bundle_response

#Download file

#get csv file id and name
file_list<-fromJSON(bundle_response)$files
file_id<-file_list$file_id[file_list$file_type=="csv"]
file_name<-file_list$file_name[file_list$file_type=="csv"]

# create a destination directory to store the file in
dest_dir <- getwd()
filepath <- paste(dest_dir, file_name, sep = '/')
suppressWarnings(dir.create(dirname(filepath)))

# write the file to disk using the destination directory and file name 
for(i in 1:length(file_id)){
  response <- GET(paste("https://lpdaacsvc.cr.usgs.gov/appeears/api/bundle/", task_id, '/', file_id[i], sep = ""),
                write_disk(filepath[i], overwrite = TRUE), progress(), add_headers(Authorization = token))
  }

#read and view data
data<-read.csv(file_name)
View(data)

#read data from my github repository
data1<-read.csv(text=getURL("https://raw.githubusercontent.com/AdamWilsonLabEDU/geo503-2018-finalproject-Henryxie1003/master/my-task-MOD13A2-006-results.csv"))
data2<-read.csv(text=getURL("https://raw.githubusercontent.com/AdamWilsonLabEDU/geo503-2018-finalproject-Henryxie1003/master/my-task-MYD13A2-006-results.csv"))
View(data1)
View(data2)

#data wrangling
#filter out cloudy pixel
data1_nonCloud<-filter(data1,MOD13A2_006__1_km_16_days_VI_Quality_MODLAND!='0b10')
data2_nonCloud<-filter(data2,MYD13A2_006__1_km_16_days_VI_Quality_MODLAND!='0b10')

#combine two data frame and rename column
data1_new<-data1_nonCloud%>%select(Date,MOD13A2_006__1_km_16_days_NDVI)
colnames(data1_new)[2]<-'NDVI'
data2_new<-data2_nonCloud%>%select(Date,MYD13A2_006__1_km_16_days_NDVI)
colnames(data2_new)[2]<-'NDVI'
data_combine<-rbind(data1_new,data2_new)

#add year and number of days marker to each observation
data_combine$Year<-substr(data_combine$Date,1,4)
data_combine$NumDays<-yday(data_combine$Date)

#SOS and EOS
SOS_range<-data_combine%>%filter(NumDays>=50&NumDays<=150)
SOS<-data.frame(Year=character(),SOS=double())
for (i in 2008:2018) {
  SOS_t<-lm(NumDays~NDVI, SOS_range%>%filter(Year==i))%>%predict(data.frame(NDVI = c(0.5)))
  SOS<-rbind(SOS,c(i,SOS_t))
}
names(SOS) <- c("Year", "SOS")
SOS$Year<-as.character(SOS$Year)

EOS_range<-data_combine%>%filter(NumDays>=250&NumDays<=350)
EOS<-data.frame(Year=integer(),SOS=double())
for (i in 2008:2017) {
  EOS_t<-lm(NumDays~NDVI, EOS_range%>%filter(Year==i))%>%predict(data.frame(NDVI = c(0.5)))
  EOS<-rbind(EOS,c(i,EOS_t))
}
names(EOS) <- c("Year", "EOS")
EOS$Year<-as.character(EOS$Year)

#plot data
ggplot(data_combine,aes(x=NumDays,y=NDVI,col=Year))+
  geom_smooth(method='auto',span=0.5,fill=NA,se=F)+
  ylim(0.0,1.0)+
  labs(x='Number of days',y='NDVI')+
  theme_bw()

ggplot(data = SOS,aes(x=Year,y=SOS,group=1))+
  geom_line(colour='blue')+
  theme_bw()

ggplot(data = EOS,aes(x=Year,y=EOS,group=1))+
  geom_line(colour='red')+
  theme_bw()

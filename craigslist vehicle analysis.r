#Text Processing using pattern matching andregular expressions

#For this project I will be extracting several key variables from a dataset containing 35,000 web scraped craigslist vehicle postings
#This project is supposed to demonstrate how effective using R one can clean and mine big data for important infomation that isnt outright given

#first lets load the data into R
load(url("http://eeyore.ucdavis.edu/stat141/Data/vehicles.rda"))

#installing necessary packages
install.packages("stringr")
library(stringr)

#Our first task is to extract to price from the "body" column in the data frame and check it against the proce given

#here's a table first checking how many posts have their price listed in perfect format
table(grepl("\\$[0-9,]+", vposts$body))

#only about 41% match. Time to do some data cleaning and mining!

#after looking through the body section of some posts, I noticed that a few used the term K for 1000, therefore, my code is designed to remove prices in the $XX,XXX format and the $XK format
table(grepl('\\$[0-9]+,[0-9]{3}|\\$[0-9]+,[0-9]{3},[0-9]{3}|\\$[0-9]k|\\$[0-9,]+', vposts$body)) #this function shows me that 7998 posting have the price in the body column

table(grepl('\\$[0-9][Kk]{1}', body))#shows 45 results are in $Xk format

price = str_match(body, "\\$[0-9]+,[0-9]{3}|\\$[0-9]+,[0-9]{3},[0-9]{3}|\\$[0-9]k") #my function used to find price in the $XXX,XXX and $XXXK fomats

#since the prices in the price column dont have K, $, or commas, these functions are used to remove said characters from the price I extracted
nodollarsign = gsub(pattern = "\\$", replacement = "", price)
nok = gsub(pattern = "[kK]", replacement = "000", nodollarsign)
bodyprice = gsub(pattern = "\\,", replacement = "", nok)
#with bodyprice being the final extracted price.

table(bodyprice == vposts$price)
#this shows me that 6385 of my prices match, while 1300 dont


#Our second tast is to extract VIN numbers from the "body" column and give them their own colums
#A VIN number is a 8-17 character long string consisting of numbers, capital and lowercase letters. Length depends on year.
#Since we dont want to grab strings like websites, I am going to use a function that accounts for "VIN" preceeding the VIN number

#Heres a table of all VINs in that format

table(grepl("VIN( Number)?[[:space:]:#]+[0-9A-HJ-NPR-Z]{8,17}", vposts$body))

vin = gregexpr("VIN( Number)?[[:space:]:#]+[0-9A-HJ-NPR-Z]{8,17}", vposts$body)
vins = regmatches(vposts$body, vin)

#Now that we've got our VINs, let check and see if we found 1 VIN per post

table(sapply(vins, length))

#most of these look good with only 13 out of 34000+ having more than 1

#an analysis on the posts with multiple VINs show that these posts correspond to multiple vehicles.
realVIN = sapply(vins, function(x) length(unique(x))) > 1
vins[realVIN]
vposts$body[ realVIN ]

#now to add the VIN's to the data frame

vincol = str_match(vposts$body, "VIN( Number)?[[:space:]:#]+[0-9A-HJ-NPR-Z]{8,17}") #make columns for VIN
VIN = vincol[,1]
vposts$vin = VIN


#Our third task to to extract phone numbers from the "body" column and give them their own column
#we also need to check and see how many columns have phone numbers listed

phone = grepl("\\(?[0-9]{3}\\)? [0-9]{3}-?[0-9]{4}", vposts$body)
table(phone)

#under basic analysis only 20% have phone numbers. Lets do a little more cleaning, but first, lets see what they look like so far

phoneorig = gregexpr("\\(?[0-9]{3}\\)? [0-9]{3}-?[0-9]{4}", vposts$body)
phoneoriginal = regmatches(vposts$body, phoneorig)
head(phoneoriginal)

#These look good so far, lets account for spaces or dashes separating the area code from the rest of the phone number

areacode = grepl("\\(?[0-9]{3}\\)?( |-)?[0-9]{3}-?[0-9]{4}", vposts$body)
table(areacode)

#almost at 50% this is great! However, we would like to not mistake phone numbers for VIN numbers

noVIN = gregexpr("(\\([0-9]{3}\\)( |-)?[0-9]{3}-?[0-9]{4}|[0-9]{3}( |-)[0-9]{3}-?[0-9]{4})", vposts$body)
phonenum = regmatches(vposts$body, noVIN)
phonenum = unique(unlist(phonenum))

#Time to add these to the data fram 

phonecol = str_match(vposts$body, "(\\([0-9]{3}\\)( |-)?[0-9]{3}-?[0-9]{4}|[0-9]{3}( |-)[0-9]{3}-?[0-9]{4})") #make columns for phone numbers
phones = phonecol[,1]
vposts$phonenumber = phones


#Our fourth task to to extract emails from the "body" column and give them their own column
#we also need to check and see how many columns have emails listed

email = gregexpr("@[^ ]+( |$)", vposts$body)
head(unlist(regmatches(vposts$body, email)))

#From this we can determine some postings are @"phone number" or "@45,000 miles"
#From this point lets take into account all emails are @"website".com, .net, etc.

cleanemail = gregexpr("[a-zA-Z0-9.]+@[^[:space:].%>]+\\.[^[:space:].%>]+([[:space:]]|$)", vposts$body)
emails = unlist(regmatches(vposts$body, cleanemail))
emails

#these look a lot more like emails, lets add them to the data frame

emailcol = str_match(vposts$body, "[a-zA-Z0-9.]+@[^[:space:].%>]+\\.[^[:space:].%>]+([[:space:]]|$)") #make columns for emails
emails = emailcol[,1]
vposts$email = emails

#Our fifth task is to extract the year fromt he "body" column and compare it to the year given

table(grepl(' 20[01][0-9]|19[0-9]{2} ', body)) 
#this grabs all the 4 digit numbers starting with 19 or 20. I put spaces before and after to make sure it only grabs 4 digit characters

bodyyear = str_match(body, " 19[0-9]{2}|20[0-1][0-9] ") 
#creates a character variable with all of results found for a 4 digit variable starting with 19 or 20
bodyyear = gsub(pattern = " ", replacement = "", bodyyear) 
#removes spaces from begining and end
Year = vposts$year 

table(bodyyear == Year) 
#compares actual year to extracted year, with 21645 true, and 1887 false

#Our sixth task is to extract the models from the "body" column and make a separate column for them

maker = vposts$maker #create seperate dataset for vector, this is to search for models based on the listed maker
titlemaker = stri_trans_totitle(maker) #since makers are lowercase and makers in the body column , capitalize them

regex2= paste0(titlemaker," [[:alnum:]]+") #makes character vector of maker + potential model

model2 = str_match(body, regex2) #finds maker and model in body

models = str_match(model2, " [[:alnum:]]+") #takes out just second word

models = gsub(pattern = " ", replacement = "", models) #removes space, and creats character vector for the models

#show sample of models column and count how many models were succesfully extracted
head(models)
length(models[!is.na(models)])

#add models to data frame

vposts$model = models

############

#Modeling

############

#Displaying the relationship of price, odometer, age, city, and condition for 2 car models
#I drive a volkwagon Jetta, so I'm curious about this models relationships.
#Lets take a look!

#subsetting data for just the data on the volkswagon Jetta
vwjetta <- vposts[ which(vposts$model == 'Jetta'),]

#load in ggplot2 for data visualizations
library(ggplot2)


#price vs odometer by city
ggplot(vwjetta, mapping = aes(x = price, y = odometer)) +
  geom_point(mapping = aes(color = city)) +
  geom_smooth()

# we can see a clear correlation with lower prices to higher odometer readings

#price vs age by city

ggplot(vwjetta, mapping = aes(x = price, y = year)) +
  geom_point(mapping = aes(color = city)) +
  geom_smooth()

# we can see a clear correlation of higher prices for newer cars














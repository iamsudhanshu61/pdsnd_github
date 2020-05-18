#loading dataset
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

#deleting some old comments and loading new
#loading libraries
library(dplyr)
library(plyr)
library(ggplot2)
##preprocessing for dataset

#a# .. replacing blank cell in Gender with NA .. #a#
chi$Gender[chi$Gender == ""] <- '0'
ny$Gender[ny$Gender == ""] <- '0'
## .. replace blank space in user.type .. #
ny$User.Type[ny$User.Type == ""] <- '0'


#b#making Age.Class as anew_col for classify the Birth.Year as 3 levels (senior adult ,adult ,adulescence)#b#
Age.Class <- ifelse(
    chi$Birth.Year >= 1899 & chi$Birth.Year <= 1975,
    chi['Age.Class'] <- 'Senior Adult',
    ifelse(
        chi$Birth.Year > 1975 & chi$Birth.Year <= 1984,
        chi['Age.Class'] <- 'Adult',
        ifelse(
            chi$Birth.Year > 1984 & chi$Birth.Year <= 2016,
            chi['Age.Class'] <- 'Adolescence ',
            chi['Age.Class'] <- '0'
        )
    )
)
chi['Age.Class'] <- Age.Class
table(chi$Age.Class)

#c## .. for ny data set .. #c#
Age.Class <- ifelse(
    ny$Birth.Year >= 1885 & ny$Birth.Year <= 1981,
    ny['Age.Class'] <- 'Senior Adult',
    ifelse(
        ny$Birth.Year > 1981 & ny$Birth.Year <= 1988,
        ny['Age.Class'] <- 'Adult',
        ifelse(
            ny$Birth.Year > 1988 & ny$Birth.Year <= 2001,
            ny['Age.Class'] <- 'Adolescence ',
            ny['Age.Class'] <- '0'
        )
    )
)
ny['Age.Class'] <- Age.Class
table(ny$Age.Class)


#d# .. splitting the hour .. #d#
hour <- function(data) {
    start_time <-
        sapply(strsplit(as.character(data$Start.Time), " "), "[", 2)
    ##suset hour from the data
    hour <- substr(x = start_time, 1, 2)
}
##calling fun hour for chi dataset
hour_chi <- hour(chi)
chi['hour'] <- hour_chi
##calling fun hour for chi dataset
hour_ny <- hour(ny)
ny['hour'] <- hour_ny
##calling fun hour for wash dataset
hour_wash <- hour(wash)
wash['hour'] <- hour_wash


#e# .. split the date .. #e#
month <- function(data) {
    start_date <-
        sapply(strsplit(as.character(data$Start.Time), " "), "[", 1)
    ##subset month from the data
    month <- substr(x = start_date, 6, 7)
}
##calling fun with data = chi
month_chi <- month(chi)
chi['month'] <- month_chi
unique(chi$month)
##calling function with data = ny
month_ny <- month(ny)
ny['month'] <- month_ny
##calling function with data = wash
month_wash <- month(wash)
wash['month'] <- month_wash


##replacing '01' with 'january' and '02' with 'February' and so on ...
old <- c('01', '02', '03', '04', '05', '06')
new <- c('January', 'Febraury', 'March', 'April', 'May', 'June')
chi$month[chi$month %in% old] <-
    new[match(chi$month, old, nomatch = 0)]
ny$month[ny$month %in% old] <-
    new[match(ny$month, old, nomatch = 0)]
wash$month[wash$month %in% old] <-
    new[match(wash$month, old, nomatch = 0)]



#deleting this old set and adding new line
##visualization generating function

rental_user <- function(data,title) {
    ggplot(data = data,
           aes(x = User.Type)) +
        labs(title = title,
             x = 'User Type',
             y = 'Rental Count'
             ) +
        geom_histogram(
            aes(y = ..count..),
            stat = "count",
            width = .6,
            col = "white",
            fill = "#f55e61"
        ) +
        facet_wrap( ~ data$month)
}
#visualize
## for chi df ,Subscriber rents bike more than Customer and independent.
rental_user(chi,title="Rental Count compresion for Chicago User Types")
table(chi$User.Type)

## for ny df ,Subscriber rents bike more than Customer and independent.
rental_user(subset(ny,!is.na(User.Type)),title ="Rental Count compresion for New York User Types" )
table(ny$User.Type)

## for wash df,Subscriber rents bike more than Customer and it is clearly shown that there is a growth in the rental during last month June.
rental_user(na.omit(wash),title="Rental Count compresion for Washington User Types")
table(wash$User.Type)

##updating the code with the summary statistics in the 2nd line of the code
##Summary Statistics:
#To calculate the rentals per user type of all the cities from the dataset
#ressources : https://www.tutorialspoint.com/r/r_data_frames.htm
#Chicago
table(chi$User.Type)
#New York
table(ny$User.Type)
#Washington
table(wash$User.Type)

dev.off()

##updating the code with the summary statistics in the 2nd line of the code
##Summary Statistics:
#To calculate the rentals per user type of all the cities from the dataset
#ressources : https://www.tutorialspoint.com/r/r_data_frames.htm
#Chicago
table(chi$User.Type)
#New York
table(ny$User.Type)
#Washington
table(wash$User.Type)

# Your solution code goes here
hours_ride <- function(data,title) {
    qplot(
        x = data$hour,
        data = data,
        xlab = "Hours",
        ylab = "count for each hour",
        main = title,
        color = I('white'),
        fill = I('#60BD68')
    )
}
## for chi dataset, The most used hour for bicycle borrowing was at 5 pm, followed by 4 pm and then from the plotting: ##
hours_ride(data = na.omit(chi),title="The most used Chicago's hour for sharing bike")
table(chi$hour)

## for ny dataset ,The most used hour for bicycle borrowing was at 5 pm, followed by 6 pm##
hours_ride(data = na.omit(ny),title="New York ~ The most used hour for sharing bike")
table(ny$hour)

## from the washington plot ,The most used hour for bicycle borrowing was at 8 AM, followed by 7 AM as we can plot: ##
hours_ride(data = na.omit(wash),title="Washington ~ The most used hour for sharing bike")
table(wash$hour)

##updating the code with the summary statistics in the 2nd line of the code
##Summary Statistics:
#To calculate the hour during which bikes were mostly used from the dataset
#ressources : https://www.tutorialspoint.com/r/r_data_frames.htm
#Chicago
table(chi$hour)
#New York
table(ny$hour)
#Washington
table(wash$hour)





dev.off()

##updating the code with the summary statistics in the 2nd line of the code
##Summary Statistics:
#To calculate the hour during which bikes were mostly used from the dataset
#ressources : https://www.tutorialspoint.com/r/r_data_frames.htm
#Chicago
table(chi$hour)
#New York
table(ny$hour)
#Washington
table(wash$hour)

# Your solution code goes here
hours_per_month <- function(data,title) {
    qplot(
        x = data$hour,
        data = data,
        #added the new line for axis label
        xlab = "Hours",
        ylab = "Bike shares count",
        main = title,
        color = I('white'),
        fill = I('#5DA5DA')
    ) +
        facet_wrap(~ month, ncol = 3) +
        coord_flip() +
        theme_bw()
}
## for chi dataset ,The month for highest rental of the bike was last month 'June’ with time at 5pm ##
hours_per_month(data = na.omit(chi),title="The most used hour per months for Chicago")
table(chi$hour)
## for ny dataset ,The month for highest rental of the bike was last month 'June’ at 5 pm, followed by 6 pm ##
hours_per_month(data = na.omit(ny),title="New York ~ The most used hour per months for New York")
table(ny$hour)
## for wash ,The hour during ﬁrst 3-January, February and March month is 6 PM,for April,May was 5 AM and June was 8 AM ##
hours_per_month(data = na.omit(wash),title="Washington ~ The most used hour per months ")
table(wash$hour)



##updating the code with the summary statistics in the 2nd line of the code
##Summary Statistics:
#To calculate the hour for each month with increasing sharing from the dataset
#ressources : https://www.tutorialspoint.com/r/r_data_frames.htm
#Chicago
table(chi$month)
table(chi$hour)
#New York
table(ny$month)
table(ny$hour)
#Washington
table(wash$month)
table(wash$hour)




dev.off()

##updating the code with the summary statistics in the 2nd line of the code
##Summary Statistics:
#To calculate the hour for each month with increasing sharing from the dataset
#ressources : https://www.tutorialspoint.com/r/r_data_frames.htm
#Chicago
table(chi$month)
table(chi$hour)
#New York
table(ny$month)
table(ny$hour)
#Washington
table(wash$month)
table(wash$hour)

system('python -m nbconvert Explore_bikeshare_data.ipynb')

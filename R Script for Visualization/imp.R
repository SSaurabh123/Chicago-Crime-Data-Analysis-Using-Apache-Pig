crime.data=read.table(file.choose(),header = TRUE,sep = '\t')
crime.data


 

 ggplot(data=crime.data, aes(table(crime.data$Primary.Type))) + 
   geom_histogram()
 group_by(crime.data$Primary.Type)
 crime.data$date <- as.POSIXlt(crime.data$Date, format= '%m/%d/%Y %H:%M')
 
 crime.data$time <- times(format(crime.data$date, '%H:%M:%S'))
 
 head(crime.data$date)
 head(crime.data$time)
 
 
 head(crime.data$Date)
 
 
 time.tag <- chron(times=c('00:00:00', '06:00:00', '12:00:00', '18:00:00', '23:59:00'))
 
 crime.data$time.tag <- cut(crime.data$time, breaks= time.tag, labels= c('00-06','06-12', '12-18', '18-00'), include.lowest= TRUE)
 
 table(crime.data$time.tag)
 
 
 crime.data$date <- as.POSIXlt(strptime(crime.data$date, format='%Y-%m-%d'))
 head(crime.data$date)
 
 
 crime.data$day <- weekdays(crime.data$date, abbreviate= TRUE)
 crime.data$month <- months(crime.data$date, abbreviate= TRUE)
 
 
 
 qplot(crime.data$time.tag, xlab='Time of day', main = 'Crimes by time of day') + scale_y_continuous('Number of crimes')
 
 crime.data$day <- factor(crime.data$day, levels= c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'))
 
 
 qplot(crime.data$day, xlab= 'Day of week', main='Crimes by day of week') + scale_y_continuous('Number of crimes')
 
 crime.data$month <- factor(crime.data$month, levels= c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) 
 
 
 qplot(crime.data$month, xlab= 'Month', main= 'Crimes by month') + scale_y_continuous('Number of crimes')
 
 crime=NA
 
 
  crime.data$crime <- as.character(crime.data$Primary.Type)
  crime.data$crime <- ifelse(crime.data$crime %in% c('CRIM SEXUAL ASSAULT', 'PROSTITUTION', 'SEX OFFENSE'), 'SEX', crime.data$crime)
  crime.data$crime <- ifelse(crime.data$crime %in% c('MOTOR VEHICLE THEFT'), 'MVT', crime.data$crime)
  crime.data$crime <- ifelse(crime.data$crime %in% c('GAMBLING', 'INTERFERE WITH PUBLIC OFFICER', 'INTERFERENCE WITH PUBLIC OFFICER', 'INTIMIDATION', 'LIQUOR LAW VIOLATION', 'OBSCENITY', 'NON-CRIMINAL', 'PUBLIC PEACE VIOLATION', 'PUBLIC INDECENCY', 'STALKING', 'NON-CRIMINAL (SUBJECT SPECIFIED)'), 'NONVIO', crime.data$crime)
                                                       
 
   crime.data$crime <- ifelse(crime.data$crime == 'CRIMINAL DAMAGE', 'DAMAGE', crime.data$crime) 
   crime.data$crime <- ifelse(crime.data$crime == 'CRIMINAL TRESPASS', 'TRESPASS', crime.data$crime)
   crime.data$crime <- ifelse(crime.data$crime %in% c('NARCOTICS', 'OTHER NARCOTIC VIOLATION', 'OTHER NARCOTIC VIOLATION'), 'DRUG', crime.data$crime) 
   crime.data$crime <- ifelse(crime.data$crime == 'DECEPTIVE PRACTICE', 'FRAUD', crime.data$crime)
   crime.data$crime <- ifelse(crime.data$crime %in% c('OTHER OFFENSE', 'OTHER OFFENSE'), 'OTHER', crime.data$crime)
   crime.data$crime <- ifelse(crime.data$crime %in% c('KIDNAPPING', 'WEAPONS VIOLATION', 'OFFENSE INVOLVING CHILDREN'), 'VIO', crime.data$crime)
   table(crime.data$crime) 
 
   qplot(crime.data$crime, xlab = 'Crime', main ='Crimes in Chicago') + scale_y_continuous('Number of crimes')
   
   
   
    temp <- aggregate(crime.data$crime, by= list(crime.data$crime, crime.data$time.tag), FUN= length)  
     names(temp) <- c('crime', 'time.tag', 'count')
   
   
     ggplot(temp, aes(x= crime, y= factor(time.tag))) + geom_tile(aes(fill= count)) + scale_x_discrete('Crime', expand = c(0,0)) + scale_y_discrete('Time of day', expand = c(0,-2)) + scale_fill_gradient('Number of crimes', low = 'white', high = 'steelblue') + theme_bw() + ggtitle('Crimes by time of day') + theme(panel.grid.major=element_line(colour=NA), panel.grid.minor=element_line (colour = NA)) 
     
     
     temp1 <- aggregate(crime.data$crime, by= list(crime.data$crime, crime.data$day), FUN= length)  
     names(temp1) <- c('crime', 'day', 'count')
     
     ggplot(temp1, aes(x= crime, y= factor(day))) + geom_tile(aes(fill= count)) + scale_x_discrete('Crime', expand = c(0,0)) + scale_y_discrete('Crime by day', expand = c(0,-2)) + scale_fill_gradient('Number of crimes', low = 'white', high = 'steelblue') + theme_bw() + ggtitle('Crimes by  day') + theme(panel.grid.major=element_line(colour=NA), panel.grid.minor=element_line (colour = NA)) 
     
     temp2 <- aggregate(crime.data$crime, by= list(crime.data$crime, crime.data$month), FUN= length)
     names(temp2) <- c('crime', 'month', 'count')
     ggplot(temp2, aes(x= crime, y= factor(month))) + geom_tile(aes(fill= count)) + scale_x_discrete('Crime', expand = c(0,0)) + scale_y_discrete('Crime by month', expand = c(0,-2)) + scale_fill_gradient('Number of crimes', low = 'white', high = 'steelblue') + theme_bw() + ggtitle('Crimes by month') + theme(panel.grid.major=element_line(colour=NA), panel.grid.minor=element_line (colour = NA)) 
     
     
     
    
     m =leaflet() %>% addTiles() %>% addMarkers(lng=crime.data$Longitude,lat=crime.data$Latitude,popup="Crime location")
     m
     

#First level distinguishing





#Second representation of data
temperature <- factor(c('cold','normal','cold'),levels = temperature_level,ordered = TRUE)
wind <- factor(c('weak','storm','storm'),levels = temperature_level,ordered = TRUE)




#Third ranking of the qualitative attributes
temperature_level_r <- rank(weather$temperature,ties.method = 'min')
wind_level_r <- rank(weather$wind,ties.method = 'min')

#library(cluster)


daisy(weather)


cloud.cover_level <- c('clear','scattered','broken','overcast')
precipitation_level <- c('none','light','moderate','heavy','extreme')
wind_level <- c('calm','strong','storm','light','moderate', 'gale')
temperature_level <- c('cold','normal','warm','heat','chilly')




weather <- data.frame("cloud.cover_level" = c('overcast', 'overcast', 'scattered', 'broken', 'broken', 'overcast', 'scattered', 'overcastlight'
), "precipitation_level" = c('light', 'heavy', 'light', 'none', 'light', 'moderate', 'none', 'light'
) ,"wind_level" = c('strong', 'moderate', 'light', 'light', 'light', 'light', 'calm', 'calm'
),"temperature_level" = c('chilly', 'chilly', 'cold', 'cold', 'cold', 'chilly', 'chilly', 'normal'
))

cloud.cover_r <- rank(cloud.cover_level, ties.method = 'min')
precipitation_level_r <- rank(precipitation_level, ties.method = 'min')
wind_level_r <- rank(wind_level,ties.method = 'min')
temperature_level_r <- rank(temperature_level, ties.method = 'min')

sapply(weather,as.numeric)


dist(weather, method = "euclidean")

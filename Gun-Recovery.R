## Police Gun Recovery
library(tidyverse)

df<- read_csv("https://bloomington.data.socrata.com/api/views/y66s-bnfm/rows.csv?accessType=DOWNLOAD",show_col_types = FALSE)




data <-df %>% select(c(3:6 , 8:9)) %>%
  mutate(Year = substring(df$stolen_date,7,10),Month = substring(df$stolen_date,1,2), Day = substring(df$stolen_date,4,5)) %>%
  arrange(desc(Year),desc(Month),desc(Day)) 




data %>%
  filter(recovery == 'NOT RECOVERED') %>%
  group_by(stolen_date,Year, Month) %>%
  summarise(Not_recovered = n()) %>%
  ggplot() + aes(x= Month , y= Not_recovered, color = Year) + geom_point(aes(size = Not_recovered )) + labs(title = "Un-Recovered Guns" , y = "Un-Recovered Weapons" )

ggsave("Not Recovered.png", width = 8, height = 8)

data %>%
  filter(recovery != 'NOT RECOVERED') %>%
  group_by(stolen_date,Year, Month) %>%
  summarise(Recovered = n()) %>%
  ggplot() + aes(x=Month , y= Recovered, color = Year) + geom_point(aes(size = Recovered )) + labs(title = "Recovered Guns" , y = "Recovered Weapons" )

ggsave("Recovered.png", width = 8, height = 8)

write.csv(data,"Gun recovery.csv",row.names=FALSE)


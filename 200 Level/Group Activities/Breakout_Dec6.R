
setwd("/Users/cvoglewede/Projects_Voglewede/2018HomegrownBreakouts/Month 1/Week 3")
energy <- read_csv("energy_data.csv")

summary(energy)

plot <- energy %>% 
  group_by(TheDate) %>% 
  summarise(
    Calls_Total=sum(!is.na(Calls)),
    Orders_Total=sum(Orders),
    GreenAttachments_Total=sum(GreenAttachments))

ggplot(plot,aes(x=TheDate)) + geom_line(aes(y=Calls_Total),color="blue") + geom_line(aes(y=Orders_Total),color="green")+ geom_line(aes(y=GreenAttachments_Total),color="red")

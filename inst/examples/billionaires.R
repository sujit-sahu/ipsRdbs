summary(bill)
head(bill)
gg <- ggplot2::ggplot(data=bill, aes(x=age, y=wealth)) +
  geom_point(aes(col=region, size=wealth)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(7, 102)) + 
  ylim(c(1, 37)) + 
  labs(subtitle="Wealth vs Age of Billionaires", 
       y="Wealth (Billion US $)", 
       x="Age", 
       # title="Scatterplot", 
       caption = "Source: Fortune Magazine, 1992.")
plot(gg)


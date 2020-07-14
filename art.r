diamonds %>%
  ggplot(aes(x=cut, y= price, color = cut))+
  geom_jitter(width = )+
  geom_violin( alpha = .8, position=position_jitter(0.2))+
  labs("Distribution of Price by Cut Type")

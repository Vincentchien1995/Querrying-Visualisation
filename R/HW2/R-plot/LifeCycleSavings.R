#SR and dpi
LifeCycleSavings %>%
  ggplot(aes(sr,dpi)) +
  geom_line(colour="blue")+
  labs(title = "The relationship between SR and dpi")+
  theme_bw()

#SR and ddpi 
LifeCycleSavings %>%
  ggplot(aes(sr, ddpi)) +
  geom_point(alpha = 1.5) +
  geom_smooth(colour="red") +
  labs(title = "The relationship between SR and ddpi")+
  theme_light()

#SR and pop15
LifeCycleSavings %>%
  ggplot(aes(sr,pop15))+
  geom_jitter(colour="blue")+
  geom_smooth(colour="black")+
  labs(title = "The relationship between SR and pop15")+
  theme_classic()

#SR and pop75
LifeCycleSavings %>%
  ggplot(aes(sr,pop75))+
  geom_jitter(colour="orange")+
  geom_smooth(colour="black")+
  labs(title = "The relationship between SR and pop75")+
  theme_classic()

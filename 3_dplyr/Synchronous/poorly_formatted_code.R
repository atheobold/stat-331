
ggplot(data=surveys, mapping=aes(x = weight, 
                                 y = species)) + geom_boxplot(outlier.alpha = 0)+ geom_jitter(color="tomato", mapping = aes(alpha=.3))


ggplot(data = surveys, mapping = aes(x=weight, y=species)) + geom_boxplot(outlier.shape=NA) + geom_jitter(col="tomato") +
  labs(x="Weight", y="Species", title = "Distribution of weight within each Species")

sapply(surveys, typeof)

# Code for question 14!
ggplot(data = surveys, mapping = aes(x=hindfoot_length,y= weight)) +  
  geom_jitter(alpha=.2,color='tomato')+ facet_wrap(~species)+geom_boxplot(outlier.shape = NA)+labs(
    title ='Weight to hindfoot comparison'
  )+ xlab('length (mm)')+ylab('Weight(g)')

ggplot(data=surveys, mapping=aes(x = weight, 
                                 y = species)) + geom_density_ridges()+ geom_jitter(color="tomato", alpha=.5) +xlab("weight of rodent")+ ylab("hindfoot_length of rodent")
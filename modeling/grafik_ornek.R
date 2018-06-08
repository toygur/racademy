library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$R.D.Spend,
                 y = test_set$Profit),
             colour = 'red') +
  geom_line(aes(x=test_set$R.D.Spend,
                y=predict(regressor, newdata=test_set)),
            colour = 'blue') +
  ggtitle('Lineer Regresyon') +
  xlab('R.D.Spend') + ylab('Profit')
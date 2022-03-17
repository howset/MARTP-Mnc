library(ggplot2)

####
## Yougov Q1/WCRex1
agyg<- read.csv('Data/Agg_yougov2.csv',sep='\t', header = TRUE)
agyg2 <- agyg[-c(12,33,20),]
agyg3 <- agyg[c(12,33,20),]

el_plot <- ggplot(data = agyg, mapping = aes(x = Pos_news, y = Surv)) +
    geom_point(data=agyg2,shape=19,color='#5e81ac') + # scatter 
    geom_point(data=agyg3,shape=4,color='#FFC000') + # highlight the outlier
    geom_smooth(method = "lm", data = agyg, se=FALSE,color='#FFC000',linetype=1) + # lm of all datapoints
    stat_ellipse(color = '#5e81ac',
                 linetype = 2,
                 lwd = 1.2) +
    geom_smooth(method = "lm", data = agyg2, se=FALSE,color='#5e81ac',linetype=2) # lm without outlier
e <- el_plot + 
    # ggtitle('YG WCRex2') +
    xlab('Positive News (%)')+
    ylab('Survey (%)') +
    theme_linedraw()
print(e)

####
## Yougov Q2/WCRex2
yg2 <- data.frame(Pos_news=c(51.05,43.48,42.86,37.5,30.26,25,31.88,29.47,35.09),
                  Surv=c(61,60,60,58,59,58,59,62,58))
yg22 <- yg2[-8,] # remove outlying data point
yg23 <- yg2[8,]# the outlying data point

el_plot <- ggplot(data = yg2, mapping = aes(x = Pos_news, y = Surv)) +
    geom_point(data=yg22,shape=19,color='#5e81ac') + # scatter 
    geom_point(data=yg23,shape=4,color='#FFC000') + # highlight the outlier
    geom_smooth(method = "lm", data = yg2, se=FALSE,color='#FFC000',linetype=1) + # lm of all datapoints
    stat_ellipse(color = '#5e81ac',
                 linetype = 2,
                 lwd = 1.2) +
    geom_smooth(method = "lm", data = yg22, se=FALSE,color='#5e81ac',linetype=2) # lm without outlier
e <- el_plot + 
    # ggtitle('YG WCRex2') +
    xlab('Positive News (%)')+
    ylab('Survey (%)') +
    theme_linedraw()
print(e)

####
## SMRC
sm <- data.frame(Pos_news=c(39.5,31.57,37.5,43,44,42.85,31.42,31.48,32.53,30.26,27.39,22.53,29.62,31.91,17.24,45.61,32.2),
                 Surv=c(69,63,73,63,72,69,66,66,69,72,74,70,67,75,57,72,74))
sm2 <- sm[-15,] # remove outlying data point
sm3 <- sm[15,] # the outlying data point

el_plot <- ggplot(data = sm, mapping = aes(x = Pos_news, y = Surv)) +
    geom_point(data=sm2,shape=19,color='#5e81ac') + # scatter
    geom_point(data=sm3,shape=4,color='#FFC000') + # highlight the outlier
    geom_smooth(method = "lm", data = sm, se=FALSE,color='#FFC000',linetype=1) + # lm of all datapoints
    stat_ellipse(color = '#5e81ac',
                 linetype = 2,
                 lwd = 1.2) +
    geom_smooth(method = "lm", data = sm2, se=FALSE,color='#5e81ac',linetype=2) # lm without outlier
e <- el_plot + 
    # ggtitle('SMRC') +
    xlab('Positive News (%)')+
    ylab('Survey (%)') +
    theme_linedraw()
print(e)




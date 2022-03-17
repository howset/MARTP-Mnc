## Library
library(ggplot2)
library(dplyr)

## Load and subset
agg_yg <- read.csv('Data/subset_indiv20210505.csv', header = TRUE)
agg_yg <- agg_yg[,-1] # remove first column
    
## Factorize columns
for (wcr in 1:length(agg_yg$WCRex1)) {
    if (agg_yg$WCRex1[wcr]=='Somewhat well' | agg_yg$WCRex1[wcr]=='Very well' ) {
        agg_yg$WCRex1[wcr] <- 'Favourably'
    } else  {
        agg_yg$WCRex1[wcr] <- 'Not favourably'
    }
}

for (col_num in 1:length(colnames(agg_yg))) {
    if (class(agg_yg[[col_num]])=='character') {
        agg_yg[[col_num]] <- as.factor(agg_yg[[col_num]])
    }
}
# summary(agg_yg)

## Subset random week (week 11)
week <- subset(agg_yg,qweek=='week 11')
summary(week)
colnames(week) <- c('Week','Gender','Age','Region', # unnecessary (?)
                    'Household_size','Household_children',
                    'Employment','WCRex1')

## Grouped barplots WCRex1
gen<-ggplot(week, aes(x = Gender, fill = WCRex1)) +
    geom_bar() +
    geom_text(aes(label=..count..),stat = 'count',size = 5 , position = position_stack(vjust = 0.5))+
    theme_linedraw()
gen+
    theme(text = element_text(size = 15)) +
    scale_fill_manual(values=c('#5992DB', '#FFC000')) +
    labs(x='Gender',y='Count')

age <- ggplot(week, aes(x = Age, fill = WCRex1)) +
    geom_bar() +
    geom_text(aes(label=..count..),stat = 'count',size = 5, position = position_stack(vjust = 0.5),angle=60)+
    theme_linedraw() 
age + 
    theme(legend.position='none',text = element_text(size = 15)) +
    scale_fill_manual(values=c('#5992DB', '#FFC000'))+ 
    labs(x='Age', y = 'Count')

hsize <- ggplot(week, aes(x = Household_size, fill = WCRex1)) +
    geom_bar() +
    geom_text(aes(label=..count..),stat = 'count',size = 4, position = position_stack(vjust = 0.5),angle=60)+
    theme_linedraw()
hsize + 
    scale_fill_manual(values=c('#5992DB', '#FFC000'))+ 
    labs(x='Household Size', y = 'Count') +
    theme(legend.position='none',text = element_text(size = 15),axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

hchild <- ggplot(week, aes(x = Household_children, fill = WCRex1)) +
    geom_bar() +
    geom_text(aes(label=..count..),stat = 'count',size = 4, position = position_stack(vjust = 0.5),angle=60)+
    theme_linedraw()
hchild + 
    scale_fill_manual(values=c('#5992DB', '#FFC000'))+ 
    labs(x='Household Children', y = 'Count') +
    theme(legend.position='none',text = element_text(size = 15),axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

week_sort <- week %>% group_by(Employment) %>%   mutate(counts = n())
emp <- ggplot(week_sort, aes(x=reorder(Employment,-counts), fill = WCRex1)) +
    geom_bar() +
    geom_text(aes(label=..count..),stat = 'count',size = 4, position = position_stack(vjust = 0.5),angle=60)+
    theme_linedraw()
emp + 
    scale_fill_manual(values=c('#5992DB', '#FFC000'))+ 
    labs(x='Employment', y = 'Count') +
    theme(legend.position='none',text = element_text(size = 15),axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))

week_sort <- week %>% group_by(Region) %>%   mutate(counts = n())
reg <- ggplot(week_sort, aes(x=reorder(Region,-counts), fill = WCRex1)) +
    geom_bar() +
    geom_text(aes(label=..count..),stat = 'count',size = 5, position = position_stack(vjust = 0.5),angle=60)+
    theme_linedraw()
reg + 
    scale_fill_manual(values=c('#5992DB', '#FFC000'))+ 
    labs(x='Region', y = 'Count') +
    theme(legend.position='none',text = element_text(size = 15),axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))


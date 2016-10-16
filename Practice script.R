# Step 1
library(tidyverse)
library(psych)
library(haven)
library(apaTables)
library(dplyr)
raw_data <- read_csv(file="raw_data.csv",na=c("","NA","-999","-888"))
categorical_variables <- select(raw_data, sex)
categorical_variables$sex <- as.factor(categorical_variables$sex)
levels(categorical_variables$sex) <- list("Male"=1,"Female"=2)
pos_affect_items <- select (raw_data, delighted, elated, enthusiastic, excited)
neg_affect_items <- select (raw_data, afraid, angry, anxious, ashamed)
Neuroticism_items <- select (raw_data, Neuroticism)
Extraversion_items <- select (raw_data, Extraversion)
is_bad_value <- pos_affect_items<0 | pos_affect_items>3
pos_affect_items[is_bad_value] <- NA
is_bad_value <- neg_affect_items<0 | neg_affect_items>3
neg_affect_items[is_bad_value] <- NA
is_bad_value <- Neuroticism_items<0 | Neuroticism_items>24
Neuroticism_items[is_bad_value] <- NA
is_bad_value <- Extraversion_items<0 | Extraversion_items>24
Extraversion_items[is_bad_value] <- NA
psych::describe(pos_affect_items)
psych::describe(neg_affect_items)
psych::describe(Neuroticism_items)
psych::describe(Extraversion_items)
pos_affect <- psych::alpha(as.data.frame(pos_affect_items),check.keys=FALSE)$scores
neg_affect <- psych::alpha(as.data.frame(neg_affect_items),check.keys=FALSE)$scores
analytic_data <- cbind(categorical_variables,neg_affect,pos_affect,Extraversion_items,Neuroticism_items)


## Creating Male and Female Subsets
analytic.data.female <- filter (analytic_data, sex=='Female')
analytic.data.female <- select(analytic.data.female, neg_affect, pos_affect, Extraversion, Neuroticism)
analytic.data.male <- filter (analytic_data, sex=='Male')
analytic.data.male <- select(analytic.data.male, neg_affect, pos_affect, Extraversion, Neuroticism)

#Saving .RData, CSV, .SAV 
save(analytic_data,file="quiz2_analytic_data.RData")
save(analytic.data.male,file="quiz2_analytic_data_male.RData")
save(analytic.data.female,file="quiz2_analytic_data_female.RData")
write_csv(analytic_data,path="quiz2_analytic_data.csv")
write_csv(analytic.data.male,path="quiz2_analytic_data_male.csv")
write_csv(analytic.data.female,path="quiz2_analytic_data_female.csv")

#Correlation Tables
apa.cor.table(analytic_data, filename="Table_1_Overall.doc", table.number=1)
apa.cor.table(analytic.data.male, filename="Table_2_Male.doc", table.number=2)
apa.cor.table(analytic.data.female, filename="Table_3_Female.doc", table.number=3)

#Figures
psych::pairs.panels(as.data.frame(analytic_data),lm=FALSE)
psych::pairs.panels(as.data.frame(analytic.data.male),lm=FALSE)
psych::pairs.panels(as.data.frame(analytic.data.female),lm=FALSE)

#Histograms
my.hist <- ggplot(analytic.data.female,aes(Neuroticism))
my.hist <- my.hist + geom_histogram(aes(y= ..count..), binwidth= 1, fill="black", color="black")
my.hist <- my.hist + labs(title="Neuroticism",x="Neuroticism Score", y="Frequency")
my.hist <- my.hist + theme_classic()
my.hist <- my.hist + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                           axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
my.hist <- my.hist + scale_x_continuous( breaks = seq(0,25,by=5) )
my.hist <- my.hist + scale_y_continuous( breaks = seq(0,150,by=10), expand=c(0,0) )
ggsave("Figure_4_Neuroticism_Histogram_Female.tiff", plot=my.hist, width=6,height=6)
print(my.hist)

my.hist2 <- ggplot(analytic.data.female,aes(neg_affect))
my.hist2 <- my.hist2 + geom_histogram(aes(y= ..count..), binwidth=1, fill="black", color="black")
my.hist2 <- my.hist2 + labs(title="Negative Affect",x="Negative Affect Score", y="Frequency")
my.hist2 <- my.hist2 + theme_classic()
my.hist2 <- my.hist2 + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                             axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
my.hist2 <- my.hist2 + scale_x_continuous( breaks = seq(0, 3,by=0.5) )
my.hist2 <- my.hist2 + scale_y_continuous( breaks = seq(0, 2000,by=100), expand=c(0,0) )
ggsave("Figure_5_NegativeAffect_Histogram_Female.tiff", plot=my.hist2, width=6,height=6)
print(my.hist2)

#Scatter Plot
my.plot <- qplot(neg_affect, Neuroticism, data=analytic.data.female)
my.plot <- my.plot + theme_classic(14)
my.plot <- my.plot + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                           axis.line.y = element_line(colour = 'black', size=0.5, linetype = 'solid'))
my.plot <- my.plot + labs(title="", x="Negative Affect", y="Neuroticism")
my.plot <- my.plot + coord_cartesian(xlim=c(0,3), ylim=c(0,25))
my.plot <- my.plot + geom_smooth(method = "lm", se = FALSE, color="black")

print(my.plot)

ggsave("Figure_6_NA_Neuroticism_Scatter.tiff", plot=my.plot, width=6,height=6)

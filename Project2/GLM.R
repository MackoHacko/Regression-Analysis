#install.packages("ggplot2")
#install.packages("foreach")
#install.packages("writexl")
rm(list=ls())
library(ggplot2)
library(foreach)
#library(xlsx)
library(writexl)
library(base)
source(file="multiplot.R")

######################### Section 1: Read data #########################

# This is where your GLM data is read form tractors.csv into a table in R 
# Note that the folder in which you have tractors.csv must be set as the working directory 
###### You do not need to change anything in this section. The data will be sotred in a table named glmdata

glmdata <- read.table("Tractors.csv", header=TRUE, sep=";", dec="," )

######################### Section 2: Create groups & aggregate data #########################

# Now you need to modify your data so that you can perform a GLM analysis 

# First, any continuous variable needs to be grouped into discrete groups 
# The code below groups the variable weight, from you table glmdata, into six groups, and stores this in a new column, weight_group 
###### This is only an example. You need to create your own groups, with breaks that suit your data
###### You might also want to group other variables from glmdata, in a similar manner

temp <- glmdata[order(glmdata$Weight),] 
nr_cat = length(temp$Weight)/10
weight_cat = c(temp$Weight[nr_cat], temp$Weight[2*nr_cat], 
               temp$Weight[3*nr_cat], temp$Weight[4*nr_cat], 
               temp$Weight[5*nr_cat], temp$Weight[6*nr_cat], 
               temp$Weight[7*nr_cat], temp$Weight[8*nr_cat], 
               temp$Weight[9*nr_cat])
weight_cat = c(0,350,450,600,1000,1500,2300,3200,3800,4800, Inf)

temp2 <- glmdata[order(glmdata$VehicleAge),] 
age_cat = c(temp2$VehicleAge[nr_cat], temp2$VehicleAge[2*nr_cat], 
               temp2$VehicleAge[3*nr_cat], temp2$VehicleAge[4*nr_cat], 
               temp2$VehicleAge[5*nr_cat], temp2$VehicleAge[6*nr_cat], 
               temp2$VehicleAge[7*nr_cat], temp2$VehicleAge[8*nr_cat], 
               temp2$VehicleAge[9*nr_cat])
age_cat = c(0,1,3,5,6,8,11,15,20,27, Inf)

glmdata$weight_group <- cut(glmdata$Weight, 
                       breaks = weight_cat, 
                       labels = c("01_<349kg", "02_350-449kg", 
                                  "03_450-599kg", "04_600-999kg", 
                                  "05_1000-1499kg", "06_1500-2299kg",
                                  "07_2300-3199kg", "08_3200-3799kg", 
                                  "09_3800-4799kg", "10_>=4800kg"), 
                       right = FALSE)

glmdata$age_group <- cut(glmdata$VehicleAge, 
                            breaks = age_cat, 
                            labels = c("01_<1yr", "02_1-2yr", 
                                       "03_3-4yr", "04_5yr", 
                                       "05_6-7yr", "06_8-10yr",
                                       "07_11-14yr", "08_15-19yr", 
                                       "09_20-26yr", "10_>=27yr"), 
                            right = FALSE)

# Secondly, we want to aggregate the data.
# That is, instead of having one row per tractor, we want one row for each existing combination of variables 
# This code aggregates columns 6-8 of glmdata, by three variables: weight_group, Climate, and ActivityCode 
# Tha aggregated data is stored in a new table, glmdata2 
##### You need to consider if there are any other variables you want to aggregate by, and modify the code accordingly 

glmdata2 <- aggregate(glmdata[,6:8],by=list(weight_group = glmdata$weight_group, 
                                            Climate = glmdata$Climate,
                                            ActivityCode = glmdata$ActivityCode,
                                            age_group = glmdata$age_group), FUN=sum, na.rm=TRUE)

# We then do some preparation for the output the GLM function will give.
# This piece of code creates a new table, glmdata3, with a row per variable and group, and with data on the total duration corresponding to this group.
##### You need ot modify the code to take into account any changes in variables you're using 

size1 <- length(glmdata2$Duration)
glmdata2 <- glmdata2[glmdata2$Duration!=0, ]
size2 <- length(glmdata2$Duration)
glmdata3 <-
  data.frame(rating.factor =
               c(rep("Weight", nlevels(glmdata2$weight_group)),
                 rep("Climate", nlevels(glmdata2$Climate)),
                 rep("ActivityCode", nlevels(glmdata2$ActivityCode)),
                 rep("Age", nlevels(glmdata2$age_group))),
             class =
               c(levels(glmdata2$weight_group),
                 levels(glmdata2$Climate),
                 levels(glmdata2$ActivityCode),
                 levels(glmdata2$age_group)),
             stringsAsFactors = FALSE)

new.cols <-
  foreach (rating.factor = c("weight_group", "Climate", "ActivityCode", "age_group"),
           .combine = rbind) %do%
           {
             nclaims <- tapply(glmdata2$NoOfClaims, glmdata2[[rating.factor]], sum)
             sums <- tapply(glmdata2$Duration, glmdata2[[rating.factor]], sum)
             n.levels <- nlevels(glmdata2[[rating.factor]])
             contrasts(glmdata2[[rating.factor]]) <-
               contr.treatment(n.levels)[rank(-sums, ties.method = "first"), ]
             data.frame(duration = sums, n.claims = nclaims)
           }
glmdata3 <- cbind(glmdata3, new.cols)
rm(new.cols)

######################### Section 3: GLM analysis #########################

# Now we get to the fun part - the GLM analysis. It is performed using R's built in GLM function 

# First, we model the claims frequency. 
# The first part of this performs a GLM analysis, with glmdata2 as the data source modelling NoOfClaims, by the Duration. It looks at three variables: weight_group, Climate, and ActivityCode.
##### This is where you can modify the model by adding or removing variables 

model.frequency <-
  glm(NoOfClaims ~ weight_group + Climate + ActivityCode + age_group + offset(log(Duration)),
      data = glmdata2, family = poisson)

# Then we save the coefficients resulting from the GLM analysis in an array
##### You should not need to modify this part of the code

rels <- coef(model.frequency)
rels <- exp(rels[1] + rels[-1])/exp(rels[1])

# Finally, we attach the coefficients to the already prepared table glmdata3, in a column named rels.frequency
# There is no good way of doing this automatically, so we need to do some manual tricks
# This code creates a vector with 6 positions consisting of the integer 1, and then positions number 1-5 in the rels array.
# Then it attaches this to rows 1-6 of glmdata3, sorted from highest to lowest duration, since the GLM data is on this form.
# In other words, the code takes the GLM coeffisients for the six weight groups and saves those in glmdata3, in the rows corresponding to those groups.
# After that, it does the same thing for the rest of the GLM coefficients, belonging to climate and activity code vairables.
##### You need to modify this code to suit your set of variables and groups, to make sure each GLM coefficient is saved in the correct place.


glmdata3$rels.frequency <-
  c(c(1, rels[1:9])[rank(-glmdata3$duration[1:10], ties.method = "first")],
    c(1, rels[10:11])[rank(-glmdata3$duration[11:13], ties.method = "first")],
    c(1, rels[12:21])[rank(-glmdata3$duration[14:24], ties.method = "first")],
    c(1, rels[22:30])[rank(-glmdata3$duration[25:34], ties.method = "first")])

# We then do the same thing again, now modelling severity instead of claim frequency.
# That means that, in this part, we want to look at the average claim. So first, we calculate the average claim for each row in glmdata2
##### You should not need to change anything in this piece of code.

glmdata2$avgclaim=glmdata2$ClaimCost/glmdata2$NoOfClaims

# Then we do the same thing as we did when modelling claims frequency, but we look at average claim;
# A GLM analysis is run, the coefficients stored, and saved in a new column, named rels.severity, glmdata3
##### You need to modify this part of the code in the same way as you did for the frequency. Add or remove variables, and make sure coefficients are stored correctly.
##### Remember that, according to the project instructions, you need to use the same variables for the severity as for the frequency.

model.severity <-
  glm(avgclaim ~ weight_group + Climate + ActivityCode + age_group ,
      data = glmdata2[glmdata2$avgclaim>0,], family = Gamma("log"), weight=NoOfClaims)

rels <- coef(model.severity)
rels <- exp( rels[1] + rels[-1] ) / exp( rels[1] )
glmdata3$rels.severity <-
  c(c(1, rels[1:9])[rank(-glmdata3$duration[1:10], ties.method = "first")],
    c(1, rels[10:11])[rank(-glmdata3$duration[11:13], ties.method = "first")],
    c(1, rels[12:21])[rank(-glmdata3$duration[14:24], ties.method = "first")],
    c(1, rels[22:30])[rank(-glmdata3$duration[25:34], ties.method = "first")])

# Finally, the final risk factor is calculated, as the product of the frequency and severity factors. 
##### You should not have to modify this coed.
##### Congratulations! You now have a model for the risk!
glmdata3$rels.risk <- with(glmdata3, rels.frequency*rels.severity)

######################### Section 4: Plotting #########################

# In this section, the results from the GLM are plotted.

# First, long variable names need to be cut, to fit into the plots.
# This row of code cuts away everything except for the first letter for variable names belonging to activity codes.
##### If you have long variable names, modify here to cut them.
shorten <- function (x) ifelse(x %in% c("Other", "Missing"), x, substr(x, 1, 1))
glmdata3[glmdata3$rating.factor == "ActivityCode", 2] <- shorten(glmdata3$class)[14:24]


# Then the results are plotted. This code plots the GLM factors for frequency, severity, and total risk, for the three variables Weight, Climate, and Activity code.
##### If you have changed what variables are included in your model, add, remove, or modify sections of this code to plot them. 
##### This is also where you can make changes to change the look of your plots, if you would like to.

p1 <- ggplot(subset(glmdata3, rating.factor=="Weight"), aes(x=class, y=rels.frequency)) + 
      geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Weight: frequency factors") +
      geom_text(aes(label=paste(round(rels.frequency,2))), nudge_y=1) +theme(axis.text.x = element_text(angle = 30, hjust = 1))

p2 <- ggplot(subset(glmdata3, rating.factor=="Weight"), aes(x=class, y=rels.severity)) + 
      geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Weight: severity factors") +
      geom_text(aes(label=paste(round(rels.severity,2))), nudge_y=0.5)+theme(axis.text.x = element_text(angle = 30, hjust = 1))

p3 <- ggplot(subset(glmdata3, rating.factor=="Weight"), aes(x=class, y=rels.risk)) + 
      geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Weight: risk factors") +
      geom_text(aes(label=paste(round(rels.risk,2))), nudge_y=1.6)+theme(axis.text.x = element_text(angle = 30, hjust = 1))

p4 <- ggplot(subset(glmdata3, rating.factor=="Climate"), aes(x=class, y=rels.frequency)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Climate: frequency factors") +
  geom_text(aes(label=paste(round(rels.frequency,2))), nudge_y=0.05)

p5 <- ggplot(subset(glmdata3, rating.factor=="Climate"), aes(x=class, y=rels.severity)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Climate: severity factors") +
  geom_text(aes(label=paste(round(rels.severity,2))), nudge_y=0.1)

p6 <- ggplot(subset(glmdata3, rating.factor=="Climate"), aes(x=class, y=rels.risk)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Climate: risk factors") +
  geom_text(aes(label=paste(round(rels.risk,2))), nudge_y=0.1)

p7 <- ggplot(subset(glmdata3, rating.factor=="ActivityCode"), aes(x=class, y=rels.frequency)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("ActivityCode: frequency factors") +
  geom_text(aes(label=paste(round(rels.frequency,2))), nudge_y=0.5) +theme(axis.text.x = element_text(angle = 30, hjust = 1))

p8 <- ggplot(subset(glmdata3, rating.factor=="ActivityCode"), aes(x=class, y=rels.severity)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("ActivityCode: severity factors") +
  geom_text(aes(label=paste(round(rels.severity,2))), nudge_y=0.5)+theme(axis.text.x = element_text(angle = 30, hjust = 1))

p9 <- ggplot(subset(glmdata3, rating.factor=="ActivityCode"), aes(x=class, y=rels.risk)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("ActivityCode: risk factors") +
  geom_text(aes(label=paste(round(rels.risk,2))), nudge_y=0.5)+theme(axis.text.x = element_text(angle = 30, hjust = 1))

p10 <- ggplot(subset(glmdata3, rating.factor=="Age"), aes(x=class, y=rels.frequency)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Age: frequency factors") +
  geom_text(aes(label=paste(round(rels.frequency,2))), nudge_y=0.5) +theme(axis.text.x = element_text(angle = 30, hjust = 1))

p11 <- ggplot(subset(glmdata3, rating.factor=="Age"), aes(x=class, y=rels.severity)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Age: severity factors") +
  geom_text(aes(label=paste(round(rels.severity,2))), nudge_y=0.5)+theme(axis.text.x = element_text(angle = 30, hjust = 1))

p12 <- ggplot(subset(glmdata3, rating.factor=="Age"), aes(x=class, y=rels.risk)) + 
  geom_point(colour="blue") + geom_line(aes(group=1), colour="blue") + ggtitle("Age: risk factors") +
  geom_text(aes(label=paste(round(rels.risk,2))), nudge_y=0.5)+theme(axis.text.x = element_text(angle = 30, hjust = 1))


multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, cols=4)

######################### Section 5: Average claim cost #########################
plot(glmdata$RiskYear, glmdata$ClaimCost)
riskyear = c(2006:2016)

for (i in 1:12){
  f <- paste0(paste0("plots/p", i), ".png")
  png(f)
  eval(parse(text=paste0("plot(p", i, ")")))
  dev.off()
}


result = matrix(0, nrow = 11, ncol = 2)
result[,1] =  riskyear 


for (k in 1:length(riskyear)){
  for(i in 1:length(glmdata[,1])){
    if(glmdata$RiskYear[i] == riskyear[k]){
      result[k,2] = result[k,2] + glmdata$ClaimCost[i]
    }
  }
}
result_MSEK = result
result_MSEK[,2] = result_MSEK[,2]/1000000
plot(result_MSEK, xlab = "Year", ylab = "Total annual claim cost (MSEK)", ylim = c(0,2), pch =20); grid()
k = lm(result_MSEK[,2] ~ result_MSEK[,1])
abline(lm(result_MSEK[,2] ~ result_MSEK[,1]))

totcost = k$coefficients[1]+k$coefficients[2]*2017;
sumprod = prod(glmdata3$rels.risk[1:10]) + prod(glmdata3$rels.risk[11:13]) + prod(glmdata3$rels.risk[14:23])
gamma0 = totcost*1000000/(0.9*sumprod)
total_prem = gamma0*sumprod

######################### Section 5: Export factors to Excel #########################

#As a last step, the risk factors are exported to excel. 
# The dopcument will be saved in the folder set as your working directory.

write_xlsx(glmdata3, "glmfactors.xlsx")
write_xlsx(data.frame(model.frequency.aic=model.frequency$aic, 
                      model.severity.aic=model.severity$aic), "values.xlsx")


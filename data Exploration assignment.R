contribution<-read.csv("H:\\contribution.csv")
library(dplyr)


contribution%>%mutate(contributions=FY04Giving+FY03Giving+FY02Giving+FY01Giving+FY00Giving)->contribution
# Analysing Contributions
# We will now create a variable for analysing the contributions across all years by the alumni.
# We will first do analysis at an aggregate level. Finding out how total contributions are distributed by Gender,
# Batch, Marital Status etc
#  1. Gender
contribution%>%
  group_by(Gender)%>%
  summarise(
    count=n(),
    percentage_count=count/1230,
    total_contribution=sum(contributions),
    percentage_contribution=total_contribution/1205454,
    average=mean(contributions)
  )
# # A tibble: 2 x 6
# Gender count percentage_count total_contribution percentage_contribution average
# <fct>  <int>            <dbl>              <dbl>                   <dbl>   <dbl>
#   1 F        615              0.5            364151.                   0.302    592.
# 2 M        615              0.5            841302.                   0.698   1368.
#male is in majority



# 2...Batch
contribution%>%
  group_by(Class.Year)%>%
  summarise(
    count=n(),
    percentage_count=count/1230,
    total_contribution=sum(contributions),
    percentage_contribution=total_contribution/1205454,
    average=mean(contributions)
    )
#1957 batch has highest contribution
#the students who have passed long ago have the highest contribution.
#also notice that they comprise of only 10 % of the student base


# 3...Marital_Status

contribution%>%
  group_by(Marital.Status)%>%
  summarise(
    count=n(),
    percentage_count=count/1230,
    total_contribution=sum(contributions),
    percentage_contribution=total_contribution/1205454,
    average=mean(contributions)
  )
#married people contribute the most and are highest in the group

# Marital.Status count percentage_count total_contribution percentage_contribution average
# <fct>            <int>            <dbl>              <dbl>                   <dbl>    <dbl>
#   1 D             78         0.0634             79515                      0.0660    1019.
# 2 M                711           0.578             941985.                  0.781    1325.
# 3 S                428           0.348              81773.                  0.0678    191.
# 4 W                 13           0.0106            102181.                  0.0848   7860.

#4...Major
contribution%>%
  group_by(Major)%>%
  summarise(
    count=n(),
    percentage_count=count/1230,
    total_contribution=sum(contributions),
    percentage_contribution=total_contribution/1205454,
    average=mean(contributions)
)%>%
  arrange(-total_contribution)
#history majors are the highest contributors followed by mathematics and economics


#5..Next Degree
contribution%>%
  group_by(Next.Degree)%>%
  summarise(
    count=n(),
    percentage_count=count/1230,
    total_contribution=sum(contributions),
    percentage_contribution=total_contribution/1205454,
    average=mean(contributions)
  )%>%
  arrange(-total_contribution)
#people who didn't go to pursue any degree further are the highest contributors


#6...Attendance Event
contribution%>%
  group_by(AttendenceEvent)%>%
  summarise(
    count=n(),
    percentage_count=count/1230,
    total_contribution=sum(contributions),
    percentage_contribution=total_contribution/1205454,
    average=mean(contributions)
  )%>%
  arrange(-total_contribution)

#thois is again obvious ,those who go to funraising events,contribute the most

# # A tibble: 2 x 6
# AttendenceEvent count percentage_count total_contribution percentage_contribution average
# <int> <int>            <dbl>              <dbl>                   <dbl>   <dbl>
#   1               1   743            0.604           1102842.                  0.915    1484.
# 2               0   487            0.396            102612.                  0.0851    211



# Now we will look at how total contributions are changing over the years
# It seems like that total contributions are variable and there is no predictable trend.

library(reshape2)
contribution%>%
  summarize(FY04=sum(FY04Giving),
            FY03=sum(FY03Giving),
            FY02=sum(FY02Giving),
            FY01=sum(FY01Giving),
            FY00=sum(FY00Giving)) %>%melt()->tot_conts

#renaming the variable and values to year and contribution
names(tot_conts)<-c("Year","Contribution")


#finding cumulative using cumsum() of per count_var
tot_conts$per_cont<-round(tot_conts$Contribution/1205454,2)

#plotting
library(ggplot2)
p<-ggplot(tot_conts,aes(x=Year,y=Contribution,fill=Year))
p+geom_bar(stat="identity",alpha=0.7)+geom_text(aes(label=per_cont,colour=per_cont),vjust=-0.3)+
  theme_classic()+
  scale_fill_discrete(c=50,h=c(1,250),h.start=50)+guides(colour=FALSE)
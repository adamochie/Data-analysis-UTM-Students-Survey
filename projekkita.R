install.packages("plotrix")
library(plotrix)

#1 gender (pie chart 3d)
datagender <- table(serbeyprojek$GENDER)
print(datagender)

gender <- c(22,42)
percent <- round(gender/sum(gender)* 100,2)
print(paste0(percent,"%"))

piePercent <- round(gender/sum(gender)*100)

pie3D(gender,labels=paste(c("Male","Female"),gender),
      explode=0.1,main="Gender",col=rainbow(length(gender)))
legend("topright",c("Male","Female"),cex=0.8,fill=rainbow(length(gender)))

#2 age (stem-and-leaf plot)
stem(serbeyprojek$AGE, scale = 0.05)
#2 age (dotplot)
sort(serbeyprojek$AGE)
dotchart(serbeyprojek$AGE, main = "Age",xlab = "number of age", pch = 21, bg = "blue", pt.cex = 1)

#3 faculty (barchart)
table(serbeyprojek$FACULTY)

numPeople <- c(53,0,7,2,0,2)
faculty <- sort(numPeople) 
faculty
barplot(numPeople, main = "FACULTY", ylab= "Number of students", 
        xlab="Type of Faculty",
        col=c("red","purple","yellow","green","orange","darkblue"), 
        border = "blue",ylim = c(0,60))
legend("topright",
       cex = 0.65,
       legend = c("FACULTY OF ENGINEERING",
                  "FACULTY OF BUILT ENVIRONMENT & SURVEYING",
                  " FACULTY OF SCIENCE",
                  " RAZAK FACULTY OF TECHNOLOGY & INFORMATICS",
                  "AZMAN HASHIM INTERNATIONAL BUSINESS SCHOOL",
                  "MALAYSIA JAPAN INTERNATIONAL INSTITUTE OF TECHNOLOGY"),
       fill = c("red","purple","yellow","green","orange","darkblue"))

#4 student year of study (pie chart)
table(serbeyprojek$YEAR)
numStud<- c(54,9,1,0)
pie(numStud, main="Students' Year",labels = c(numStud),col=rainbow(length(numStud)))
legend("topright",c("Year 1","Year 2","Year 3","Year 4"),cex=0.8,fill=rainbow(length(numStud)))

#7 monthly food expenses (histogram)
table(serbeyprojek$`YOUR MONTHLY FOOD EXPENSES IN UNIVERSITY`)
hist(serbeyprojek$`YOUR MONTHLY FOOD EXPENSES IN UNIVERSITY`, 
     main = "Monthly Food Expenses By UTM Students",
     xlim = c(100,1400), axis(side=1,at = c(100,300,500,700,900,1100,1300)),
     ylab="Number of Student", freq = TRUE, ylim = c(0,30), col = "magenta", 
     xlab = "Monthly Food Expenses (RM)")


#5 Having aid or relief (pie chart)
dapatzakat <- table(serbeyprojek$`RECEIVE ZAKAT, UTM FOOD COUPONS?`)
print(dapatzakat)
D
zakat <- c(61,3)
percent <- round(zakat/sum(zakat)* 100,2)
print(paste0(percent,"%"))

piePercent <- round(zakat/sum(zakat)*100)

pie(zakat,labels=paste(c("No","Yes"),zakat),
  main="Having Aid Relief (Food Coupon/Zakat)",col=rainbow(length(zakat)))
legend("topright",c("No","Yes"),cex=1,fill=rainbow(length(zakat)))


#6 Consistency of buying food (boxplot) 
boxplot(serbeyprojek$`CONSISTENCY OF BUYING FOOD FOR BREAKFAST PER DAY`,
        serbeyprojek$`CONSISTENCY OF BUYING FOOD FOR LUNCH PER DAY`,
        serbeyprojek$`CONSISTENCY OF BUYING FOOD FOR DINNER PER DAY`,
        main = "Consistency Of Buying Meals Per Day",
        col=c("red","lightblue","orange"),ylab = "Consistency scale")
legend("bottomright",
       cex = 0.55,
       legend = c("Breakfast","Lunch","Dinner"),
       fill = c("red","lightblue","orange"))


#8 meals per day vs saving money planning (scatterplot)
table(serbeyprojek$`TIME SPEND DURING LUNCH`)
table(serbeyprojek$`TIME SPEND DURING DINNER`)
timelunch <- c(5,28,25,6)
timedinner <- c(4,23,26,11)
plot(x = timelunch,
     y = timedinner, 
     xlab = "Number of student having lunch ",
     ylab = "Number of student having dinner ", 
     main = "Time spend eating (lunch vs dinner)", pch = 16)

#9 usual meals per day (bar chart)
table(serbeyprojek$`USUAL MEALS PER DAY`)
mealsperday <- c("0","1","2","3","4")
meals <- c(0,7,38,14,5)
susunmeals <- sort(meals)
susunmeals
barplot(meals, main = "Usual Meals Have in a Day", 
        ylab= "Number of student", xlab="Number of meals",
        col = "orange", border = "blue", names.arg = mealsperday, ylim = c(0,40))


#69 Money spend on breakfast (histogram)
hist(serbeyprojek$`ESTIMATE MONEY SPEND ON BREAKFAST IN A DAY`, main = "Money Spend on Breakfast By UTM Students", xlim = c(0,20), axis(side=1,at = c(0,5,10,15,20)),
     ylab="Number of Student", freq = TRUE, ylim = c(0,60), col = "LIGHTBLUE", xlab = "Money spend (RM)")
lines(density(serbeyprojek$`ESTIMATE MONEY SPEND ON BREAKFAST IN A DAY`), col = "red")



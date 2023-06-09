G4<- read.csv("G4_howell.csv")
View(G4)
dim(G4)



G4$sex <- ifelse(G4$sex =="M" ,"Male" , "Female")
View(G4)



str(G4)
G4$weight <- gsub("kg","" , G4$weight)
G4$weight <- gsub(",","" , G4$weight)
G4$weight <- as.numeric(as.character(G4$weight))
str(G4)



G4$age <- as.integer(G4$age)
str(G4)



med_ret <- median(G4[G4$sex == "Male" & G4$age <= 60,"weight"], na.rm = T)
G4[is.na(G4$Overweight) & G4$sex =="Male" & G4$age <= 60, "weight"] <-med_ret

med_ret <- median(G4[G4$sex == "Female" & G4$age <= 60,"weight"], na.rm = T)
G4[is.na(G4$Overweight) & G4$sex =="Female" & G4$age <= 60 , "weight"] <-med_ret

med_ret <- median(G4[G4$sex == "Male" & G4$age > 60,"weight"], na.rm = T)
G4[is.na(G4$Overweight) & G4$sex =="Male" & G4$age > 60 , "weight"] <-med_ret

med_ret <- median(G4[G4$sex == "Female" & G4$age > 60,"weight"], na.rm = T)
G4[is.na(G4$Overweight) & G4$sex =="Female" & G4$age > 60 , "weight"] <-med_ret

View(G4)



G4$BMI <- G4$weight / ((G4$height)/100) ^ 2
View(G4)


G4$age_classification[ G4$age >= 0 & G4$age <= 8  ] = "Child" 
G4$age_classification[ G4$age >= 9 & G4$age <= 17  ] = "Teenage"
G4$age_classification[ G4$age >= 18 & G4$age < 30  ] = "Young Adults"
G4$age_classification[ G4$age >= 30 & G4$age <= 60  ] = "Middle_age Adults"
G4$age_classification[ G4$age >  60 ] = "Old_age Adults"

View(G4)




G4$Overweight <- ifelse(G4$BMI >= 25 , "Overweight", "Normal weight")
View(G4)



mydata1 <- G4[G4$Overweight == "Overweight",]
View(mydata1)



mydata2 <- G4[G4$age > 60 & G4$Overweight == "Normal weight",]
View(mydata2)




library(ggplot2)



draw_point <- ggplot(G4, aes(x = height, y = weight)) + 
  geom_point(alpha = 0.5 , color = "blue") +theme_dark() 
draw_point



draw_line <- ggplot(data = G4, mapping = aes(x = height, y = weight)) + geom_line(alpha = 0.5 , color = "blue") +
  facet_wrap(vars(age_classification)) +
  labs(title = "The observed obesity through time",
       x = "The height",
       y = "The weight") +
  theme_dark() 
draw_line



draw_hist<-ggplot(G4, aes(x=age)) + 
  geom_histogram(color="blue", fill="white",alpha = 0.5, bins=30) +
  theme_dark() +
  labs(x="Passenger age", y="Passenger count") + 
  ggtitle("Age of Passengers")
draw_hist


draw_bar <- ggplot(G4, aes(x=age , fill = sex)) +
  geom_bar() + 
  theme_dark() +
  labs(x="Passenger age", y="Passenger count", title = "Obesity Rate")
draw_bar


data <- read.csv("ClassData.csv", header = TRUE)

data
summary(data)

####### GPA #######
maxGPA = max(data$GPA)
minGPA = min(data$GPA)
avgGPA = mean(data$GPA)
medianGPA = median(data$GPA)

####### Work-Exp #######
maxWE = max(data$Years.of.work.experience)
minWE = min(data$Years.of.work.experience)
avgWE = mean(data$Years.of.work.experience)
medianWE = median(data$Years.of.work.experience)

####### Mode of a Salary #######
#modeExpSal = getmode(data$Expected.Salary.after.graduation)
#modeSal = getmode(data$Latest.salary..per.year.)
temp <- table(as.vector(data$Expected.Salary.after.graduation))
names(temp) [temp == max(temp)]

# Another method by creating function mode and then executing it.
mode <- function(num){
  unique_num = unique(num)
  unique_num[which.max(tabulate(match(num, unique_num)))]
}

mode(data$Expected.Salary.after.graduation)
mode(data$Latest.salary..per.year.)

##### Percentage of people who got Co-op #####
class.strength = length(as.vector(data$Coops.Internships..YN.))
intern.strength = length(data$Coops.Internships..YN.[data$Coops.Internships..YN. == "Y"])
fresher.strength = length(data$Coops.Internships..YN.[data$Coops.Internships..YN. == "N"])

intern.percentage = intern.strength/class.strength * 100
fresher.percentage = fresher.strength/class.strength * 100

####### more than 500 contacts on linkedIn #######
linkedin.contacts = length(which(data$Number.of.contacts.on.Linkedin > 500))

####### Interquartile Range for salary after Graduation #######
iqr.range = IQR(data$Expected.Salary.after.graduation)


####### PRINT ALL OUTPUTS #######
sprintf("The MINIMUM GPA in the dataset is: %s",minGPA)
sprintf("The MAXIMUM GPA in the dataset is: %s", maxGPA)
sprintf("The AVERAGE GPA in the dataset is: %s", avgGPA)
sprintf("The MEDIAN GPA in the dataset is: %s", medianGPA)

cat("The MINIMUM WorkEX in the dataset is: ", minWE)
cat("The MAXIMUM WorkEX in the dataset is: ", maxWE)
cat("The AVERAGE WorkEX in the dataset is: ", avgWE)
cat("The MEDIAN WorkEX in the dataset is: ", minGPA)

cat("The MODE in the expected Salary column is: ", mode(data$Expected.Salary.after.graduation))
cat("The MODE in the current Salary column is: ", mode(data$Latest.salary..per.year.))

sprintf("There are %s percent of students in class, who has got a Coop / Internship", intern.percentage)
sprintf("There are %s percent of students in class, who hasn't got a Coop / Internship yet", fresher.percentage)

sprintf("There are %s students having more than 500 contacts in our students", linkedin.contacts)

print(paste("The Inter-quartile range for salary after Graduation is: ", iqr.range))



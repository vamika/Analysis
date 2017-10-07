x=read.csv("C:\\Users\\Vamika Razdan\\Downloads\\hack\\Book1.csv")
View(x)
max(x$Literacy.Rate)
min(x$Literacy.Rate)
filter(x,Literacy.Rate==90.9)#kerela
filter(x,Literacy.Rate==47)#bihar
max(x$Drop.out.Classes..I.X.)#Nagaland
filter(x,Drop.out.Classes..I.X.==97.29)
min(x$Drop.out.Classes..I.X.)
filter(x,Drop.out.Classes..I.X.==-6.98)#HP
max(x$Gross.Enrollment.Ratio..Classes...I.VIII.)
min(x$Gross.Enrollment.Ratio..Classes...I.VIII.)
filter(x,Gross.Enrollment.Ratio..Classes...I.VIII.==129.65)#Manipur,dropout=43
filter(x,Gross.Enrollment.Ratio..Classes...I.VIII.==58.75)#Lakshwadeep , dropout=18.88,LITERACY RATE=86.7
max(x$Pupil.Teacher.Ratio)
min(x$Pupil.Teacher.Ratio)
filter(x,Pupil.Teacher.Ratio==17)#Mizoram
filter(x,Pupil.Teacher.Ratio==104)#bihar
max(x$Elementary.School.per.lakh.population)
min(x$Elementary.School.per.lakh.population)
filter(x,Elementary.School.per.lakh.population==317)
filter(x,Elementary.School.per.lakh.population==3)
max(x$Tenth.Plan.Sarva.Siksha.Abhiyan.Expenditure...Rs.cr.)
filter(x,Tenth.Plan.Sarva.Siksha.Abhiyan.Expenditure...Rs.cr.==6836.31)
max(x$Tenth.Plan.Sarva.Siksha.Abhiyan.Expenditure.as.percentage.of.Total)
filter(x,Tenth.Plan.Sarva.Siksha.Abhiyan.Expenditure.as.percentage.of.Total==19.27)
max(x$Tenth.Plan.Sarva.Siksha.Abhiyan.Expenditure...Per.capita.6.14.age)
filter(x,Tenth.Plan.Sarva.Siksha.Abhiyan.Expenditure...Per.capita.6.14.age==8826)
min(x$Tenth.Plan.Sarva.Siksha.Abhiyan.Expenditure...Rs.cr.)
filter(x,Tenth.Plan.Sarva.Siksha.Abhiyan.Expenditure...Rs.cr.==.1)
min(x$Tenth.Plan.Sarva.Siksha.Abhiyan.Expenditure...Per.capita.6.14.age)
filter(x,Tenth.Plan.Sarva.Siksha.Abhiyan.Expenditure...Per.capita.6.14.age==53)#lakshwadeep
############################################################################################################
####GER####
y=read.csv("C:\\Users\\Vamika Razdan\\Downloads\\hack\\GERclasswise.csv")
View(y)
y$All.Categories...Classes.I.V.6.10.Years....Girls
y$All.Categories...Classes.VI.VIII.11.13.Years....Girls
y$All.Categories...Classes.I.V.6.10.Years....Boys
y$All.Categories...Classes.VI.VIII.11.13.Years....Boys
max(y$All.Categories...Classes.I.V.6.10.Years....Boys)
min(y$All.Categories...Classes.I.V.6.10.Years....Boys)
min(y$All.Categories...Classes.I.V.6.10.Years....Girls)
max(y$All.Categories...Classes.I.V.6.10.Years....Girls)

filter(y,All.Categories...Classes.I.V.6.10.Years....Boys==136.3)#Arunachal Pradesh
filter(y,All.Categories...Classes.I.V.6.10.Years....Boys==79.9)#assam
filter(y,All.Categories...Classes.I.V.6.10.Years....Girls==83.2)#Assam
filter(y,All.Categories...Classes.I.V.6.10.Years....Girls==135.8)#manipur

max(y$All.Categories...Classes.VI.VIII.11.13.Years....Boys)
min(y$All.Categories...Classes.VI.VIII.11.13.Years....Boys)
min(y$All.Categories...Classes.VI.VIII.11.13.Years....Girls)
max(y$All.Categories...Classes.VI.VIII.11.13.Years....Girls)

filter(y,All.Categories...Classes.VI.VIII.11.13.Years....Boys==115.8)#Goa boys
filter(y,All.Categories...Classes.VI.VIII.11.13.Years....Boys==60.2)#Nagaland min boys
filter(y,All.Categories...Classes.VI.VIII.11.13.Years....Girls==62.4)#Nagaland min girls
filter(y,All.Categories...Classes.VI.VIII.11.13.Years....Girls==117.6)#Lakshdweep max girls
############################################LITERACY REALTED TO PUPIL TEACHER RATIO################################################
y1=read.csv("C:\\Users\\Vamika Razdan\\Downloads\\hack\\Book1.csv")
View(y1)
outs=lm(Literacy.Rate~Drop.out.Classes..I.X.,data=y1)
summary(outs)
outs1=lm(Literacy.Rate~Gross.Enrollment.Ratio..Classes...I.VIII.,data=y1)
summary(outs1)
outs2=lm(Literacy.Rate~Pupil.Teacher.Ratio,data=y1)
summary(outs2)
outs3=lm(Literacy.Rate~Tenth.Plan.Sarva.Siksha.Abhiyan.Expenditure...Per.capita.6.14.age,data=y1)
summary(outs3)
outs3=lm(Literacy.Rate~Availability.Within.School.Premises...Drinking.Water,data=y1)
summary(outs3)
outs4=lm(Literacy.Rate~Availability.Within.School.Premises...Playground,data=y1)
summary(outs4)
outs5=lm(Literacy.Rate~Availability.Within.School.Premises...Usable.Urinal,data=y1)
summary(outs5)
####################################DROPOUTS NOT RELATED TO ANY FACTOR################################################################
y1=read.csv("C:\\Users\\Vamika Razdan\\Downloads\\hack\\Book1.csv")
View(y1)
outs11=lm(Drop.out.Classes..I.X.~Literacy.Rate,data=y1)
summary(outs11)
outs12=lm(Drop.out.Classes..I.X.~Gross.Enrollment.Ratio..Classes...I.VIII.,data=y1)
summary(outs12)
outs23=lm(Drop.out.Classes..I.X.~Pupil.Teacher.Ratio,data=y1)
summary(outs23)
outs34=lm(Drop.out.Classes..I.X.~Tenth.Plan.Sarva.Siksha.Abhiyan.Expenditure...Per.capita.6.14.age,data=y1)
summary(outs34)
outs35=lm(Drop.out.Classes..I.X.~Availability.Within.School.Premises...Drinking.Water,data=y1)
summary(outs35)
outs46=lm(Drop.out.Classes..I.X.~Availability.Within.School.Premises...Playground,data=y1)
summary(outs46)
outs57=lm(Drop.out.Classes..I.X.~Availability.Within.School.Premises...Usable.Urinal,data=y1)
summary(outs57)
#########################################NOT RELATED#######################################################
y12=read.csv("C:\\Users\\Vamika Razdan\\Downloads\\hack\\datafile-3.csv")
View(y12)
outs123<-lm(Increase.in.Attendance...age.of.Sample.School.~Other.Contributing.Factors.for.Increase.in.Attendance,data=y12)
summary(outs123)
########################################RELATED#########################################################
y=read.csv("C:\\Users\\Vamika Razdan\\Downloads\\hack\\LiteracyRate in r and sc.csv")
View(y)
sx=lm(Male~Female,data=y)
summary(sx)
#######################################################################################################
h5=read.csv("C:\\Users\\Vamika Razdan\\Downloads\\hack\\Dropreason.csv")
View(h5)
h5$ï..Reasons
h5$Boys
h5$Girls
max(h5$Boys)
min(h5$Girls)
filter(h5,Boys==37.2)#kerela
filter(h5,Girls==0.3)

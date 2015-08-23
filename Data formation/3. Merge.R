#Data science specialisation Capstone
#A.J.Nicholson
#Code chunk 3: Merge


#Purpose: 
#1. To join together the training set (75% of whole (15/20 samples))
#2. Merged by alphabet so that frequencies from each source could be combined to give an overall frequency

######################

#Step 1: load functions (see MergeData.R for function details)
setwd("filepath")
source("MergeData.R")

########################

#Step 2: Select test and train, randomly 
set.seed(8902)
x<-sample(1:20,20,replace=FALSE)# first 15= training set


#####################

#Step 3: set up file paths, combined with x to give file to be read  
#e.gFirst file read will be file[x[1]]
file<-c("./data/data1.csv","./data/data2.csv","./data/data3.csv","./data/data4.csv","./data/data5.csv",
        "./data/data6.csv","./data/data7.csv","./data/data8.csv","./data/data9.csv","./data/data10.csv",
        "./data/data11.csv","./data/data12.csv","./data/data13.csv","./data/data14.csv","./data/data15.csv",
        "./data/data16.csv","./data/data17.csv","./data/data18.csv","./data/data19.csv","./data/data20.csv")

#################

#Step 4: Regular expressions for alphabet split 
alpha<-c("^[a:c].","^[d:f].","^[g:j].","^[k:m].","^[n:p].","^[q:s].","^[t:v].","^[w:z].","^[#]")


###############

#Step 5: Creating Training set
#15 files will be loaded in 3 sets to help with memory issues
#Code cycled 9 times for every variation of alpha (currently set to abc)

#set 1: 1:5
#Load data and extract relevant words
one<-readin(1,1)
two<-readin(2,1)
three<-readin(3,1)
four<-readin(4,1)
five<-readin(5,1)

#Merge data
A<-mergedata(one,two)
A<-mergedata(A,three)
B<-mergedata(four,five)
A<-mergedata(A,B)

#remove original data
rm(one,two,three,four,five,B)

#write merged file out (to be read in later)
write.csv(A,"abc.csv",row.names=FALSE)
rm(A)

#set2
one<-readin(6,1)
two<-readin(7,1)
three<-readin(8,1)
four<-readin(9,1)
five<-readin(10,1)

#merge
B<-mergedata(one,two)
B<-mergedata(B,three)
C<-mergedata(four,five)
B<-mergedata(B,C)
rm(one,two,three,four,five,C)
#merege with last set
A<-read.csv("abc.csv")
A<-mergedata(A,B)
rm(B)
#write out
write.csv(A,"abc.csv",row.names=FALSE)
rm(A)

#set3
one<-readin(11,1)
two<-readin(12,1)
three<-readin(13,1)
four<-readin(14,1)
five<-readin(15,1)
#merge
B<-mergedata(one,two)
B<-mergedata(B,three)
C<-mergedata(four,five)
B<-mergedata(B,C)
rm(one,two,three,four,five,C)
#merge with other sets
A<-read.csv("abc.csv")
A<-mergedata(A,B)
rm(B)

#order by freqencies for next stage
A<-A[order(-A$Freq1,-A$Freq2,-A$Freq3,-A$Freq4),]
#write out final alphabet dataset
write.csv(A,"abc.csv",row.names=FALSE)
rm(A)




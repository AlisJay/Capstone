#Data science specialisation Capstone
#A.J.Nicholson
#Code chunk 4: Split

#Purpose: 
#1.Join alphabet training data together 
#2. split into final datasets by word frequency 
#one.csv containing the highest frequency words

setwd("filepath")

#################################

#Step 1: Determine splits
#get word freqencies
abc<-read.csv("./data/abc.csv");abc<-abc$Freq1
def<-read.csv("./data/def.csv");def<-def$Freq1
ghij<-read.csv("./data/ghij.csv");ghij<-ghij$Freq1
klm<-read.csv("./data/klm.csv");klm<-klm$Freq1
nop<-read.csv("./data/nop.csv");nop<-nop$Freq1
qrs<-read.csv("./data/qrs.csv");qrs<-qrs$Freq1
tuv<-read.csv("./data/tuv.csv");tuv<-tuv$Freq1
wxyz<-read.csv("./data/wxyz.csv");wxyz<-wxyz$Freq1

#combine
Freq<-c(abc,def,ghij,klm,nop,qrs,tuv,wxyz)
rm(abc,def,ghij,klm,nop,qrs,tuv,wxyz)

# calculate percentiles (need 8 even groups)
quantile(Freq, c(0.125,0.25,0.375,0.5,0.625,0.75,0.875)) 
#results= 439,2006,5703,15526,48439,129208,581698

####################################

#Step 2: load data (currently set to create one.csv)
abc<-read.csv("./data/abc.csv")
#abc<-abc[abc$Freq1<=439,]#eight.csv
#abc<-abc[abc$Freq1 %in% 440:2006,]#seven.csv
#abc<-abc[abc$Freq1 %in% 2007:5703,]#six.csv
#abc<-abc[abc$Freq1 %in% 5704:15526,]#five.csv
#abc<-abc[abc$Freq1 %in% 15527:48439,]#four.csv
#abc<-abc[abc$Freq1 %in% 48440:129208,]#three.csv
#abc<-abc[abc$Freq1 %in% 129209:581698,]#two.csv
abc<-abc[abc$Freq1>=581699,]#one.csv
row.names(abc)<-NULL

def<-read.csv("./data/def.csv")
#def<-def[def$Freq1<=439,]
#def<-def[def$Freq1 %in% 440:2006,]
#def<-def[def$Freq1 %in% 2007:5703,]
#def<-def[def$Freq1 %in% 5704:15526,]
#def<-def[def$Freq1 %in% 15527:48439,]
#def<-def[def$Freq1 %in% 48440:129208,]
#def<-def[def$Freq1 %in% 129209:581698,]
def<-def[def$Freq1>=581699,]
row.names(def)<-NULL


ghij<-read.csv("./data/ghij.csv")
#ghij<-ghij[ghij$Freq1<=439,]
#ghij<-ghij[ghij$Freq1 %in% 440:2006,]
#ghij<-ghij[ghij$Freq1 %in% 2007:5703,]
#ghij<-ghij[ghij$Freq1 %in% 5704:15526,]
#ghij<-ghij[ghij$Freq1 %in% 15527:48439,]
#ghij<-ghij[ghij$Freq1 %in% 48440:129208,]
#ghij<-ghij[ghij$Freq1 %in% 129209:581698,]
ghij<-ghij[ghij$Freq1>=581699,]
row.names(ghij)<-NULL

klm<-read.csv("./data/klm.csv")
#klm<-klm[klm$Freq1<=439,]
#klm<-klm[klm$Freq1 %in% 440:2006,]
#klm<-klm[klm$Freq1 %in% 2007:5703,]
#klm<-klm[klm$Freq1 %in% 5704:15526,]
#klm<-klm[klm$Freq1 %in% 15527:48439,]
#klm<-klm[klm$Freq1 %in% 48440:129208,]
#klm<-klm[klm$Freq1 %in% 129209:581698,]
klm<-klm[klm$Freq1>=581699,]
row.names(klm)<-NULL


nop<-read.csv("./data/nop.csv")
#nop<-nop[nop$Freq1<=439,]
#nop<-nop[nop$Freq1 %in% 440:2006,]
#nop<-nop[nop$Freq1 %in% 2007:5703,]
#nop<-nop[nop$Freq1 %in% 5704:15526,]
#nop<-nop[nop$Freq1 %in% 15527:48439,]
#nop<-nop[nop$Freq1 %in% 48440:129208,]
#nop<-nop[nop$Freq1 %in% 129209:581698,]
nop<-nop[nop$Freq1>=581699,]
row.names(nop)<-NULL

qrs<-read.csv("./data/qrs.csv")
#qrs<-qrs[qrs$Freq1<=439,]
#qrs<-qrs[qrs$Freq1 %in% 440:2006,]
#qrs<-qrs[qrs$Freq1 %in% 2007:5703,]
#qrs<-qrs[qrs$Freq1 %in% 5704:15526,]
#qrs<-qrs[qrs$Freq1 %in% 15527:48439,]
#qrs<-qrs[qrs$Freq1 %in% 48440:129208,]
#qrs<-qrs[qrs$Freq1 %in% 129209:581698,]
qrs<-qrs[qrs$Freq1>=581699,]
row.names(qrs)<-NULL


tuv<-read.csv("./data/tuv.csv")
#tuv<-tuv[tuv$Freq1<=439,]
#tuv<-tuv[tuv$Freq1 %in% 440:2006,]
#tuv<-tuv[tuv$Freq1 %in% 2007:5703,]
#tuv<-tuv[tuv$Freq1 %in% 5704:15526,]
#tuv<-tuv[tuv$Freq1 %in% 15527:48439,]
#tuv<-tuv[tuv$Freq1 %in% 48440:129208,]
#tuv<-tuv[tuv$Freq1 %in% 129209:581698,]
tuv<-tuv[tuv$Freq1>=581699,]
row.names(tuv)<-NULL

wxyz<-read.csv("./data/wxyz.csv")
#wxyz<-wxyz[wxyz$Freq1<=439,]
#wxyz<-wxyz[wxyz$Freq1 %in% 440:2006,]
#wxyz<-wxyz[wxyz$Freq1 %in% 2007:5703,]
#wxyz<-wxyz[wxyz$Freq1 %in% 5704:15526,]
#wxyz<-wxyz[wxyz$Freq1 %in% 15527:48439,]
#wxyz<-wxyz[wxyz$Freq1 %in% 48440:129208,]
#wxyz<-wxyz[wxyz$Freq1 %in% 129209:581698,]
wxyz<-wxyz[wxyz$Freq1>=581699,]
row.names(wxyz)<-NULL

###################################

#Step 4: Join data together
x<-rbind(abc,def,ghij,klm,nop,qrs,tuv,wxyz)
rm(abc,def,ghij,klm,nop,qrs,tuv,wxyz)

#####################################

#Step 5: Reorder by frequencies
x<-x[with(x,order(-Freq1,-Freq2,-Freq3,-Freq4)),]
row.names(x)<-NULL

#######################################

#Step 6: write out a csv file
write.csv(x,"./data/one.csv",row.names=FALSE)
rm(x)


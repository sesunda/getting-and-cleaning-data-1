#read data
x<-read.table("test/X_test.txt")   
y<-read.table("train/X_train.txt")

#add variable name
n<-read.table("features.txt")
n<-n[,2]
names(x)<-n
names(y)<-n

#add activity name
m<-read.table("test/y_test.txt")
t<-read.table("train/y_train.txt")
o<-read.table("activity_labels.txt")
i<-1
while(i<=2947){m[i,2]<-o[m[i,1],2];i<-i+1;}
test<-cbind(m[,2],x)
i<-1
while(i<=7352){t[i,2]<-o[t[i,1],2];i<-i+1;}
train<-cbind(t[,2],y)
n<-as.vector(n)
n<-c("activity",n)
names(train)<-n
names(test)<-n

#merge test data and train data
dataset<-rbind(test,train)

#extracts only the measurements on the mean and standard deviation for each measurement. 
i<-1
while(i<=562){k[i]<-grepl("mean",n[i])|grepl("std",n[i]);i<-i+1}
k[1]<-T
tidydataset<-dataset[,k]
subt<-read.table("test/subject_test.txt")
subtrain<-read.table("train/subject_train.txt")
sub<-rbind(subt,subtrain)
tidydataset<-cbind(sub,tidydataset)

#creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidydataset2<-tidydataset;
tidyset<-tidydataset2[1,];
i<-1
while(nrow(tidydataset2)!=0){
  kk<-2
  while((kk<=nrow(tidydataset2))&(tidydataset2[kk,1]==tidydataset2[1,1])&(tidydataset2[kk,2]==tidydataset2[1,2])){
    kk<-kk+1;
  }
  xx<-tidydataset2[c(1:kk-1),];
  yy<-apply(xx[,c(3:81)],2,mean);
  tidyset[i,]<-cbind(xx[1,c(1:2)],t(yy));
  tidydataset2<-tidydataset2[-c(1:kk-1),];
  i<-i+1;
}
write.table(tidyset,file="tidydataset.txt",row.name=F)
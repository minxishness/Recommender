library(plyr)
library(reshape2)
#gets positions of records to be used for testing
getTestPos<-function(groupId,gsoData) {
    groupPos<-which(gsoData$groupId==groupId)
    return(groupPos[1])
}

begTime <- Sys.time()
gsoData<-read.csv("GroupMembersNew.csv")
colnames(gsoData)<-c("favouriteId","userId","entityId","addedDate", "groupId", "isVisible", "entityName", "groupName" )
gsoData$addedDate<-as.Date(gsoData$addedDate, "%d/%m/%Y")
gsoData<-arrange(gsoData,desc(addedDate))
#get relevant columns only
gsData<-gsoData[,c(3,5)]
#get Unique group ID's to get suggestions for
# groupsUnique<-gsTest[,2]
# groupsUnique<-groupsUnique[(!duplicated(groupsUnique))]
# # testPos<-sapply(groupsUnique,getTestPos,gsoData=gsoData)
# #generate testing and training sets
# gsTest<-gsData[testPos,]
# gsData<-gsData[-testPos,]
gsData$score<-1
recoData<-acast(gsData, groupId ~ entityId ~ score)
recoData<-as.data.frame(recoData)
cnames<-colnames(recoData)
cnames<-substr(cnames,1,nchar(cnames)-2)
colnames(recoData)<-cnames
recData<-as.matrix(recoData)
recData[is.na(recData)]<-0


recData<-as(recData, "binaryRatingMatrix")
r<-Recommender(recData,method="UBCF")
recom<-predict(r,recData, n=10)
# r1<-Recommender(recData[1:1300],method="IBCF")
# recom1<-predict(r,recData[1301:1461], n=10)


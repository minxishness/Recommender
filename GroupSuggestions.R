library(plyr)

#finds the similarity score for two groups
matchScore<-function(group1,group2Id, contData) {
   group2<-contData[contData$groupId==group2Id,]
   groupBoth<-c(as.vector(group1$entityId),as.vector(group2$entityId))
   common<-(duplicated(groupBoth)==TRUE)
   common<-length(common[common])
    # adjust score to include/exclude length of group 1, etc
   score<-(common/((nrow(group1)+nrow(group2)))) 
   return(score)  
}

#calculates the match coefficient for an Entity for a specific group
calcEntitySug<-function(entityId,group,contData) {
    contGroups<-as.data.frame(contData[contData$entityId==entityId,2])
    matchs<-sum(apply(contGroups,1,matchScore, group1=group, contData=contData))
    return(c(entityId, group[1,2], matchs))
}

#main function - calculates scores for all suggestions for a group
calcGroupSug<-function(groupId) {
    group<-gsData[gsData$groupId==groupId,]    
    #find all groups which contain any of the entities from our main group
    contGroups<-gsData[gsData$entityId %in% group$entityId,]
    #remove main group from contGroups
    contGroups<-contGroups[contGroups$groupId!=groupId,]
    contGroups<-gsData[gsData$groupId %in% contGroups$groupId,]
    #get list of entities to check
    entitiesCheck<-contGroups[,1]
    entitiesCheck<-entitiesCheck[(!duplicated(entitiesCheck))]
    #remove all entities that are already in the group from list of entities to check
    entitiesCheck<-entitiesCheck[!(entitiesCheck %in% group$entityId)]
    groupRes<-sapply(entitiesCheck,calcEntitySug, group=group, contData=contGroups)
     return(groupRes)
}

#gets positions of records to be used for testing
#testing is done by emoving the last entity from each group and then checking whether it was in the recommended list
getTestPos<-function(groupId,gsoData) {
    groupPos<-which(gsoData$groupId==groupId)
    #exclude groups with just one entity from test set
    if (length(groupPos)>1)
        return(groupPos[1])
}

#returns the position of the test entity in the recommendations list
testRes<-function(groupId,comboRes, gsTest) {
    entityId<-gsTest[gsTest$groupId==groupId,1]  
    groupSugs<-comboRes[comboRes$groupId==groupId,]
    pos<-which(groupSugs$entityId==entityId)
    return(pos)
}

#prints out test summary
testSummary<-function(resTesting) {
    print("Spots 1-5")
    print(c(length(resTesting[resTesting[,1]<6,]),length(resTesting[resTesting[,1]<6,])/nrow(gsTest)))
    print("Spots 6-10")
    print(c(length(resTesting[((resTesting[,1]>5) & (resTesting[,1]<11)),]), length(resTesting[((resTesting[,1]>5) & (resTesting[,1]<11)),])/nrow(gsTest)))
    print("Spots 11-20")
    print(c(length(resTesting[((resTesting[,1]>10) & (resTesting[,1]<21)),]),length(resTesting[((resTesting[,1]>10) & (resTesting[,1]<21)),])/nrow(gsTest)))
    print("Other")
    print(c(length(resTesting[resTesting[,1]>21,]),length(resTesting[resTesting[,1]>21,])/nrow(gsTest)))
}
    
begTime <- Sys.time()
gsoData<-read.csv("GroupMembers14Apr2015.csv")
colnames(gsoData)<-c("favouriteId","userId","entityId","addedDate", "groupId", "isVisible", "entityName", "groupName" )
gsoData$addedDate<-as.Date(gsoData$addedDate, "%d/%m/%Y")
gsoData<-arrange(gsoData,desc(addedDate))
#get relevant columns only
gsData<-gsoData[,c(3,5)]
#get Unique group ID's to get suggestions for
groupsUnique<-gsData[,2]
groupsUnique<-groupsUnique[(!duplicated(groupsUnique))]
#get positions of test records
#COMMENT OUT ONCE LIVE
testPos<-sapply(groupsUnique,getTestPos,gsoData=gsoData)
testPos<-(unlist(testPos))
#generate testing and training sets 
#COMMENT OUT ONCE LIVE
gsTest<-gsData[testPos,]
gsData<-gsData[-testPos,]
#get suggestions
comboRes<-sapply(groupsUnique,calcGroupSug)
comboRes <- data.frame(matrix(unlist(comboRes), ncol=3, byrow=T))
colnames(comboRes)<-c("entityId", "groupId", "score" )
comboRes<-arrange(comboRes, groupId, desc(score))
View(comboRes)
fn<-paste("groupsEntitySuggestions", Sys.time(),,".csv", sep="")
write.csv(comboRes,fn)
runTime <- Sys.time()-begTime
runTime

#test results
#COMMENT OUT ONCE LIVE
resTesting<-sapply(groupsUnique, FUN=testRes, comboRes=comboRes, gsTest=gsTest)
resTesting<-as.data.frame(unlist(resTesting))
colnames(resTesting)<-"pos"
resTesting<-arrange(resTesting,pos)
#test results random suggestions (equivalent to non-user baased simple group member look-up)
#COMMENT OUT ONCE LIVE
comboRes1<-comboRes
comboRes1$score<-runif(nrow(comboRes1),0,1.5)
comboRes1<-arrange(comboRes1, groupId, desc(score))
resTesting1<-sapply(groupsUnique, FUN=testRes, comboRes=comboRes1, gsTest=gsTest)
resTesting1<-as.data.frame(unlist(resTesting1))
colnames(resTesting1)<-"pos"
resTesting<-arrange(resTesting1,pos)
print("Results")
testSummary(resTesting)
print("Random group filtering results")
testSummary(resTesting1)

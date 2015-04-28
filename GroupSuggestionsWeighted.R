library(plyr)

#gets positions of records to be used for testing
getTestPos<-function(groupId,gsoData) {
    groupPos<-which(gsoData$groupId==groupId)
    return(groupPos[1])
}

#tests results
testRes<-function(groupId,comboUnique, gsTest) {
    entityId<-gsTest[gsTest$groupId==groupId,1]  
    groupSugs<-comboRes[comboRes$groupId==groupId,]
    pos<-which(groupSugs$entityId==entityId)
    return(pos)
}

testSummary<-function(resTesting) {
    print("Spots 1-4")
    print(c(length(resTesting[resTesting[,1]<5,]),length(resTesting[resTesting[,1]<5,])/length(groupsUnique)))
    print("Spots 5-8")
    print(c(length(resTesting[((resTesting[,1]>4) & (resTesting[,1]<9)),]), length(resTesting[((resTesting[,1]>4) & (resTesting[,1]<9)),])/length(groupsUnique)))
    print("Spots 8-20")
    print(c(length(resTesting[((resTesting[,1]>8) & (resTesting[,1]<21)),]),length(resTesting[((resTesting[,1]>8) & (resTesting[,1]<21)),])/length(groupsUnique)))
    print("Other")
    print(c(length(resTesting[resTesting[,1]>21,]),length(resTesting[resTesting[,1]>21,])/length(groupsUnique)))
}
    
#finds the similarity score for two groups
matchScore<-function(group1,group2Id, contData) {
   #group1<-gsData[gsData$groupId==group1Id,]
   group2<-contData[contData$groupId==group2Id,]
   groupBoth<-c(as.vector(group1$entityId),as.vector(group2$entityId))
   #adjust common to introduce weighting
#    common<-(duplicated(groupBoth)==TRUE)
#    common<-length(common[common])
   common<-group2[group2$entityId %in% group1$entityId,]
   common<-sum(common$weight)
    # adjust score to include/exclude length of group 1, etc
   score<-(common/(nrow(group1)+nrow(group2))) 
   return(score)  
}

#calculates the match coefficient for an Entity for a specific group
calcEntitySug<-function(entityId,group,contData) {    
    #group<-gsData[gsData$groupId==groupId,]    
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
    
begTime <- Sys.time()
gsoData<-read.csv("GroupMembersNew.csv")
colnames(gsoData)<-c("favouriteId","userId","entityId","addedDate", "groupId", "isVisible", "entityName", "groupName" )
#gsoData<-gsoData[gsoData$groupName!="Favourites",]
gsoData$addedDate<-as.Date(gsoData$addedDate, "%d/%m/%Y")
gsoData<-arrange(gsoData,desc(addedDate))
#get relevant columns only
gsData<-gsoData[,c(3,5,4)]
#get last date an entity was added to a group to use it for weighing
entitiesUnique<-gsData[,1]
entitiesUnique<-which((!duplicated(entitiesUnique)))
entitiesMax<-gsData[entitiesUnique,c(1,3)]
entitiesMax$addedDate<-as.numeric(entitiesMax$addedDate)
gsData$addedDate<-as.numeric(gsData$addedDate)
for (i in (1:nrow(gsData))) 
    gsData[i,3]<-entitiesMax[entitiesMax$entityId==gsData[i,1],2]
maxDate<-max(gsData$addedDate)
minDate<-min(gsData$addedDate)
# #adjust the weighing here
gsData$addedDate<-(gsData$addedDate-minDate)/(maxDate-minDate)
colnames(gsData)[3]<-"weight"
#get Unique group ID's to get suggestions for
groupsUnique<-gsData[,2]
groupsUnique<-groupsUnique[(!duplicated(groupsUnique))]
testPos<-sapply(groupsUnique,getTestPos,gsoData=gsoData)
#generate testing and training sets
gsTest<-gsData[testPos,]
gsData<-gsData[-testPos,]
comboRes<-sapply(groupsUnique,calcGroupSug)
comboRes <- data.frame(matrix(unlist(comboRes), ncol=3, byrow=T))
colnames(comboRes)<-c("entityId", "groupId", "score" )
comboRes<-arrange(comboRes, groupId, desc(score))
View(comboRes)
runTime <- Sys.time()-begTime
write.csv(comboRes,"newres.csv")
runTime
resTesting<-sapply(groupsUnique, FUN=testRes, comboUnique=comboUnique, gsTest=gsTest)
resTesting<-as.data.frame(unlist(resTesting))
colnames(resTesting)<-"pos"
resTesting<-arrange(resTesting,pos)
testSummary(resTesting)

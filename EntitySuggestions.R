library(plyr)

#calculate score for an entity pair
entityMatchScore<-function(entityId,contData, baseEntityId) {    
    occurances<-sum(contData$entityId == entityId)
    matchs<-occurances/nrow(contData)
   
    return(c(baseEntityId, entityId, matchs,occurances,nrow(contData)))
}

#calculatee scores for all recommendations for an entity
calcEntitySug<-function(entityId) {
   #find all groups which contain the entity
    contGroups<-gsData[gsData$entityId %in% entityId, 2]
    #find all entites we need to calculate score for
    contData<-gsData[gsData$groupId %in% contGroups,]
    entitiesCheck<-contData
    entitiesCheck<-entitiesCheck[entitiesCheck$entityId!=entityId,1]
    entitiesCheck<-entitiesCheck[(!duplicated(entitiesCheck))]
    entityRes<-sapply(entitiesCheck,entityMatchScore, contData=contData, baseEntityId=entityId)
    return(entityRes)
}

begTime <- Sys.time()
gsoData<-read.csv("GroupMembers14Apr2015.csv")
colnames(gsoData)<-c("favouriteId","userId","entityId","addedDate", "groupId", "isVisible", "entityName", "groupName" )
gsoData$addedDate<-as.Date(gsoData$addedDate, "%d/%m/%Y")
gsoData<-arrange(gsoData,desc(addedDate))
#get relevant columns only
gsData<-gsoData[,c(3,5)]
#get Unique entity ID's to get suggestions for
entitiesUnique<-gsData[,1]
entitiesUnique<-entitiesUnique[(!duplicated(entitiesUnique))]
comboRes<-sapply(entitiesUnique,calcEntitySug)
comboRes <- data.frame(matrix(unlist(comboRes), ncol=5, byrow=T))
colnames(comboRes)<-c("entity1Id", "suggestionId", "score", "common_count","total_count" )
comboRes<-arrange(comboRes, desc(score))
View(comboRes)
#write.csv(comboRes,"newres.csv")
runTime <- Sys.time()-begTime
runTime
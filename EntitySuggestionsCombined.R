library(plyr)

#calculate score for an entity pair
entityMatchScore<-function(entityId,contData, baseEntityId) {    
    occurances<-sum(contData$entityId == entityId)
    matchs<-occurances/(nrow(contData))
    baseGenre<-agData[agData$entityId==baseEntityId,2]
    entityGenre<-agData[agData$entityId==entityId,2]
    commonGenre<-length(intersect(baseGenre,entityGenre))
    totalGenres<-length(entityGenre)+length(baseGenre)
    if (commonGenre>0)
        matchs<-matchs+commonGenre/totalGenres
    baseEntityPos<-which(gsoData$entityId==baseEntityId)[1]
    suggestionPos<-which(gsoData$entityId==entityId)[1]
    baseEntityName<-as.character(gsoData$entityName[baseEntityPos])
    sugEntityName<-as.character(gsoData$entityName[suggestionPos])
    return(c(baseEntityId, entityId, matchs,baseEntityName,sugEntityName))
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

#combine scores 
combineScores<-function(comboRow, groupsRes) {
    score<-comboRow[3][1]
    groupScore<-groupsRes[groupsRes$entity1Id==comboRow[1] & groupsRes$suggestionId==comboRow[2],3][1]
    if (!is.na(groupScore))
        score<-as.numeric(score)+as.numeric(groupScore)
    return(c(comboRow[1],comboRow[2], score,comboRow[4],comboRow[5]))
}
    

begTime <- Sys.time()
########################################################################
#get scores based on recently viewed
########################################################################
rwoData<-read.csv("recentlyViewed17-Apr-2014.csv")
colnames(rwoData)<-c("userId","entityId","addedDate", "entityName" )
agData<-read.csv("artistGenre.csv")
agData<-agData[,c(3,1)]
colnames(agData)<-c("entityId", "genre")
popPos<-which (agData$genre=="POPXXX")
agData<-agData[-popPos,]
rwoData$groupId<-paste(gsoData$userId,gsoData$addedDate,sep="")
#get relevant columns only
rwData<-rwoData[,c(2,5)]
#get Unique entity ID's to get suggestions for
entitiesUnique<-rwData[,1]
entitiesUnique<-entitiesUnique[(!duplicated(entitiesUnique))]
similarRes<-sapply(entitiesUnique,calcEntitySug)
similarRes <- data.frame(matrix(unlist(similarRes), ncol=5, byrow=T))
colnames(similarRes)<-c("entity1Id", "suggestionId", "score", "entityName","suggestionName" )
similarRes$score<-as.numeric(as.character(similarRes$score))

########################################################################
#Get scores based on grops
########################################################################
gsoData<-read.csv("GroupMembers17Apr2015.csv")
colnames(gsoData)<-c("favouriteId","userId","entityId","addedDate", "groupId", "isVisible", "entityName", "groupName" )
gsoData$addedDate<-as.Date(gsoData$addedDate, "%d/%m/%Y")
gsoData<-arrange(gsoData,desc(addedDate))
#get relevant columns only
gsData<-gsoData[,c(3,5)]
#get Unique entity ID's to get suggestions for
entUniqueGroups<-gsData[,1]
entUniqueGroups<-entUniqueGroups[(!duplicated(entUniqueGroups))]
groupRes<-sapply(entUniqueGroups,calcEntitySug)
groupRes <- data.frame(matrix(unlist(groupRes), ncol=5, byrow=T))
colnames(groupRes)<-c("entity1Id", "suggestionId", "score", "EntityName","SuggestionName" )
groupRes<-sapply(entitiesUnique,calcEntitySug)
groupRes <- data.frame(matrix(unlist(groupRes), ncol=5, byrow=T))
colnames(groupRes)<-c("entity1Id", "suggestionId", "score", "EntityName","SuggestionName" )
groupsRes$score<-as.numeric(as.character(groupsRes$score))
groupRes<-arrange(groupRes, desc(score))

########################################################################
#Combine scores
########################################################################
comboRes<-apply(comboRes,1, combineScores, groupsRes=groupsRes)
comboRes <- data.frame(matrix(unlist(comboRes), ncol=5, byrow=T))
colnames(comboRes)<-c("entity1Id", "suggestionId", "score", "EntityName","SuggestionName" )
comboRes$score<-as.numeric(as.character(comboRes$score))
groupRes<-arrange(groupRes, desc(score))
View(comboRes)
write.csv(comboRes,"combinedRes.csv")
runTime <- Sys.time()-begTime
runTime
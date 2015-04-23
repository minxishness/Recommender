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
    sugEntityName<-as.character(gsoData[suggestionPos,7])
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

begTime <- Sys.time()
gsoData<-read.csv("GroupMembers17Apr2015.csv")
agData<-read.csv("artistGenre.csv")
agData<-agData[,c(3,1)]
colnames(agData)<-c("entityId", "genre")
popPos<-which (agData$genre=="POPXXX")
agData<-agData[-popPos,]
colnames(gsoData)<-c("favouriteId","userId","entityId","addedDate", "groupId", "isVisible", "entityName", "groupName" )
gsoData$addedDate<-as.Date(gsoData$addedDate, "%d/%m/%Y")
gsoData<-arrange(gsoData,desc(addedDate))
#get relevant columns only
gsData<-gsoData[,c(3,5)]
#get Unique entity ID's to get suggestions for
entitiesUnique<-gsData[,1]
entitiesUnique<-entitiesUnique[(!duplicated(entitiesUnique))]
comboResGroups<-sapply(entitiesUnique,calcEntitySug)
comboResGroups <- data.frame(matrix(unlist(comboResGroups), ncol=5, byrow=T))
colnames(comboResGroups)<-c("entity1Id", "suggestionId", "score", "EntityName","SuggestionName" )
comboRes$score<-as.numeric(as.character(comboRes$score))
comboResGroups<-arrange(comboResGroups, desc(score))
View(comboResGroups)
fn<-paste("similarArtistsGroupsBased", Sys.time(),,".csv", sep="")
write.csv(comboResGroups,fn)
runTime <- Sys.time()-begTime
runTime
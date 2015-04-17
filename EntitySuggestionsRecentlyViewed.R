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

begTime <- Sys.time()
gsoData<-read.csv("recentlyViewed17-Apr-2014.csv")
colnames(gsoData)<-c("userId","entityId","addedDate", "entityName" )
agData<-read.csv("artistGenre.csv")
agData<-agData[,c(3,1)]
colnames(agData)<-c("entityId", "genre")
popPos<-which (agData$genre=="POPXXX")
agData<-agData[-popPos,]
gsoData$groupId<-paste(gsoData$userId,gsoData$addedDate,sep="")
#get relevant columns only
gsData<-gsoData[,c(2,5)]
#get Unique entity ID's to get suggestions for
entitiesUnique<-gsData[,1]
entitiesUnique<-entitiesUnique[(!duplicated(entitiesUnique))]
comboRes<-sapply(entitiesUnique,calcEntitySug)
comboRes <- data.frame(matrix(unlist(comboRes), ncol=5, byrow=T))
colnames(comboRes)<-c("entity1Id", "suggestionId", "score", "EntityName","SuggestionName" )
comboRes<-arrange(comboRes, desc(score))
View(comboRes)
write.csv(comboRes,"newres.csv")
runTime <- Sys.time()-begTime
runTime
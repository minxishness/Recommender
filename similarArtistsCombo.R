library(plyr)

#calculate score for an entity pair
entityMatchScore<-function(entityId,contData, baseEntityId, gsoData) {    
    occurances<-sum(contData$entityId == entityId)
    matchs<-occurances/(nrow(contData))
    baseGenre<-agData[agData$entityId==baseEntityId,2]
    entityGenre<-agData[agData$entityId==entityId,2]
    commonGenre<-length(intersect(baseGenre,entityGenre))
    totalGenres<-length(entityGenre)+length(baseGenre)
    #adjust here to weigh on genre
    if (commonGenre>0)
        matchs<-matchs+commonGenre/totalGenres
    baseEntityPos<-which(gsoData$entityId==baseEntityId)[1]
    suggestionPos<-which(gsoData$entityId==entityId)[1]
    baseEntityName<-as.character(gsoData$entityName[baseEntityPos])
    sugEntityName<-as.character(gsoData$entityName[suggestionPos])
    return(c(baseEntityId, entityId, matchs,baseEntityName,sugEntityName))
}

#calculatee scores for EventsEventsEventsall recommendations for an entity
calcEntitySug<-function(entityId, gsData, gsoData) {
    #find all groups which contain the entity
    contGroups<-gsData[gsData$entityId %in% entityId, 2]
    #find all entites we need to calculate score for
    contData<-gsData[gsData$groupId %in% contGroups,]
    entitiesCheck<-contData
    entitiesCheck<-entitiesCheck[entitiesCheck$entityId!=entityId,1]
    entitiesCheck<-entitiesCheck[(!duplicated(entitiesCheck))]
    entityRes<-sapply(entitiesCheck,entityMatchScore, contData=contData, baseEntityId=entityId, gsoData=gsoData)
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
gsoData<-read.csv("GroupMembers17Apr2015.csv")
rwoData<-read.csv("recentlyViewed17-Apr-2014.csv")
colnames(rwoData)<-c("userId","entityId","addedDate", "entityName" )


rwoData$addedDate<-as.Date(rwoData$addedDate, "%d/%m/%Y")
rwoData<-rwoData[rwoData$addedDate>'2014-01-01',]



agData<-read.csv("artistGenre.csv")
agData<-agData[,c(3,1)]
colnames(agData)<-c("entityId", "genre")
popPos<-which (agData$genre=="POPXXX")
agData<-agData[-popPos,]
rwoData$groupId<-paste(rwoData$userId,rwoData$addedDate,sep="")
#get relevant columns only
rwData<-rwoData[,c(2,5)]
#get Unique entity ID's to get suggestions for
entitiesUnique<-rwData[,1]
entitiesUnique<-entitiesUnique[(!duplicated(entitiesUnique))]
recentRes<-sapply(entitiesUnique,calcEntitySug, gsData=rwData, gsoData=rwoData)
recentRes <- data.frame(matrix(unlist(recentRes), ncol=5, byrow=T))
colnames(recentRes)<-c("entity1Id", "suggestionId", "score", "entityName","suggestionName" )
recentRes$score<-as.numeric(as.character(recentRes$score))
recentTime<-Sys.time()
########################################################################
#Get scores based on groups
########################################################################

colnames(gsoData)<-c("favouriteId","userId","entityId","addedDate", "groupId", "isVisible", "entityName", "groupName" )
gsoData$addedDate<-as.Date(gsoData$addedDate, "%d/%m/%Y")
gsoData<-arrange(gsoData,desc(addedDate))
#get relevant columns only
gsData<-gsoData[,c(3,5)]
#get Unique entity ID's to get suggestions for
entUniqueGroups<-gsData[,1]
entUniqueGroups<-entUniqueGroups[(!duplicated(entUniqueGroups))]
groupRes<-sapply(entUniqueGroups,calcEntitySug, gsData=gsData, gsoData=gsoData)
groupRes <- data.frame(matrix(unlist(groupRes), ncol=5, byrow=T))
colnames(groupRes)<-c("entity1Id", "suggestionId", "score", "EntityName","SuggestionName" )
groupRes<-sapply(entitiesUnique,calcEntitySug, gsData=gsData, gsoData=gsoData)
groupRes <- data.frame(matrix(unlist(groupRes), ncol=5, byrow=T))
colnames(groupRes)<-c("entity1Id", "suggestionId", "score", "EntityName","SuggestionName" )
groupRes$score<-as.numeric(as.character(groupRes$score))
groupRes<-arrange(groupRes, desc(score))
groupTime<-Sys.time()
########################################################################
#Combine scores
########################################################################
comboRes<-merge(recentRes, groupRes, by=c("entity1Id","suggestionId"), all = TRUE)
comboRes$EntityName<-as.character(comboRes$EntityName)
comboRes$SuggestionName<-as.character(comboRes$SuggestionName)
comboRes$entityName<-as.character(comboRes$entityName)
comboRes$entityName[is.na(comboRes$entityName)]<-comboRes$EntityName[is.na(comboRes$entityName)]
comboRes$suggestionName<-as.character(comboRes$suggestionName)
comboRes$suggestionName[is.na(comboRes$suggestionName)]<-comboRes$SuggestionName[is.na(comboRes$suggestionName)]
comboRes<-comboRes[,-c(7,8)]
comboRes$score.y[is.na(comboRes$score.y)]<-0
comboRes$score.x[is.na(comboRes$score.x)]<-0
comboRes<-comboRes[,c(4,5,3,6,1,2)]
colnames(comboRes)<-c("entityName", "suggestionName", "recentlyViewedScore", "groupScore", "entity1Id", "suggestionId" )
#adjust here to weigh towards recentlyViewed or group
comboRes$finalScore<-comboRes$recentlyViewedScore+comboRes$groupScore
comboRes<-comboRes[,c(1,2,7,3,4,5,6)]
comboRes<-arrange(comboRes, desc(finalScore))
combineTime<-Sys.time()
fn<-paste("similarArtistsCombo", Sys.Date(),".csv", sep="")
write.csv(comboRes,fn)
recentRunTime <- recentTime-begTime
groupRunTime<-groupTime-recentTime
comboRunTIme<-combineTime-groupTime
cat("Recently viewed calc time:", recentRunTime)
cat("Groups calc time:", groupRunTime)
cat("Combo time", comboRunTIme)

library(plyr)
library(data.table)
#finds the similarity score for two groups
matchScore<-function(groupRow) {
   group1<-gsData[groupId==groupRow[1],]
   group2<-gsData[groupId==groupRow[2],]
   #groupBoth<-c(as.vector(group1$entityId),as.vector(group2$entityId))
   common<-length(intersect(group1,group2))
    # adjust score to include/exclude length of group 1, etc
   score<-(common/(nrow(group1)+nrow(group2)))
   return(score)
  
}

#calculates the match coefficient for an Entity for a specific group
calcSug<-function(comboRow) {
    entityId<-comboRow[1]
    groupId<-comboRow[2]
    matchs<-0
    group<-gsData[groupId==groupId,]    
    #if an entity is already in a group, it can't be a valid suggestion
    if (!(entityId %in% group))
        {
        
        #print("early escape")
        # else find all groups which contain the entity, calculate their match level to the main group 
        # and add that to the entity match coefficient
       # } else 
        #  {
              contGroups<-(gsData[entityId==entityId,2])
              groupCheck<-comboGroup[group1Id==groupId,]
              #print(groupCheck)
              groupCheck<-groupCheck[groupCheck[,2] %in% comboGroup,]
              
              matchs<-sum(groupCheck$res)
           
#             for (i in 1: length(contGroups) ){
#                 groupTemp<-gsData[gsData$groupId==contGroups[i],]
#                 match<-match+matchScore(group,groupTemp)  
#          #       print("found match")
#           }
        
        }

    return(c(entityId, groupId, matchs))
}


countMembers<-function(gsRow) {
    return(nrow(gsData[gsData$entityId==gsRow[1],]))
}
    
begTime <- Sys.time()
gsoData<-read.csv("GroupMembers1.csv")
colnames(gsoData)<-c("favouriteId","userId","entityId","addedDate", "groupId", "isVisible", "entityName", "groupName" )
#get relevant columns only
gsData<-gsoData[,c(3,5)]
entityCount<-apply(gsData,1,countMembers)
gsData1<-gsData[entityCount>1,]
#create a df with all unique Group/Entity ID combos so suggestion coefficient can be calculated for all groups/entites
groupsUnique<-gsData[,2]
entitiesUnique<-gsData1[,1]
entitiesUnique<-entitiesUnique[(!duplicated(entitiesUnique))]
groupsUnique<-groupsUnique[(!duplicated(groupsUnique))]
comboUnique<-sapply(groupsUnique, FUN=function(x) sapply(entitiesUnique, FUN=function (y) rbind(y,x)))
comboUnique<-matrix(comboUnique,nrow = 2)
comboUnique<-as.data.frame(comboUnique)
comboUnique<-t(comboUnique)
comboUnique<-as.data.frame(comboUnique)
colnames(comboUnique)<-c("entityId", "groupId" )

comboGroup<-sapply(groupsUnique, FUN=function(x) sapply(groupsUnique, FUN=function (y) rbind(y,x)))
comboGroup<-matrix(comboGroup,nrow = 2)
comboGroup<-as.data.frame(comboGroup)
comboGroup<-t(comboGroup)
comboGroup<-as.data.frame(comboGroup)
colnames(comboGroup)<-c("group1Id", "group2Id" )
comboGroup<-comboGroup[!(comboGroup$group1Id==comboGroup$group2Id),]
comboGroup<-data.table(comboGroup)
setkey(comboGroup,group1Id)
comboUnique<-data.table(comboUnique)
setkey(comboUnique,groupId)
gsData<-data.table(gsData)
setkey(gsData,groupId)
#rm(groupsUnique, entitiesUnique, gsoData)
#totalCombos<-nrow(comboUnique)
#find match coefficients for all entity/group combos
# for (i in 1: totalCombos ){
#     comboUnique$match[i]<-calcSug(comboUnique[i,1], comboUnique[i,2], gsData)
#     print(paste(i,"of",totalCombos))
# }
comboGroup$res<-apply(comboGroup,1,matchScore)
comboRes<-apply(comboUnique,1,calcSug)
comboRes<-t(comboRes)
comboRes<-as.data.frame(comboRes)
colnames(comboRes)[3]<-"Score"
comboRes<-arrange(comboRes,desc(Score))
View(comboRes)
runTime <- Sys.time()-begTime
runTime
write.csv(comboRes,"testres.csv")

library(plyr)
library(data.table)
#finds the similarity score for two groups
matchScore<-function(group1Id,group2Id) {
    group1<-gsData[groupId==group1Id,]
    group2<-gsData[groupId==group2Id,]
    groupBoth<-c(as.vector(group1$entityId),as.vector(group2$entityId))
    common<-(duplicated(groupBoth)==TRUE)
    common<-length(common[common])
    
    # adjust score to include/exclude length of group 1, etc
    score<-(common/(nrow(group1)+nrow(group2)))
    
    return(score)
    
}

#calculates the match coefficient for an Entity for a specific group
calcSug<-function(comboRow) {
    entityId<-comboRow[1]
    groupId<-comboRow[2]
    group<-gsData[groupId==groupId,]    
 #   contGroups<-as.data.frame((gsData[entityId==entityId,2]))
   
    #if an entity is already in a group, it can't be a valid suggestion
    if (nrow(group[group$entityId==entityId,])>0) #(entityId %in% group[,1]) 
        {
        matchs<-0
        #print("early escape")
        # else find all groups which contain the entity, calculate their match level to the main group 
        # and add that to the entity match coefficient
        } else 
          {
            contGroups<-gsData[entityId==entityId,2]
            matchs<-sum(apply(contGroups,1,matchScore, group1Id=groupId))
           
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
    

gsoData<-read.csv("GroupMembers1.csv")
colnames(gsoData)<-c("favouriteId","userId","entityId","addedDate", "groupId", "isVisible", "entityName", "groupName" )
#get relevant columns only
gsData<-gsoData[,c(3,5)]
entityCount<-apply(gsData,1,countMembers)
gsData1<-gsData[entityCount>1,]
#create a df with all unique Group/Entity ID combos so suggestion coefficient can be calculated for all groups/entites
groupsUnique<-subset(gsData1, !duplicated(gsData[,2]))[,2]
entitiesUnique<-subset(gsData1, !duplicated(gsData[,1]))[,1]
comboUnique<-sapply(groupsUnique, FUN=function(x) sapply(entitiesUnique, FUN=function (y) rbind(y,x)))
rm(groupsUnique, entitiesUnique, gsoData)
comboUnique<-matrix(comboUnique,nrow = 2)
comboUnique<-t(comboUnique)
comboUnique<-as.data.frame(comboUnique)
colnames(comboUnique)<-c("entityId", "groupId" )
comboUnique<-data.table(comboUnique)
setkey(comboUnique,groupId)
gsData<-data.table(gsData)
setkey(gsData,groupId)
#find match coefficients for all entity/group combos
# for (i in 1: totalCombos ){
#     comboUnique$match[i]<-calcSug(comboUnique[i,1], comboUnique[i,2], gsData)
#     print(paste(i,"of",totalCombos))
# }
begTime <- Sys.time()
comboRes<-apply(comboUnique,1,calcSug)
runTimeOld <- Sys.time()-begTime
comboRes<-t(comboRes)
comboRes<-as.data.frame(comboRes)
colnames(comboRes)[3]<-"Score"
comboRes<-arrange(comboRes,desc(Score))
View(comboRes)

runTimeOld

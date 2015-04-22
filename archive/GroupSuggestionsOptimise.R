library(plyr)
#finds the similarity score for two groups
matchScore<-function(group1Id,group2Id) {
    group1<-as.vector(gsData[gsData$groupId==group1Id,1])
    group2<-as.vector(gsData[gsData$groupId==group2Id,1])
    groupBoth<-c(group1, group2)
    common<-(duplicated(groupBoth)==TRUE)
    common<-length(common[common])
    # adjust score here to include/exclude length of group 1, etc
    score<-common/length(groupBoth)
    return(score)
    
}

#calculates the match coefficient for an Entity for a specific group
calcSug<-function(comboRow) {
    entityId<-comboRow[1]
    groupId<-comboRow[2]
    group<-gsData[groupId==groupId,]    
    contGroups<-data.table(gsData[entityId==entityId,2])
    #if an entity is already in a group, it can't be a valid suggestion
     matchs<- ifelse (
         (nrow(group[entityId==entityId,])>0), 
         0, 
         sum(apply(contGroups,1,matchScore, group1Id=groupId))) 
#     {
#         matchs<-0
#         #print("early escape")
#         # else find all groups which contain the entity, calculate their match level to the main group 
#         # and add that to the entity match coefficient
#     } else 
#     {
#         
#         matchs<-0
#         matchs<-sum(apply(contGroups,1,matchScore, group1Id=groupId))
#         
#         #             for (i in 1: length(contGroups) ){
#         #                 groupTemp<-gsData[gsData$groupId==contGroups[i],]
#         #                 match<-match+matchScore(group,groupTemp)  
#         #          #       print("found match")
#         #           }
#         
#     }
#     
    return(matchs)
}



begTime <- Sys.time()
gsoData<-read.csv("GroupMembers.csv")
colnames(gsoData)<-c("favouriteId","userId","entityId","addedDate", "groupId", "isVisible", "entityName", "groupName" )
#get relevant columns only
gsData<-data.table(gsoData[,c(3,5)])
#remove all artistId's which only appear once from result combo as they will not be suggested or add to other suggestions
gsData1<-gsData[duplicated(gsData[,1]),]
#create a df with all unique Group/Entity ID combos so suggestion coefficient can be calculated for all groups/entites
groupsUnique<-subset(gsData1, !duplicated(gsData[,2]))[,2]
entitiesUnique<-subset(gsData1, !duplicated(gsData[,1]))[,1]
comboUnique<-sapply(groupsUnique, FUN=function(x) sapply(entitiesUnique, FUN=function (y) 
    {if {(nrow(gsData[,groupId==x])==0) rbind(y,x)}}))
rm(groupsUnique, entitiesUnique, gsData1)
comboUnique<-matrix(comboUnique,nrow = 2)
comboUnique<-as.data.frame(comboUnique)
comboUnique<-t(comboUnique)
comboUnique<-as.data.frame(comboUnique)
comboUnique<-data.table(comboUnique)
setnames(comboUnique)<-c("entityId", "groupId" )
#totalCombos<-nrow(comboUnique)
#find match coefficients for all entity/group combos
# for (i in 1: totalCombos ){
#     comboUnique$match[i]<-calcSug(comboUnique[i,1], comboUnique[i,2], gsData)
#     print(paste(i,"of",totalCombos))
# }
comboRes<-vector(mode="numeric", length=nrow(combUnique))
comboRes<-apply(comboUnique,1,calcSug)
comboRes<-cbind(comboUnique,comboRes)
setnames(comboRes)[3]<-"Score"
View(comboRes)
runTime <- Sys.time()-begTime

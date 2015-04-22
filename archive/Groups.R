#create unique ID for each relationship
getRel<-function(x) {
  if (x[1]>x[2]) {
    return(paste(as.character(x[1]),as.character(x[2]),as.character(x[7])))
  }
  else {
    return(paste(as.character(x[2]),as.character(x[1]), as.character(x[7])))
  }
}

getRelFinal<-function(x) {
    if (x[1]>x[2]) {
        return(paste(as.character(x[1]),as.character(x[2]),sep=""))
    }
    else {
        return(paste(as.character(x[2]),as.character(x[1]),sep=""))
    }
}

#something about the csv export makes R thing there is an extra column somewhere
#if problems arise delete a few columns after the last filled onw from the .csv
gData<-read.csv("GroupsExport.csv")

#all relationships are doubled from the SQL, removing duplcates now (based on unique ID)

gData$Rel<-apply(gData,1, getRel)
gData<-subset(gData, !duplicated(gData[,10]))
gData$Rel<-as.numeric(apply(gData,1, getRelFinal))

#count apperances of correlations
gData<-arrange( gData, gData$Rel)
Count <- rle( gData$Rel )
gData$RelCount <- Count[[1]][ match( gData$Rel , Count[[2]] ) ]
rm(Count)

#save past data and remove all occurances of the same relationship from different groups
#keep a copy of the duplicates, could be useful for determining the size of a vertice (node)
gDataPast<-gData
gData<-subset(gData, !duplicated(gData[,10]))
gData<-arrange( gData, desc(gData$RelCount))

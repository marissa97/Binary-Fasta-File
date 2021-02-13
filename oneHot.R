library("splus2R")

#faFile<-  "/home/marissa/test.fa"

#######################################
#importing data
import<- function(fastaFile){
data <- data.frame(read.table(file = fastaFile, header = TRUE))
data
}

########################################
#saving chr name
chrBinaryConversion<- function(impFasta){
chrName<- colnames(impFasta)
chrName<- substr(chrName,nchar(chrName),nchar(chrName))
chrList<- list()
chrList<- data.frame(chr=chrName)

chrPos<- list()

i<- 1
while(i<=nrow(data)){
if((grepl("chr", data[i,], fixed = TRUE))== TRUE){
  chrName<- as.character(data[i,])
  chrName<- data.frame(chr=substr(chrName,nchar(chrName),nchar(chrName)))
  chrList<- rbind(chrList,chrName)
  pos <- data.frame(pos=i)
  chrPos<- rbind(chrPos,pos)
}
i<- i+1
}


#per chromosome 
binRes<- list()
i<- 1
k<-1
while(i< nrow(data)){
  if(i %in% chrPos$pos){
    i<-i+1
  }
  else{
    num <- chrPos$pos[chrPos>=i]
    num <- num[1]
    chrBases<- as.character(data[i:(num-1),])
    binData<- oneCode(chrBases,nchar(chrBases))
    binData<- paste( unlist(binData), collapse='')
    chrData<- paste(">chr",chrList$chr[k],sep="")
    res<-list()
    res<- cbind(res,chrData,binData)
    binRes<- rbind(binRes,res)
    i<- i+1 
    k<- k+1
  }
}

chrBases<- as.character(data[i,])
binData<- oneCode(chrBases,nchar(chrBases))
binData<- paste( unlist(binData), collapse='')
chrData<- paste(">chr",chrList$chr[k],sep="")
res<-list()
res<- cbind(res,chrData,binData)
binRes<- rbind(binRes,res)
}

#writing(binRes, "testingHot.binFas")


####################################################################
oneCode<- function(fasta, fastaLength){
  
baseList<- list('a','g','t','c','n')
a<- data.frame(bin=c(0,0,0,1))
g<- data.frame(bin=c(0,0,1,0))
t<- data.frame(bin=c(0,1,0,0))
c<-data.frame(bin=c(1,0,0,0))
n<- data.frame(bin=c(0,0,0,0))

newList<- list()
i<- 1
while (i<= fastaLength) {
  if(lowerCase(substring(fasta,i,i))== baseList[1]){
    newList<- rbind(newList,a)
  }  
  else if(lowerCase(substring(fasta,i,i))== baseList[2]){
    newList<- rbind(newList,g)
  }  
  else if(lowerCase(substring(fasta,i,i))== baseList[3]){
    newList<- rbind(newList,t)
  }  
  else if(lowerCase(substring(fasta,i,i))== baseList[4]){
    newList<- rbind(newList,c)
  }  
  i<- i+1
}
newList<- t(newList)
return(newList[1,])
}

####################################################
writing <- function(binList,bsName){
pathName<- paste("/home/marissa/Documents/",bsName,sep="")
write.table(binList, file= pathName, row.names = FALSE, col.names=FALSE)
print("done")
}

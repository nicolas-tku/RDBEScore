createTableOfRDBESIds<-function(x, hierarchy, addSAseqNums=TRUE)
{

# note: needs developments for different lower hierarchies

# x is RDBESobj
# hierarchy is hierarchy (integer)
# outputs a table with ids for matching 

if(hierarchy==1) CStableNames<- c("DE","SD","VS","FT","FO","SS","SA","FM","BV")
if(hierarchy==5) CStableNames<- c("DE","SD","OS","LE","SS","SA","BV")
if(!hierarchy %in% c(1,5)) stop ("hierarchy not defined")

for (i in 1:(length(CStableNames)-1))
{
id_1<-paste0(CStableNames[i],"id")
id_2<-paste0(CStableNames[i+1],"id")
if (i==1) df_1<-data.frame(x[[CStableNames[i]]][,list(get(id_1))]); colnames(df_1)<-id_1
if((CStableNames[i+1] == "SA" & addSAseqNums == TRUE) | CStableNames[i+1] %in% c("BV")){
							if(CStableNames[i+1]=="SA") {df_2<-data.frame(x[[CStableNames[i+1]]][,list(get(id_1), get(id_2), get("SAseqNum"), get("SAparSequNum"))]); colnames(df_2)<-c(id_1,id_2,"SAseqNum","SAparSequNum")}	
							if(CStableNames[i+1]=="BV") {df_2<-data.frame(x[[CStableNames[i+1]]][,list(get(id_1), get(id_2), get("BVfishId"))]); colnames(df_2)<-c(id_1,id_2,"BVfishId")}		
							} else {
								df_2<-data.frame(x[[CStableNames[i+1]]][,list(get(id_1), get(id_2))]); colnames(df_2)<-c(id_1,id_2)
								}

if (i==1) out<-merge(df_1,df_2, all.x=T) else out<-merge(out, df_2, all.x=T)

#colnames(out)<-c(id_1,id_2)

}
# reorders
if(hierarchy==1) {
				if(addSAseqNums==TRUE){
					out<-out[,c("DEid","SDid","VSid","FTid","FOid","SSid","SAid","FMid","BVid","BVfishId","SAseqNum","SAparSequNum")]
					} else { 
						out<-out[,c("DEid","SDid","VSid","FTid","FOid","SSid","SAid","FMid","BVid","BVfishId")]
						}
				}

if(hierarchy==5) {
				if(addSAseqNums==TRUE){
					out<-out[,c("DEid","SDid","OSid","LEid","SSid","SAid","BVid","BVfishId","SAseqNum","SAparSequNum")] 
					} else { 
						out<-out[,c("DEid","SDid","OSid","LEid","SSid","SAid","BVid","BVfishId")]
						}
				}
out
}

# e.g.,
 ## default adds "SAseqNum","SAparSequNum"
 #head(createTableOfRDBESIds(x = RDBESprepObj, hierarchy = 1))
 ## if addSAseqNums is set to FALSE, "SAseqNum" and "SAparSequNum" are not added to output
 # head(createTableOfRDBESIds(x = RDBESprepObj, hierarchy = 1, addSAseqNums=FALSE))
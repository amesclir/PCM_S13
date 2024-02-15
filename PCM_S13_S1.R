
library(phytools)
library(picante)
comunidades.data<-read.csv("comunidades.csv",row.names=1)
comunidades.data
row.names(comunidades.data) <- gsub(" ", "_", row.names(comunidades.data))
tree <- mytree$scenario.3
class(tree)
tree$node.label<-NULL
library(geiger)

## set colors
colors<-setNames(replicate(ncol(comunidades.data),setNames(c(make.transparent(palette()[4],0.15),palette()[2]),0:1),simplify=FALSE),colnames(comunidades.data))
## make foreground color transparent
par(fg="transparent")
## plot tree and data matrix
plotTree.datamatrix(tree,comunidades.data,yexp=1,header=TRUE,xexp=1.6,fsize=0,colors=colors)
## reset foreground color to black
par(fg="black")
## add legend
legend(x=0,y=4,c("absent","present"),pch=15,col=colors[[1]],bty="n",pt.cex=1.5)


name.check(tree,comunidades.data)
allPd<-pd(t(comunidades.data),tree)
head(allPd)

## set margins
par(mar=c(5.1,4.1,1.1,2.1))
## plot PD as a function of species richness
plot(allPd[,2:1],bty="n",pch=21,bg="gray",cex=1.5,las=1,cex.axis=0.7,cex.lab=0.9,xlab="Species richness",ylab="Phylogenetic diversity")


pd.test<-ses.pd(t(comunidades.data),tree,null.model="taxa.labels")

head(pd.test)
pd.test

phydistmat<-cophenetic(tree)

clustResult_mpd<-ses.mpd(t(comunidades.data),phydistmat,null.model="taxa.labels")
head(clustResult_mpd)

smallmpd<-which(clustResult_mpd$mpd.obs.p < 0.05)
clustResult_mpd[smallmpd,]

bigmpd<-which(clustResult_mpd$mpd.obs.p > 0.95)
clustResult_mpd[bigmpd,]


clustResult_mntd<-ses.mntd(t(comunidades.data),phydistmat,null.model="taxa.labels")
head(clustResult_mntd)
smallmntd<-which(clustResult_mntd$mntd.obs.p < 0.05)
clustResult_mntd[smallmntd,]


D.comdist<-comdist(t(comunidades.data),phydistmat)

## create a community dendrogram
community_dendro<-hclust(D.comdist)
## remove underscore character from site names
community_dendro$labels<-gsub("_"," ",community_dendro$labels)
## graph community dendrogram
plot(community_dendro,font=3,cex=0.5,main="",xlab="")

# R version 4.0.2 (2020-06-22)
# "Similarity & Stability of connectivity profile" script (March. 25, 2021) for manuscript:

# Title: "Similarity and stability of face network across populations and throughout adolescence and adulthood"
# Journal: Nature Human Behaviour 
# Authors:Zhijie Liao et al.
# R script author: Zhijie Liao

# Edgelist data frame contains the following columns:
#     1) edge.n: names of edges, e.g., AmygdalaL-AmygdalaR
#     2) edge: strength of edge
# subjectlist is a list of participants' IDs

library(psych)
library(tidyverse)

## individual-canonical profile similarity 
#function to calculate individual-canonical profile similarity
similarity<-function(subjectlist, condition){
  #condition =("angry","ambiguous", "visit1-angry","visit1-ambiguous","visit2-angry","visit2-ambiguous","visit3-angry","visit3-ambiguous")
  inpath1<-paste("/",subjectlist[1],"_",condition,"_edgelist.csv",sep = "")#path to edgelist directory
  all.edge<-read.csv(inpath1, row.names = 1)
  for (j in subjectlist[2:length(subjectlist)]) {#read and combine all participants' edgelist
    inpath<-paste("./",j,"_",condition,"_edgelist.csv",sep = "")
    df<-read.csv(inpath, row.names = 1)
    all.edge<-left_join(all.edge, df, by="edge.n")# combine all participants' edgelist by matching names of edges
  }
  colnames(all.edge)<-c("edge.n", subjectlist)# rename dataframe, one colume represent edgelist of one participant
  all.edge$mean.edge<-rowMeans(all.edge[,2:ncol(all.edge)])# calculate the group-averaged profile, namely canonical profile
  group.cor<-cor(column_to_rownames(all.edge, "edge.n"), use = "p")%>%as.data.frame()%>%
    rownames_to_column("sub_id")%>%select(sub_id, mean.edge)%>%
    filter(sub_id!="mean.edge")# calculate similarity of individual-canonical profile, i.e., Pearson correlation between individual profile and group-averaged profile
  group.cor
}

#calculate individual-canonical similarity for each population and each visit and each face condition
#ALSPAC
alspac.similarity.angry<-similarity(alsp.subjectlist, "angry")
alspac.similarity.ambiguous<-similarity(alsp.subjectlist, "ambiguous")
#IMAGEN Visit 1
imagen.similarity.angry.visit1<-similarity(imagen.subjectlist,  "visit1-angry")
imagen.similarity.ambiguous.visit1<-similarity(imagen.subjectlist, "visit1-ambiguous")
#IMAGEN Visit 2
imagen.similarity.angry.visit2<-similarity(imagen.subjectlist,  "visit2-angry")
imagen.similarity.ambiguous.visit2<-similarity(imagen.subjectlist, "visit2-ambiguous")
#IMAGEN Visit 3
imagen.similarity.angry.visit3<-similarity(imagen.subjectlist,  "visit3-angry")
imagen.similarity.ambiguous.visit3<-similarity(imagen.subjectlist, "visit3-ambiguous")

#overall individual-canonical similarity for IMAGEN

mean.similarity<-function(visit1,visit2,visit3){
  df<-inner_join(visit1,visit2, by="sub_id")%>%inner_join(., visit3, by="sub_id")
  colnames(df)<-c("sub_id","visit1","visit2","visit3")
  df<-df%>%mutate(similarity=(visit1+visit2+visit3)/3)
  df<-select(df, sub_id, similarity)
  df
}
imagen.similarity.angry<-mean.similarity(imagen.similarity.angry.visit1, imagen.similarity.angry.visit2, imagen.similarity.angry.visit3)
imagen.similarity.ambiguous<-mean.similarity(imagen.similarity.ambiguous.visit1, imagen.similarity.ambiguous.visit2, imagen.similarity.ambiguous.visit3)


## stability of individual connectivity profile -IMAGEN only
get.stability<-function(subjectlist, condition1, condition2){
  #condition1 =("visit1-angry","visit1-ambiguous","visit2-angry","visit2-ambiguous","visit3-angry","visit3-ambiguous")
  #condition2 =("visit1-angry","visit1-ambiguous","visit2-angry","visit2-ambiguous","visit3-angry","visit3-ambiguous")
  stability<-NULL
  for (j in subjectlist) {
    pathname1=paste("./",j,"_",condition1,"_edgelist.csv",sep = "")#path of edgelist from one visit
    pathname2=paste("./",j,"_",condition2,"_edgelist.csv",sep = "")#path of edgelist from another visit
    edglist1<-read.csv(pathname1)
    edglist2<-read.csv(pathname2)
    edglist<-inner_join(edglist1,edglist2, by="edge.n")#combine edgelists from two visits by matching names of edges
    colnames(edglist)<-c("edge.n", "edgelist1","edgelist2")
    stab<-cor(edglist$edgelist1, edglist$edgelist2, method = "pearson")#calculate stability (i.e., Pearson correlation) of individual profile from one visit to another
    stability<-c(stability, stab)
  }
  stability.df<-data.frame(sub_id=subjectlist, stability=stability)
  stability.df
}

#stability of visit1 to visit 2
imagen.stability.angry.v1_v2<-get.stability(imagen.subjectlist,  condition1 = "visit1-angry", condition2 = "visit2-angry")
imagen.stability.ambiguous.v1_v2<-get.stability(imagen.subjectlist,  condition1 = "visit1-ambiguous", condition2 = "visit2-ambiguous")

#stability of visit2 to visit 3
imagen.stability.angry.v2_v3<-get.stability(imagen.subjectlist,  condition1 = "visit2-angry", condition2 = "visit3-angry")
imagen.stability.ambiguous.v2_v3<-get.stability(imagen.subjectlist,  condition1 = "visit2-ambiguous", condition2 = "visit3-ambiguous")

#stability of visit1 to visit 3
imagen.stability.angry.v1_v3<-get.stability(imagen.subjectlist,  condition1 = "visit1-angry", condition2 = "visit3-angry")
imagen.stability.ambiguous.v1_v3<-get.stability(imagen.subjectlist,  condition1 = "visit1-ambiguous", condition2 = "visit3-ambiguous")

#overall stability of individual profile from visit1 to visit2 to visit3
mean.stability<-function(v1_v2, v2_v3, v1_v3){
  df<-inner_join(v1_v2,v2_v3, by="sub_id")%>%inner_join(., v1_v3, by="sub_id")
  colnames(df)<-c("sub_id","v1_v2", "v2_v3", "v1_v3")
  df<-df%>%mutate(stability=(v1_v2+v2_v3+v1_v3)/3)
  df<-select(df, sub_id, stability)
  df
}

imagen.stability.angry<-mean.stability(imagen.stability.angry.v1_v2, imagen.stability.angry.v2_v3, imagen.stability.angry.v1_v3)
imagen.stability.ambiguous<-mean.stability(imagen.stability.ambiguous.v1_v2, imagen.stability.ambiguous.v2_v3, imagen.stability.ambiguous.v1_v3)



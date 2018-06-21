# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

## check packages in local directory
check.packages <- function(x){
  path<- paste(getwd(),"/library",sep="")
  myPaths<- .libPaths()
  myPaths<- c(myPaths, path)

  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( !require( i , character.only = TRUE, lib.loc = myPaths[2] ) ){
      j<- paste("rlib/",i,".tar.gz",sep="")
      #  If package was not able to be loaded then re-install
      install.packages( j , dependencies = TRUE , repos = NULL,type="source", lib = myPaths[2])
      #  Load package after installing
      require( i , character.only = TRUE, lib.loc = myPaths[2] )
    }
  }
}


## Adverse event summary

mySummary.ae <- function(ae,arm=as.factor(ae$arm),var=ae$AE,SUBJID="SUBJID", name="something"){
  require(tidyverse)
  require(flextable)
  ae.name<- sort(unique(var))
  ae.name<- c("Any events",ae.name)

  gr.lev <- levels(arm)
  n.1 <- sum(arm==gr.lev[1])
  n.2 <- sum(arm==gr.lev[2])

  ae.count<- data.frame("ae.name"=ae.name,"Group1.n.pt"=rep(NA,length(ae.name)),"Group1.n.e"=rep(NA,length(ae.name)),
                        "Group2.n.pt"=rep(NA,length(ae.name)),"Group2.n.e"=rep(NA,length(ae.name)), stringsAsFactors = FALSE)

  ae.count$Group1.n.pt[1]<- ae %>% filter(arm==gr.lev[1] & var %in% ae.name) %>% select(SUBJID) %>% n_distinct()
  ae.count$Group1.n.e[1]<- ae %>% filter(arm==gr.lev[1] & var %in% ae.name) %>% select(SUBJID) %>% nrow()
  ae.count$Group2.n.pt[1]<- ae %>% filter(arm==gr.lev[2] & var %in% ae.name) %>% select(SUBJID) %>% n_distinct()
  ae.count$Group2.n.e[1]<- ae %>% filter(arm==gr.lev[2] & var %in% ae.name) %>% select(SUBJID) %>% nrow()

  for (i in 2:length(ae.name)) {
    ae.count$Group1.n.pt[i]<- ae %>% filter(arm==gr.lev[1]& var==ae.name[i]) %>% select(SUBJID) %>% n_distinct()
    ae.count$Group1.n.e[i]<- ae %>% filter(arm==gr.lev[1]& var==ae.name[i]) %>% select(SUBJID) %>% nrow()
    ae.count$Group2.n.pt[i]<- ae %>% filter(arm==gr.lev[2]& var==ae.name[i]) %>% select(SUBJID) %>% n_distinct()
    ae.count$Group2.n.e[i]<- ae %>% filter(arm==gr.lev[2]& var==ae.name[i]) %>% select(SUBJID) %>% nrow()
  }

  for (i in 1:length(ae.name)){
    if(ae.count$Group1.n.pt[i]+ae.count$Group2.n.pt[i]>0) {
      ae.count$p.value[i] <- format.pval(fisher.test(cbind(c(ae.count$Group1.n.pt[i],n.1-ae.count$Group1.n.pt[i]),
                                                           c(ae.count$Group2.n.pt[i],n.2-ae.count$Group2.n.pt[i])))$p.value,digits=3)
    } else NA
  }
  ae.count$Group1.n.pt <- paste(ae.count$Group1.n.pt," (",round(100*ae.count$Group1.n.pt/n.1,2),"%)",sep="")
  ae.count$Group2.n.pt <- paste(ae.count$Group2.n.pt," (",round(100*ae.count$Group2.n.pt/n.2,2),"%)",sep="")

  rownames(ae.count)<-NULL

  ae.count


  ae.table<- regulartable(ae.count)

  # text alignment
  ae.table<- align(ae.table, align= "center", part= "all")

  # add header
  ae.table<- set_header_labels(ae.table,ae.name= name,Group1.n.pt= paste(gr.lev[1],"(n=",n.1,")",sep=""), Group1.n.e=paste(gr.lev[1],"(n=",n.1,")",sep=""),Group2.n.pt=paste(gr.lev[2],"(n=",n.2,")",sep=""), Group2.n.e=paste(gr.lev[2],"(n=",n.2,")",sep="") )

  ae.table<- add_header(ae.table,ae.name = name,Group1.n.pt="n.pt", Group1.n.e="n.ae",Group2.n.pt="n.pt", Group2.n.e="n.ae",p.value="p.value", top=FALSE)
  

  ## merge identical cells
  ae.table<- merge_h(ae.table, part="header")
  ae.table<- merge_v(ae.table, part="header")
  ae.table<- add_footer(ae.table,ae.name = "n.pt is the number of patient with at least one event; n.e is the total number of total events",top=FALSE)
  ae.table<- merge_at(ae.table,j=1:5,part="footer")
  ae.table
}


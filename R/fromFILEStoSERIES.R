#' @title fromFILEStoSERIES function
#' @description This function helps to organize the Belgium data by adding extra "Series" row and reformatting data in a proper format.
#' @param dat data frame
#' @param series_title  variable name "Series title"
#' @param files variable name for "Files"
#' @param series_scope_note variable name for "Series scope note"
#' @param series_date_range variable name for "Hoover date range"
#' @param scope_and_content variable name for "Scope and content"
#' @param problems_notes variable name for "Problems/Notes"
#' @param box_barcode variable name for "Box Barcode"
#' @param top_container variable name for "Top container"
#' @param ... other parameters
#' @export
#' @importFrom stringdist stringdist
#' @import dplyr
#' @import readxl
#' @import xlsx
#' @return Returns altered dataframe.
#' @examples
#' library(HooverArchives)
#' library(readxl)
#' library(xlsx)
#'
#' #Load data and create indices
#' dat2.1<-read.xlsx(system.file("belgiumdata.xlsx", package="HooverArchives"), sheetIndex=1, header=FALSE, encoding = "utf-8")
#' dat2.1[]<-lapply(dat2.1, as.character)
#' colnames(dat2.1)<-as.character(dat2.1[3,])
#' dat2.1<-dat2.1[-(1:3),-c(1,14)];
#' dat2.1$indexW<-dat2.1$`Item title`
#'
#' dat2.2<-read.xlsx(system.file("belgiumdata.xlsx", package="HooverArchives"), sheetIndex=2, header=TRUE, encoding = "utf-8")
#' dat2.2$indexW<- dat2.2$`Packet.Catalog.Title`
#'
#' #Merge two dataframe using BuildIndex and Merge_data functions
#' index_matches<-buildIndex(dat2.1$indexW,dat2.2$indexW,
#'                           index_simplify=TRUE,
#'                           fuzzy_matching=TRUE,
#'                           index_hashing=FALSE)
#' mdat<-mergeData(dat2.1,dat2.2, index_matches)
#'
#' #Use fromFILEStoSERIES() to add the Series row
#' coverted.dat<-fromFILEStoSERIES(dat=mdat,
#'                                 series_title="Series title",
#'                                 files="index",
#'                                 series_scope_note="Series scope note",
#'                                 series_date_range="Hoover date range",
#'                                 scope_and_content="Scope.and.content",
#'                                 problems_notes="Series scope note",
#'                                 box_barcode="Box_Barcode",
#'                                 top_container="Final.Box..")
#'
#' datHarvard<-subset(coverted.dat, select=c("Group", "Title", "Hierarchical_Relationship",	"Processing_Information",
#'                                            "Description_Level",	"Date", "Top_Container_[indicator]", "Box_Barcode",
#'                                            "Scope_and_content"))
#' #Save file in xlsx to preserve diacritic characters
#' #write.xlsx(datHarvard, "convertedtoHarvardStyle.xlsx", sheetName = "HarvardStyle", col.names = TRUE)


fromFILEStoSERIES<-function(dat=NULL,
                            series_title=NULL,
                            files=NULL,
                            series_scope_note=NULL,
                            series_date_range=NULL,
                            scope_and_content=NULL,
                            problems_notes=NULL,
                            box_barcode=NULL,
                            top_container=NULL,
                            ...){

  dat[]<-lapply(dat, as.character)
  cNames <- colnames(dat)
  series_titleV <- dat[,cNames%in%series_title]
  series_scope_noteV <- dat[,cNames%in%series_scope_note]
  series_date_rangeV <- dat[,cNames%in%series_date_range]
  filesV <- dat[,cNames%in%files]

  k=1
  TitleS <-	paste(series_titleV, '. (', series_scope_noteV, ", ", series_date_rangeV, ")", sep="")
  dat$TitleF <-	filesV
  dat$Group <- NA
  dat$Title <- NA
  dat$'Hierarchical Relationship' <- NA
  dat$'Description Level' <- NA
  superdat <- data.frame(matrix(NA, dim(dat)[1]*2, dim(dat)[2]))
  colnames(superdat)<-colnames(dat)

  #reordering
  index_order<-paste(series_titleV, dat$TitleF, series_date_rangeV, sep=":")
  dat<-dat[order(index_order),]

  gr=1
  for (i in 1:length(unique(dat$TitleF))){
    repObs<-sum(dat$TitleF%in%unique(dat$TitleF)[i])
    if(repObs>1){
      superdat[k,] <- NA
      superdat$Title[k]<-TitleS[which(dat$TitleF%in%unique(dat$TitleF)[i])[1]];
      superdat$'Hierarchical Relationship'[k]<-1
      superdat$'Description Level'[k]<-"Series"
      superdat[(k+1):(k+repObs),] <- dat[which(dat$TitleF%in%unique(dat$TitleF)[i]),];
      superdat[(k+1):(k+repObs),'Title'] <- dat[dat$TitleF%in%unique(dat$TitleF)[i],'TitleF'];
      superdat[(k+1):(k+repObs),'Hierarchical Relationship'] <- 2
      superdat[(k+1):(k+repObs),'Description Level'] <- "File"
      superdat[k:(k+repObs), 'Group']<-gr
      k=k+repObs+1;
    }else{
      superdat[k,] <- NA
      superdat$Title[k]<-TitleS[which(dat$TitleF%in%unique(dat$TitleF)[i])[1]];
      superdat$'Hierarchical Relationship'[k]<-1
      superdat$'Description Level'[k]<-"Series"
      superdat[(k+1):(k+2),] <- dat[which(dat$TitleF%in%unique(dat$TitleF)[i]),];
      superdat[(k+1):(k+2),'Title'] <- dat[dat$TitleF%in%unique(dat$TitleF)[i],'TitleF'];
      superdat[(k+1):(k+2),'Hierarchical Relationship'] <- 2
      superdat[(k+1):(k+2),'Description Level'] <- "File"
      superdat[k:(k+2), 'Group']<-gr
      k=k+2
    }
    gr=gr+1
  }

  colnames(superdat)<-gsub(" |\\.", "_", colnames(dat))

  if(!is.null(top_container)){
  superdat$"Top_Container_[indicator]"<- superdat[,names(superdat)%in%gsub("\\.", "_", top_container)]
  }

  superdat <- superdat[!is.na(superdat$Group),]
  superdat <- superdat %>% arrange(Group, Hierarchical_Relationship, Title, groupby=Group)

  #g	The scope and contents notes CHANGES:
  if(!is.null(scope_and_content)){
  superdat[,gsub("\\.|\\s+", "_", scope_and_content)]<-
    gsub("many numbers missing", "many issues missing", superdat[,gsub("\\.", "_", scope_and_content)])
  }

  #h
  if(!is.null(box_barcode) & !is.null(problems_notes)){
    superdat$Processing_Information<-
      paste(superdat[,gsub("\\.|\\s+", "_", box_barcode)], superdat[,gsub("\\.|\\s+", "_", problems_notes)], sep="; ")
    superdat$Processing_Information<-gsub("(^; )|(; $)", "", superdat$Processing_Information)
    superdat$Processing_Information<-gsub("NA;*\\s*NA", "", superdat$Processing_Information)}

  if(!is.null(scope_and_content)){
  superdat[,gsub("\\.|\\s+", "_", scope_and_content)]<-
    gsub("\\s+[Xx]\\s+", "", superdat[,gsub("\\.", "_", scope_and_content)])
  }

  #j
  #concatenation
  superdat$Title[superdat$Hierarchical_Relationship==1] <- paste(superdat$Title[superdat$Hierarchical_Relationship==1],
                                                                 "This is being done for larger countries (countries with lots of titles).", sep=" ")
  superdat[is.na(superdat)] <- ""

  for (name in colnames(superdat[,sapply(superdat, is.character)])){
    Encoding(superdat[[name]]) <- "UTF-8"}

return(superdat)}

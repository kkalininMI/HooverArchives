#' @title mergeData function
#' @description This function helps to index data files or separate spreadsheets for subsequent merging.
#' @param dat1 dataset 1
#' @param dat2 dataset 2
#' @param indexed_data list of indices from buildIndex() function
#' @export
#' @return Returns merged dataframe.
#' @examples
#'
#' library(HooverArchives)
#' library(readxl)
#' library(xlsx)
#'
#' #Load data and create indices
#'
#' #Open Sheet 1
#' dat2.1<-read.xlsx(system.file("BelgiumData.xlsx", package="HooverArchives"), sheetIndex=1, header=FALSE, encoding = "utf-8")
#' dat2.1[]<-lapply(dat2.1, as.character)
#' colnames(dat2.1)<-as.character(dat2.1[3,])
#' dat2.1<-dat2.1[-(1:3),-c(1,14)];
#' dat2.1$indexW<-dat2.1$`Item title`
#'
#' #Open Sheet 2
#' dat2.2<-read.xlsx(system.file("BelgiumData.xlsx", package="HooverArchives"), sheetIndex=2, header=TRUE, encoding = "utf-8")
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
#'                                series_title="Series title",
#'                                files="index",
#'                                series_scope_note="Series scope note",
#'                                series_date_range="Hoover date range",
#'                                scope_and_content="Scope.and.content",
#'                                problems_notes="Series scope note",
#'                                box_barcode="Box_Barcode",
#'                                top_container="Final.Box..")
#' coverted.dat$Date<-dateReformatter(coverted.dat$Date)
#' convertedtoArchivesSpace<-subset(coverted.dat, select=c("Title", "Hierarchical_Relationship",	"Processing_Information",
#'                                           "Ckey_x", "Ckey_y", "Description_Level",	"Date", "Top_Container_[indicator]",
#'                                           "Box_Barcode", "Scope_and_content"), value=TRUE)
#' #Save file in xlsx to preserve diacritic characters
#' #write.xlsx(convertedtoArchivesSpace, "convertedtoArchivesSpace.xlsx", sheetName = "ArchivesSpace", col.names = TRUE)


mergeData <-function(dat1=NULL, dat2=NULL, indexed_data=NULL){
  dat1$index <- indexed_data[[1]]
  dat2$index <- indexed_data[[2]]
  merged_data <- merge(dat1, dat2, by="index", all=TRUE)
  merged_data$index <- gsub("^\\s+", "", merged_data$index)
  merged_data <- merged_data[!is.na(merged_data$index),]
  return(merged_data)}

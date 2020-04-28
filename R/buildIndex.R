#' @title buildIndex function
#' @description This function helps to index data files or separate spreadsheets for subsequent merging.
#' @param x index 1
#' @param y index 2
#' @param index_simplify parameter eliminating major inconsistencies between indices (FALSE by default)
#' @param index_hashing converting index to hash index (FALSE by default)
#' @param fuzzy_matching enable fuzzy matching between the indices (FALSE by default). If TRUE, the window pops up: enter 0 in the third column for correctly fuzzy matched pair, and 1 otherwise. If all pairs are correctly matched, close the window without entering any information into the third column.
#' @param ... other parameters
#' @export
#' @importFrom stringdist stringdist
#' @importFrom utils edit
#' @import dplyr
#' @import digest
#' @return Returns the list of "matched" indices for both data frames.
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


#Matching function for two dataframes
buildIndex<-function(x=NULL,y=NULL, index_simplify=FALSE, index_hashing=FALSE, fuzzy_matching=FALSE, ...){

  Index_conv_function <- function(var){
    s1 <- paste(gsub(",.*","", var), ")", sep="");
    s2 <- gsub("\\)+",")", s1)
    s3 <- gsub("\\.|\\,", "", s2)
    s3[!grepl("\\(", s3)] <- gsub("\\)", "", s3[!grepl("\\(", s3)])
    s4<-gsub("([a-z])(\\()", "\\1 \\2",  s3)
    return(s4)}

  Hindex_conv_function <- function(var){
    s <- unlist(lapply(var, digest))
    return(s)
  }

  Index_fuzzy_function <- function(a,b,x,y){
    a<-unique(as.character(a));
    b<-unique(as.character(b));
    minInd <- sapply(a, function(x, y = b){
      min_index <- which.min(stringdist(x, y, method = "jw"))
      return(y[min_index])})
    df <- data.frame(minInd);
    a <- rownames(df); b <- as.character(df[,1])
    df <- data.frame(a,b, stringsAsFactors = FALSE)
    fuzzy_decision <- edit(df)
    if(dim(fuzzy_decision)[2]==2){fuzzy_decision$decision<-NA}

    for(i in 1:length(fuzzy_decision$b)){
      if (is.na(fuzzy_decision[i,3])|fuzzy_decision[i,3]==0){
        y[y%in%fuzzy_decision$b[i]] <- fuzzy_decision$a[i]}else{
          y[y%in%fuzzy_decision$b[i]] <- "NOT MATCHED B"
        }
    }
    return(y)}

  if(index_simplify==TRUE){
    x <- Index_conv_function(x);
    y <- Index_conv_function(y);
  }

  if(fuzzy_matching==TRUE){
    a<-x[!x%in%y]
    b<-y[!y%in%x]
    fuzzy_data <- Index_fuzzy_function(a,b,x,y)
    fuzzy_data[!(fuzzy_data%in%x) & fuzzy_data!="NOT MATCHED B"]  <- "NOT MATCHED A"
    y=fuzzy_data
  }

  if(index_hashing==TRUE){
    x <- Hindex_conv_function(x);
    y <- Hindex_conv_function(y);
  }

  result<-list(x,y)
  return(result)}

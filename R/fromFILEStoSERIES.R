#' @title fromFILEStoSERIES function
#' @description This function helps to organize the Belgium data by adding extra "Series" row and reformatting data in a proper format.
#' @param dat data frame
#' @param series_title  variable name "Series title"
#' @param files variable name for "Files"
#' @param series_scope_note variable name for "Series scope note"
#' @param series_date_range variable name for "Hoover date range"
#' @param scope_and_content variable name for "Scope and content"
#' @param box_barcode variable name for "Box Barcode"
#' @param ckey variable name for "Ckey"
#' @param top_container variable name for "Top container"
#' @param processing_information variable name for "Processing information"
#' @param lang_encoding sets system's locale to specific language (by default "English")
#' @param add_articles vector of extra articles to be excluded from alphabetical ordering
#' @param remove_special_characters remove special characters, i.e. horizontal brackets (TRUE by default)
#' @param alphabetizewithinbrackets alphabetize by a string within square brackets (FALSE by default)
#' @param diacriticslatinization transform characters with diacritics to Latin for alphabetization (TRUE by default)
#' @param USextension file name change to reflect specifics of US data
#' @param ... other parameters
#' @export
#' @importFrom stringdist stringdist
#' @import dplyr
#' @import readxl
#' @import xlsx
#' @import stringi
#' @return Returns altered dataframe.
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
#'                                ckey="Ckey.x",
#'                                top_container="Final.Box..")
#' coverted.dat$Date<-dateReformatter(coverted.dat$Date)
#' convertedtoArchivesSpace<-subset(coverted.dat, select=c("Title", "Hierarchical_Relationship",	"Processing_Information",
#'                                           "CkeyV", "Description_Level",	"Date", "Top_Container_[indicator]",
#'                                           "Box_Barcode", "Scope_and_content"), value=TRUE)
#' #Save file in xlsx to preserve diacritic characters
#' #write.xlsx(convertedtoArchivesSpace, "convertedtoArchivesSpace.xlsx", sheetName = "ArchivesSpace", col.names = TRUE)



fromFILEStoSERIES<-function(dat=NULL,
                            series_title=NULL,
                            files=NULL,
                            series_scope_note=NULL,
                            series_date_range=NULL,
                            scope_and_content=NULL,
                            box_barcode=NULL,
                            top_container=NULL,
                            processing_information=NULL,
                            ckey=NULL,
                            lang_encoding="English",
                            add_articles=NULL,
                            remove_special_characters=TRUE,
                            alphabetizewithinbrackets=FALSE,
                            diacriticslatinization=TRUE,
                            USextension=FALSE,
                            ...){

  dat[] <- lapply(dat, as.character)

  Sys.setlocale('LC_ALL', lang_encoding)

  if(isTRUE(remove_special_characters)){
    remove_sc = "\U00fe20|\U00fe21|\U00361"
    remove_sc<-paste(remove_special_characters, remove_sc, collapse="|")
    remove_sc<-gsub("^\\s+","", remove_sc)
    dat <- as.data.frame(apply(dat, 2, function(x) gsub(remove_sc, "", x, perl=TRUE)), stringsAsFactors = FALSE)
    }

  dat[] <- lapply(dat, as.character)

  cNames <- colnames(dat)
  series_titleV <- dat[,cNames%in%series_title]
  series_scope_noteV <- dat[,cNames%in%series_scope_note]
  series_date_rangeV <- dat[,cNames%in%series_date_range]
  processing_informationV <- dat[,cNames%in%processing_information]
  CkeyV <- dat[,cNames%in%ckey]
  filesV <- dat[,cNames%in%files]

  #browser()
  k=1
  series_titleV<-gsub("^\\s+|\\s+$", "", series_titleV)
  series_scope_noteV<-gsub("^\\s+|\\s+$", "", series_scope_noteV)
  series_titleV<-gsub("^\\s+|\\s+$", "", series_titleV)
  TitleS <-	paste(series_titleV, '. (', series_scope_noteV, ", ", series_date_rangeV, ")", sep="")
  TitleS <-	gsub("\\(+|\\(+\\s*\\(+", "(", TitleS); TitleS <-	gsub("\\)+|\\)+\\s*\\)+", ")", TitleS)
  dat$TitleF <-	filesV
  dat$Group <- NA
  dat$Title <- NA
  dat$'Hierarchical Relationship' <- NA
  dat$'Description Level' <- NA
  dat$CkeyV<-NA

  superdat <- data.frame(matrix(NA, dim(dat)[1]*2, dim(dat)[2]))
  colnames(superdat)<-colnames(dat)

  gr=1
  for (i in 1:length(unique(dat$TitleF))){
    repObs<-sum(dat$TitleF%in%unique(dat$TitleF)[i])
    if(repObs>1){
      superdat[k,] <- NA
      superdat$Title[k]<-TitleS[which(dat$TitleF%in%unique(dat$TitleF)[i])[1]];
      superdat$CkeyV[k]<-CkeyV[which(dat$TitleF%in%unique(dat$TitleF)[i])[1]];
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
      superdat$CkeyV[k]<-CkeyV[which(dat$TitleF%in%unique(dat$TitleF)[i])[1]];
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
  #browser()

  colnames(superdat)<-gsub(" |\\.", "_", colnames(dat))

  if(!is.null(top_container)){
    superdat$"Top_Container_[indicator]"<- superdat[,names(superdat)%in%gsub("\\.", "_", top_container)]
  }

  superdat <- superdat[!is.na(superdat$Group),]

  if(USextension){
    superdat$Title[superdat$Description_Level!="Series"]<-paste(gsub(":", "",
                                                                     gsub("\\s+(?=:)", "", gsub("(?<=:).+", "", superdat$Title[superdat$Description_Level!="Series"], perl=TRUE), perl=TRUE)), ")", sep="")
  }


  #reordering
  remove_articles<-function(x){
    Caps <- function(s) {
      unique(c(s,
               paste(toupper(substring(s, 1,1)), substring(s, 2), sep=""),
               paste(toupper(s), sep="")))
    }
    articles<-Caps(c("l'", "l", "le", "la", "les", "un", "une", "des", "du", "de", "la", "der", "die", "das", "ein", "eine", "het", add_articles))
    x[is.na(x)] <- "NA"
    first <- regmatches(x, regexpr("(\\w+)", x))
    x[first %in% articles]<-gsub("(^\\w+\\s+)|([[:alpha:]])'", "", x[first%in%articles])
    x <- gsub("\\s*\\([^\\)]+\\)","", x)
    x <- gsub("^\\s+", "", x)
    return(x)}

  if(alphabetizewithinbrackets){
    rem_nat_char_Title <- gsub("[[:punct:]]| ", "", gsub("^.+(?=[\\[\\{])", "", remove_articles(superdat$Title), perl=TRUE))
    superdat$indexN <- remove_articles(rem_nat_char_Title)
  }else{
    superdat$indexN <- remove_articles(superdat$Title)
    superdat$indexN <- gsub("\u00B4", "", superdat$indexN)
    if(diacriticslatinization){
      Encoding(superdat[["indexN"]]) <- "UTF-8"
      superdat$indexN <- stri_trans_general(superdat$indexN,"Latin-ASCII")
    }
  }

  index_frame<-superdat%>%select(Hierarchical_Relationship, Group, indexN)%>%
    filter(Hierarchical_Relationship==1)%>%
    arrange(indexN)%>%
    mutate(GroupN = 1:n())

  superdat$GroupN<-sapply(superdat$Group, function(x)  index_frame$GroupN[index_frame$Group%in%x])
  superdat <- arrange(superdat, GroupN, Hierarchical_Relationship, indexN, Date)

  #g	The scope and contents notes CHANGES:
  if(!is.null(scope_and_content)){
    superdat[,gsub("\\.|\\s+", "_", scope_and_content)]<-
      gsub("many numbers missing", "many issues missing", superdat[,gsub("\\.", "_", scope_and_content)])
  }


  if(!is.null(scope_and_content)){
    superdat[,gsub("\\.|\\s+", "_", scope_and_content)]<-
      gsub("\\s+[Xx]\\s+", "", superdat[,gsub("\\.", "_", scope_and_content)])
  }

  superdat[is.na(superdat)] <- ""

  for (name in colnames(superdat[,sapply(superdat, is.character)])){
    Encoding(superdat[[name]]) <- "UTF-8"}

  #remove rows with NAs in Title
  superdat <- superdat[!grepl("(^NA\\.*$)|(^NA\\.)", superdat$Title),]



  superdat$Title <-	gsub("\\(+|\\(+\\s*\\(+", "(", superdat$Title)
  superdat$Title <-	gsub("\\)+|\\)+\\s*\\)+", ")", superdat$Title)


  return(superdat)}

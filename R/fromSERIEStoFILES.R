#' @title fromSERIEStoFILES function
#' @description This function helps to obtain "Files" variable by splitting the issueDates into separate years
#' @param dat original dataframe.
#' @param issueDates Variables containing information
#' @param locale  system's locale [locale = English]
#' @export
#' @importFrom stats setNames
#' @return Returns the dataframe with the dataframe.
#' @examples
#' library(HooverArchives)
#'
#' item_nodes<-list(path="(//c)|(//c01)|(//c02)|(//c03)|(//c04)|(//c05)|(//c06)|(//c07)|(//c08)",
#'                  nodes=c("primarynode", ".//unittitle", "./did//unitdate", "./did//unitdate",
#'                          "./did//language", "./did//abstract", ".//container",
#'                          "./did//container", "./scopecontent[@id]", ".//scopecontent/head",
#'                          ".//scopecontent/p", "./accessrestrict[@id]",
#'                          ".//accessrestrict/head", ".//accessrestrict/p", ".//note"),
#'                  types=c("attrs", "text", "text", "attrs", "attrs", "text", "text", "attrs", "attrs",
#'                          "text", "text", "attrs", "text", "text", "text"))
#'
#' collection_nodes<-list(path="archdesc[@level='collection']",
#'                  nodes=c(".//unittitle", "./did//unitdate", "./did//language", "./did//abstract"),
#'                  types=c("text", "text", "attrs", "text"))
#'
#'file_transf<-fromXMLtoCSV(system.file("rusnewspapers.xml", package="HooverArchives"), item_nodes, collection_nodes)
#'
#'convdata<-fromSERIEStoFILES(file_transf, issueDates="note.text", locale="Russian")
#'
#'#write.csv(convdata, "convdata_2012C30.csv")



fromSERIEStoFILES <- function(dat=NULL, issueDates=NULL, locale="English"){

  Sys.setlocale("LC_ALL",locale = locale)

  issueDates_extractor <- function(y){
    if(is.na(y)|!grepl("\\d+", y)) return (issueDates=NA)
    regex <- "(\\d{2,4})\\?*(?=\\:)"
    years <- unlist(regmatches(y, gregexpr(regex,y,perl=TRUE)))

    if(length(years)==0){
      regex <- "^(\\d{2,4}\\?*(?=\\:*))"
      years <- unlist(regmatches(y, gregexpr(regex,y,perl=TRUE)))
    }

    if(any(nchar(years)<3)){
      years<-unlist(regmatches(y, gregexpr("\\d{2,4}\\/\\d{2,4}(?=\\:)",y,perl=TRUE)))}

    nums <- unlist(strsplit(gsub(regex, "", y, perl=TRUE),"((?<=\\:)\\s+no\\.)|((?<=\\:)\\s+nos\\.)", perl=TRUE))
    nums <- gsub("(^\\s+|\\s+$)|^\\:|\\:$", "", nums); nums <- nums[!nums==""]
    nums <- gsub("no.\\s*", "", nums)


    if(length(nums)<length(years)){
      nums <- unlist(strsplit(gsub(regex, "", y, perl=TRUE),"(?<!Note)\\s*\\:", perl=TRUE))
      nums <- gsub("^(\\s*no\\.*\\s*)", "", nums)
      nums <- gsub("(^\\s+|\\s+$)|^\\:|\\:$", "", nums); nums <- nums[!nums==""]}

    if(length(nums)>length(years)){
      testD<-grepl("\\d+", nums)
      if(any(!testD)){
        nums<-paste(nums[testD],"POSSIBLE PROCESSING ERROR!", nums[!testD], sep="")
      }else{
        nums<-nums[-1]
      }
    }
    issueDates <- paste(years, ": ", "no. ", nums, sep="")
    issueDates <- gsub("^(\\s*\\:\\s*no\\.*\\s*)", "", issueDates)
    return(issueDates)}

  cNames <- colnames(dat)
  issueDates_var <- dat[,cNames%in%issueDates]
  counter <- 1
  bigdata<-data.frame(matrix(NA, 1, dim(dat)[2]+4))
  group <- 1
  colnames(bigdata) <- c('Group', colnames(dat), 'Hierarchical_Relationship', 'Description_Level', 'Year_Num')

  for (iter in 1:dim(dat)[1]){
    issueDates_obs <- issueDates_extractor(issueDates_var[iter]);
    datSeries <- data.frame(dat[iter,], 'Hierarchical_Relationship'=1, 'Description_Level'='Series', 'Year_Num'=NA, stringsAsFactors = FALSE)

    if(!is.na(issueDates_obs[1])){
      datFiles <- data.frame(setNames(data.frame(matrix(ncol = dim(dat)[2], nrow = 1)), colnames(dat)),
                             'Hierarchical_Relationship'=2, 'Description_Level'='File', 'Year_Num'=issueDates_obs, stringsAsFactors = FALSE)
      datMerged <- cbind(group, rbind(datSeries, datFiles))}else{
        datMerged <- data.frame(group, datSeries)
      }
    bigdata[counter:((counter+dim(datMerged)[1])-1),] <- data.frame(datMerged, stringsAsFactors = FALSE)
    counter=counter+dim(datMerged)[1]
    group=group+1
    gc()
  }
  return(bigdata)}

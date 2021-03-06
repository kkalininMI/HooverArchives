# HooverArchives
This package implements various metadata processing tasks performed at the Hoover Institution Library & Archives.


1. <em> fromXMLtoCSV(filename, item_nodes, collection_nodes, excludeFiles = NULL, ...) </em>

  This function is used to convert our data from *.xml* to *.csv* format using XPath expressions. Define item nodes (i.e. files, items etc.) and collection nodes (information about collection) to extract the data.
    
  + *filename* -- name of XML file or vector of XML filenames.
  + *item_nodes* -- list of item nodes defining path, vector of nodes and node types (text/attrs).
  + *collection_nodes* -- list of collection nodes defining path, vector of nodes and vector of node types (text/attrs).
  + *excludeFiles* -- vector of filenames excluded from XML to CSV conversion.
  + *...* -- auxiliary parameters.

 Example
 
      library(HooverArchives)
      item_nodes = list(path = "(//c)|(//c01)|(//c02)|(//c03)",
                        nodes = c("primarynode", ".//unittitle", "./did//unitdate"),
                        types = c("attrs", "text", "text"))
      
      collection_nodes <- list(path="archdesc[@level='collection']",
                              nodes=c(".//unittitle", "./did//unitdate"),
                              types=c("text", "text"))
      
      filedata <- fromXMLtoCSV(system.file("rusdata.xml", package="HooverArchives"),
                               item_nodes, collection_nodes)




2. <em> fromFILEStoSERIES(dat = NULL, series_title = NULL, files = NULL, series_scope_note = NULL,
  series_date_range = NULL, scope_and_content = NULL, problems_notes = NULL, box_barcode = NULL, top_container = NULL,
  ...) </em>
  
  This function helps to organize the Belgium data by adding extra "Series" row and reformatting data in a proper format.

  + *dat* -- data frame.
  + *series_title* -- variable name "Series title".
  + *files* -- variable name for "Files".
  + *series_scope_note* -- variable name for "Series scope note".
  + *series_date_range* -- variable name for "Hoover date range".
  + *scope_and_content* -- variable name for "Scope and content".
  + *problems_notes* -- variable name for "Problems/Notes".
  + *box_barcode* -- variable name for "Box Barcode".
  + *top_container* -- variable name for "Top container".
  + *...* -- auxiliary parameters.


 Example 
 
     library(HooverArchives)
     library(readxl)
     library(xlsx)
    
     #Load data and create indices
     
     #Open Sheet 1
     dat2.1<-read.xlsx(system.file("BelgiumData.xlsx", package="HooverArchives"), 
                       sheetIndex=1, header=FALSE, encoding = "utf-8")
     dat2.1[]<-lapply(dat2.1, as.character)
     colnames(dat2.1)<-as.character(dat2.1[3,])
     dat2.1<-dat2.1[-(1:3),-c(1,14)];
     dat2.1$indexW<-dat2.1$`Item title`
    
     #Open Sheet 2
     dat2.2<-read.xlsx(system.file("BelgiumData.xlsx", package="HooverArchives"), 
                       sheetIndex=2, header=TRUE, encoding = "utf-8")
     dat2.2$indexW<- dat2.2$`Packet.Catalog.Title`
    
     #Merge two dataframe using BuildIndex and Merge_data functions
     index_matches<-buildIndex(dat2.1$indexW,dat2.2$indexW,
                               index_simplify=TRUE,
                               fuzzy_matching=TRUE,
                               index_hashing=FALSE)
     mdat<-mergeData(dat2.1,dat2.2, index_matches)
    
     #Use fromFILEStoSERIES() to add the Series row
     coverted.dat<-fromFILEStoSERIES(dat=mdat,
                               series_title="Series title",
                               files="index",
                               series_scope_note="Series scope note",
                               series_date_range="Hoover date range",
                               scope_and_content="Scope.and.content",
                               problems_notes="Series scope note",
                               box_barcode="Box_Barcode",
                               ckey="Ckey.x",
                               top_container="Final.Box..")
     coverted.dat$Date<-dateReformatter(coverted.dat$Date)
     convertedtoArchivesSpace<-subset(coverted.dat, select=c(
                               "Title", "Hierarchical_Relationship",	
                               "Processing_Information", "CkeyV", "Description_Level",	"Date",
                               "Top_Container_[indicator]",
                               "Box_Barcode", "Scope_and_content"), value=TRUE)

     #Save file in xlsx to preserve diacritic characters
     #write.xlsx(datHarvard, "convertedtoArchivesSpace.xlsx", sheetName = "ArchivesSpace", col.names = TRUE)


3. <em> fromSERIEStoFILES(dat = NULL, issueDates = NULL, locale = "English") </em>

  This function helps to obtain "Files" variable by splitting issue dates into separate rows.
  
  + *dat* -- original dataframe.
  + *issueDates* -- variables containing issue dates.
  + *locale* -- system's locale [locale = English].

 Example  
 
    library(HooverArchives)
    
    item_nodes<-list(path="(//c)|(//c01)|(//c02)|(//c03)|(//c04)|(//c05)|(//c06)|(//c07)|(//c08)",
                     nodes=c("primarynode", ".//unittitle", "./did//unitdate", "./did//unitdate",
                             "./did//language", "./did//abstract", ".//container",
                             "./did//container", "./scopecontent[@id]", ".//scopecontent/head",
                             ".//scopecontent/p", "./accessrestrict[@id]",
                             ".//accessrestrict/head", ".//accessrestrict/p", ".//note"),
                     types=c("attrs", "text", "text", "attrs", "attrs", "text", "text", "attrs", "attrs",
                             "text", "text", "attrs", "text", "text", "text"))
    
    collection_nodes<-list(path="archdesc[@level='collection']",
                     nodes=c(".//unittitle", "./did//unitdate", "./did//language", "./did//abstract"),
                     types=c("text", "text", "attrs", "text"))
    
    file_transf<-fromXMLtoCSV(system.file("rusnewspapers.xml", package="HooverArchives"), item_nodes, collection_nodes)
    
    convdata<-fromSERIEStoFILES(file_transf, issueDates="note.text", locale="Russian")
    
    #write.csv(convdata, "convdata_2012C30.csv")

4. <em> fromLATtoCYR(mdat, LAOR = TRUE, OROR = FALSE, EnglishDetection = TRUE, EnglishLength = NULL, RussianCorrection = FALSE, SensitivityThreshold = 0.1) </em>

  This function helps to convert transliterated Cyrillic to original Cyrillic.

  + *mdat* -- character vector to be back-transliterated to Cyrillic.
  + *tolanguage* -- 	language the text needs to be converted to ("Russian" by default). "Ukrainian" is an option.
  + *LAOR* -- rules of tranliteration from transliterated Cyrillic to original Cyrillic (the rules are listed in the file "transliterationLAOR.csv").
  + *OROR* -- rules to correct transliterated original Cyrillic (the rules are listed in the file "transliterationOROR.csv").
  + *EnglishDetection* -- if set to TRUE, the script avoids transliteration of words found in the English vocabulary (file: english.txt). If set to FALSE, only user defined stop words are used (file: stopwordsfile.csv).
  + *EnglishLength* -- threshold is set to ignore EnglishDectection words below given threshold.
  + *RussianCorrection* -- if set to TRUE, the script attempts to match every back-transliterated word with the Russian vocabulary (files: russian.txt and russian_surnames.txt).
  + *SensitivityThreshold* -- is used if RussianCorrection==TRUE. It determines algorithm's sensitivity to mismatches (values closer to 0 stand for higher sensitivity to mismatches). By deafault, *sensitivityThreshold* is set to 0.1 by default.

 Example  

    library(HooverArchives)
    
    # conversion to Russian
    dat<-c("Mezhdunarodnaia gazeta. Gl. redaktor: Iu. Zarechkin. Moscow, Russia. Semiweekly. 199?", "DEN' UCHITELIA komissiia po obrazovaniiu ob''edineniia Iabloko", "III-ii RIM vestnik Rossiiskogo patrioticheskogo dvizheniia. Redaktory: M. Artem'ev, V. Rugich. Moscow, Russia.")
    
    converteddata_ru <- fromLATtoCYR(dat, LAOR=TRUE, OROR=FALSE, EnglishDetection=TRUE)

    # conversion to Ukrainian
    dat<-read.csv(system.file("Ukraine_microform.csv", package="HooverArchives"), sep=",", encoding = "UTF-8", stringsAsFactors = FALSE)
    
    converteddata_uk <- fromLATtoCYR(dat$FIELD.245, tolanguage="Ukrainian")


5. <em> dateReformatter(datV) </em>

  This function helps to standartize and correct misspellings of date entries.

  + *datV* -- character vector of dates.

  Example
  
    library(HooverArchives)
    
    datesV<- c("1914:Aug 2 - 20; 1918:NOv 18",
               "1941:August-September 5, 1943 :Aug-1944:July")
    
    reformated.data <- dateReformatter(datesV)
    reformated.data

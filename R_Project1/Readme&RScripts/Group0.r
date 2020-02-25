#Group 0:- G3-Genes Genomes Genetics
#url:- http://www.g3journal.org
#Group Mates :- Aishwarya Rasal and Mounica Sreedhara

library(bitops)
library(RCurl)
library(XML)
library(xml2)
library(httr)
library(stringr)
library(Rcrawler)
library(rvest)
library(rlist)
library(openxlsx)
# group_id generation
Aishwarya = utf8ToInt("Rasal")
Mounica = utf8ToInt("Sreedhara")
group_id = (sum(Aishwarya) + sum(Mounica) )%%10


site.url = "http://www.g3journal.org"
main.page = read_html(site.url)
download_html(site.url,file="C:\\Users\\aishw\\Downloads\\R_Project1\\HTML//main_page.html")


#Extracting Articles Url
article.url=grep("content", LinkExtractor(site.url)[[2]], value = TRUE)
#TO extract all the articles for all the reports
all_article_url=list()
for(i in 1:length(article.url))
{
  all_article_url =append(all_article_url,
                          grep("content/?[0-9]/?[0-9]/?[0-9]*?[^./a-zA-Z]?[^/a-zA-Z]$",
                               LinkExtractor(article.url[i])[[2]],value = TRUE))
}
write.xlsx(unlist(all_article_url),"C:\\Users\\aishw\\Downloads\\R_Project1\\HTML//BME_content_Links.xlsx")

page.article.url=list()
#Extracting D DOI, Title, Authors, Author Affiliation, Corresponding Author, Corresponding Author Email, 
#Abstract are extracted and stored in a data frame. 
output=data.frame()
for(i in 1:length(all_article_url)){
  url= paste(all_article_url[i],sep="")
  download_html(url,file=paste("C:\\Users\\aishw\\Downloads\\R_Project1\\Crawled_pages//",i,"Article.html",sep=""))
  page=unlist(all_article_url[i])
  html=read_html(page)
  doc = htmlParse(html, asText=TRUE)
  
  
  #Extracting Title, DOI, Authors, Author Affiliation, Corresponding Author, Corresponding Author Email, Abstract and Pub date
  #Extracting Title
  Title = xpathSApply(doc, "//*[(@id = 'page-title')]", xmlValue)[1]
  DOI = xpathSApply(doc, "//*[@class='highwire-cite-metadata-doi highwire-cite-metadata']",xmlValue)[1]
  Authors = xpathSApply(doc, "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'highwire-cite-authors', ' ' ))]", xmlValue)[1]
  Authors = unlist(Authors)
  Authors = trimws(Authors, which="both")
  
  #Extracting AUthor email
  Primary_AUthor_email=xpathSApply(doc,"//*[@name='citation_author_email']",xmlGetAttr,"content")
  Primary_AUthor_email=trimws(Primary_AUthor_email,which="both")
  
  #Extracting AUthor Name + Auth Aff  
  
  Author_name = xpathSApply(doc, "//*[@name='citation_author']",xmlGetAttr, "content")
  Auth_aff= xpathSApply(doc, "//*[@name='citation_author_institution']",xmlGetAttr,
                        "content")
  Auth_aff=trimws(Auth_aff,which="both")
  
  
  #Extracting Abstract
  Abstract = xpathSApply(doc, "//*[(@id = 'p-2')]",xmlValue)
  Abstract = gsub('\n', '', Abstract)
  Abstract = gsub('\\sBackground\\s*', 'Background:', Abstract)
  Abstract = gsub('\\sResults\\s*', 'Results:', Abstract)
  Abstract = gsub('\\sConclusions\\s*', 'Conclusions:', Abstract)
  Abstract =trimws(Abstract,which="both")
  
  
  #Extracting Pubdate
  PubDate = xpathSApply(doc, "//*[@class='highwire-cite-metadata-date highwire-cite-metadata']",xmlValue )[1]
  PubDate=trimws(PubDate,which="both")
  Full_text= xpathSApply(doc, "//*[@name='citation_pdf_url']",xmlGetAttr,"content")
  Full_text=trimws(Full_text,which="both")
  
  #COncatenating all fields into one
  extract_row <- data.frame( DOI = paste(DOI,collapse = ','),
                             Title = paste(Title,collapse = ','),
                             Authors = paste(Author_name,collapse =','),
                             Corresponding_Author=paste(Author_name[1],collaspse=''),
                             Corresponding_Author_Email = paste(Primary_AUthor_email,collapse = ','),
                             Author_Affiliations=paste(Auth_aff, collapse =','),
                             Published_Date = PubDate, 
                             Abstract = paste(Abstract,collapse= ','),
                             Keywords = "NA",
                             FullText = Full_text)
  #Appending the extracted row of each article to output dataframe
  output = rbind(output,  extract_row)
}

# Writting the output dataframe 
write.table(output,file="C:\\Users\\aishw\\Downloads\\R_Project1\\SummaryAndResult//G3journals.txt",sep="\t",row.names=FALSE)
#reading a text file
reading.output=read.table("C:\\Users\\aishw\\Downloads\\R_Project1\\SummaryAndResult//G3journals.txt",header = TRUE, sep = "\t")
#writting summary to excel sheet
write.xlsx(reading.output,"C:\\Users\\aishw\\Downloads\\R_Project1\\SummaryAndResult//G3Journals_summary_of_fields.xlsx",sheetName = "Sheet1",col.names = TRUE,keepNA=TRUE)
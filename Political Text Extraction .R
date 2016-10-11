##File for extracting information from PDFs

#get the package 
install.packages("pdftools")
library(pdftools)

rm(list = ls())

#the location of the file
pdf_file = file.path("election results.pdf")

#extract from the file 
info = pdf_info(pdf_file)
text = pdf_text(pdf_file)

###########First extract the district from the data frame
#create vector to store the district info in 
districts = vector(mode = "character", length = length(text))
#extract and store district in the districts vector.  Also remove the processed part from the string
for (i in 1:length(text)) {
  pos = regexpr(',', text[i]) # Returns position of 1st comma on the page
  district = substr(text[i], 1, pos) # extract everything before the 1st comma on the page
  district = gsub(" ", "", district, fixed = TRUE) #remove spaces
  district = gsub(",", "", district, fixed = TRUE) #remove comma
  districts[i] = district
  substr(text[i], 1, pos) = ""
}
###########First extract the constituency from the data frame
#create vector to store the constituency info in 
constituencies = vector(mode = "character", length = length(text))
#extract and store district in the districts vector.  Also remove the processed part from the string
for (i in 1:length(text)) {
  pos1 = regexpr(',', text[i]) # Returns position of 1st comma on the page
  pos2 = regexpr('\n', text[i]) # Returns position of 1st \n on the page
  constituency = substr(text[i], pos1, pos2) # extract everything before the 1st comma on the page
  constituency = gsub(",", "", constituency, fixed = TRUE) #remove comma
  constituencies[i] = constituency
}

output = data.frame(districts, constituencies)
colnames(output) = c("District", "Constituency")
write.csv(output, file = "output.csv")

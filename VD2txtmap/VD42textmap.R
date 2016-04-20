require('RSQLite')
require('fpc')
require('DBI')
vdl="Voice Dream Library"
#vdl="VoiceDreamLibrary"
citekeys="menu"
#sqlite='Voice_Dream_Reader.sqlite'
sqlite='LibraryIndex.sqlite'
#system(paste("sqlite3 \"",vdl,"/",sqlite,"\" \"VACUUM;\"",sep="")) # checkpoint WAL file and force VoiceDream.sqlite to update
con <- 	dbConnect(RSQLite::SQLite(), dbname=paste(vdl,"/",sqlite,sep="")) ## connect to db
tables <- dbListTables(con) ## list all tables
vddb <- vector("list", length=length(tables))
for (i in seq(along=tables)) vddb[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep="")) ## create a data.frame for each table

require(XML)

xml.url <-'/Users/bambrose/Library/Mobile Documents/iCloud~com~voicedream~reader/Documents/Library/4E03222B-F9CB-41E6-B15F-13E3D3F181B0/Metadata.vdrmetadata/UserMetadata'
xml.url <-'/Users/bambrose/Library/Mobile Documents/iCloud~com~voicedream~reader/Documents/Library/4E03222B-F9CB-41E6-B15F-13E3D3F181B0/Metadata.vdrmetadata/Presentations'
xml.url <-readLines('/Users/bambrose/Library/Mobile Documents/iCloud~com~voicedream~reader/Documents/Library/4E03222B-F9CB-41E6-B15F-13E3D3F181B0/Metadata.vdrmetadata/StaticMetadata')

# Use the xmlTreePares-function to parse xml file directly from the web

xmlfile <- xmlTreeParse(xml.url,asText = T)
# the xml file is now saved as an object you can easily work with in R:
class(xmlfile)
# Use the xmlRoot-function to access the top node
xmltop = xmlRoot(xmlfile)
# have a look at the XML-code of the first subnodes:
print(xmltop)[1:2]

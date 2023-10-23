# update md links

file  = "MainPost"
fileName = paste0(file,'-reddit.md')
file.copy(paste0(file,".md"),fileName, overwrite = T)

text <- readLines(fileName, warn = FALSE)
text <- gsub("\\!\\[", "[", text)
text <- gsub("\\.\\./", "https://bischrob.github.io/", text)
writeLines(text, fileName)

week = 8
file  = "WeeklyPost"
fileName = paste0(file,'-week',week,'-reddit.md')
file.copy(paste0(file,".md"),fileName, overwrite = T)

text <- readLines(fileName, warn = FALSE)
text <- gsub("\\!\\[", "[", text)
text <- gsub("\\.\\./", "https://bischrob.github.io/", text)
writeLines(text, fileName)

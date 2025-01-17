setwd('~/Dropbox/RWD/metin-analizi-genel/')

library(data.table)
library(stringr)
library(crayon)
library(tabulapdf)


sozluk_patika = 'data/TDK - Türkçe Sözlük 1 Cilt A-J.pdf'
text = 'initial'
page_index = 17
max_page_index = 30
my_data = data.table()

while(text!='' & page_index<=max_page_index){
  cat(yellow('Reading PDF from page:',page_index,' '))
  text = tabulapdf::extract_text(file = sozluk_patika,pages = page_index)
  cat(green('OK!....'))
  
  lines = str_split(string = text,pattern = '\n',simplify = T)
  lines = as.character(lines)
  
  d = data.table(lines=lines)
  
  d = d[lines%like%' (i)*s\\.']
  
  cat(blue('Number of lines:',nrow(d)),'\n')
  
  my_data = rbind(my_data,d)
  
  gc()
  
  page_index = page_index+1
}


my_data = my_data[!lines%like%'[2-9]']
my_data[,vocab:=gsub(' (i)*s\\..*','',lines)]



my_data = my_data[!vocab%like%'[A-zğüşıöçĞÜŞİÖÇ]{3}(lık|luk|lik|lük)']
my_data[,vocab:=gsub(' [A-zŞİÖÇĞÜğşiıöç]{1,3}\\.','',vocab)]

my_data[,vocab:=str_trim(vocab,'both')]

my_data[,spaces:=sapply(vocab,function(x){
  y = gsub('[^ ]','',x)
  y = nchar(y)
  y
})]

my_data[spaces>2,vocab:=gsub(' ','',vocab)]

my_data[,is_single_letter:=ifelse(vocab%like%'\\b[A-zğüşıöçĞÜŞİÖÇ]\\b',T,F)]
my_data[is_single_letter==T,vocab:=gsub(' ','',vocab)]

my_data[,vocab:=gsub(' ve\\b','',vocab)]
my_data[,vocab:=gsub('\\(.*\\)','',vocab)]

my_data[,vocab:=str_trim(vocab,'both')]

my_data[,spaces:=NULL]
my_data[,is_single_letter:=NULL]


my_data[,form1:=gsub(',.*','',vocab)]

x = 'abat, -dı'
my_data[,form2:=sapply(vocab,function(x){
  
  if(grepl(pattern = ',',x)){
    y1 = str_split(x,',',n=2,simplify = T)[1]
    y2 = str_split(x,',',n=2,simplify = T)[2]
    
    y1 = str_sub(y1,1,nchar(y1)-1)
    y2 = gsub('[ \\-]','',y2)
    y2 = str_sub(y2,1,1)
    
    y_new = paste0(y1,y2)
    
    y_new  
    
  }else{
    ''
  }
  
  
})]


my_data[1:99]





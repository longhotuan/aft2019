#### import libraries ####
# data wrangling

library(tidyverse)
library(reshape)
library(data.table)
library(xlsx)
library(gridExtra)
library(grid)
library(chron)
library(devtools)
library(rscopus)
library(rlist)
library(rgeos)
library(future)
library(parallel)
library(doParallel)
library(feather)

# data visualization 

library(GGally)
library(RColorBrewer)
library(proj4)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(mice)
library(VIM)
library(ggmosaic)
library(esquisse)
library(bibliometrix)
library(ggwordcloud)
library(colorspace)
library(rworldmap)
library(countrycode)
library(usethis)

#### From Scopus website (csv) #### # still wrong the data

temp <- list.files(pattern="*.feather")
myfiles <- lapply(temp, read_feather)

#### Remove the lat and long, changing factor

rm_latlong <- function(x){
    y <- str_which(colnames(x), "lat_")[1]
    x <- x[,1:(y-1)]
}

myfiles <- lapply(myfiles, rm_latlong)

factor_convert <- function(x){
    x$`Source title` <- as.factor(x$`Source title`)
    x$`Language of Original Document`<- as.factor(x$`Language of Original Document`)
    x$`Document Type` <- as.factor(x$`Document Type`)
    x$`Access Type` <- as.factor(x$`Access Type`)
    x$`Abbreviated Source Title` <- as.factor(x$`Abbreviated Source Title`)
    x <- x %>% filter(Year < 2020)
}

myfiles <- lapply(myfiles, factor_convert)
names(myfiles) <- c("SDG_6.1", "SDG_6.2", "SDG_6.3", "SDG_6.4", "SDG_6.5", "SDG_6.6", "SDG_6.a", "SDG_6.b","SDG_6")

#### Only takes the publications with the collaboration with researchers from partner countries

choose_PC <- function(x){
    y <- x[, which(colnames(x) %in% c("Benin", "Burkina Faso", "Burundi", "DR Congo", "Guinea", "Mali", "Morocco", 
                                         "Mozambique", "Niger", "Uganda", "Palestine", "Rwanda", "Senegal", "Tanzania"))]
    y <- bind_cols(x[,1:19], y)
    y <- y[rowSums(is.na(y[20:30])) != ncol(y[20:30]), ]
}


myfiles <- lapply(myfiles, choose_PC)

# # save csv and feather #### 
# 
# save_df <- function(x, y){
#     write.csv(x, file = paste(y, "_PC.csv", sep=""), row.names = FALSE)
#     write_feather(x, path = paste(y, "_PC.feather", sep=""))
# }
# 
# map2(myfiles, names(myfiles), save_df)
# 
# 


#### Number and top of keywords ----

KW <- function(x){
    keyword <- strsplit(x$`Author Keywords`, "; ")
    for (i in 1:length(keyword)){
        keyword[i] <- as.data.frame(matrix(as.data.frame(keyword[i])))
    }
    keyword2 <- rbindlist(keyword)
    colnames(keyword2)[1]<- "keyword"
    keyword2<- keyword2[complete.cases(keyword2),]
    keyword2$keyword <- str_to_title(keyword2$keyword)
    keyword3 <- keyword2 %>%
        dplyr::group_by(keyword) %>% 
        dplyr::summarise(n=n()) %>% 
        dplyr::arrange(desc(n)) 
    return(keyword3)
}

# make the top keywords

myplot <- lapply(myfiles, KW)
myplot <- lapply(myplot, head, n = 20)
names(myplot) <- c("SDG_6.1", "SDG_6.2", "SDG_6.3", "SDG_6.4", "SDG_6.5", "SDG_6.6", "SDG_6.a", "SDG_6.b","SDG_6")
myfiles <- map2(myfiles, names(myfiles), function(x,y) {x$SDG6 <- y; return(x)})

# print the graphs

plot_kw <- function(x, y){
    ggplot(x, aes(label = keyword, size =n ,color = rainbow_hcl(20))) +
        geom_text_wordcloud_area(shape = "star") +
        scale_size_area(max_size = 15) +
        theme_minimal()+
        ggtitle(paste(y))+
        theme(plot.title = element_text(hjust = 0.5, size = 18))
}

plot_topkw <- map2(myplot, names(myplot), plot_kw)

ggsave(filename="2009-2019_PC.jpeg", 
       arrangeGrob(grobs = plot_topkw),
       units = 'cm', height = 50, width = 75, dpi = 300)

lapply(names(plot_topkw), function(x) ggsave(filename=paste(x,"_alltime_PC.jpeg",sep=""), plot=plot_topkw[[x]], units = 'cm', height = 20, width = 20, dpi = 300))

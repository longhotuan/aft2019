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
library(mapview)
library(htmlwidgets)
library(corrplot)
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

#### From Scopus website (csv) ####

temp <- list.files(pattern="*.csv")
myfiles <- lapply(temp, read_csv)
colnames_chosen <- colnames(myfiles[[1]])[c(2, 4:6, 13, 14, 16, 18, 19, 20, 30:33, 38:40, 42, 43)]

#choose the useful columns
myfiles <- lapply(myfiles, subset, select = colnames_chosen)

factor_convert <- function(x){
    x$`Source title` <- as.factor(x$`Source title`)
    x$`Language of Original Document`<- as.factor(x$`Language of Original Document`)
    x$`Document Type` <- as.factor(x$`Document Type`)
    x$`Access Type` <- as.factor(x$`Access Type`)
    x$`Abbreviated Source Title` <- as.factor(x$`Abbreviated Source Title`)
    x <- x %>% filter(Year < 2020 & Year > 2008)
}

myfiles <- lapply(myfiles, factor_convert)
names(myfiles) <- c("SDG_6.1", "SDG_6.2", "SDG_6.3", "SDG_6.4", "SDG_6.5", "SDG_6.6", "SDG_6.a", "SDG_6.b","SDG_6")


# put the affiliation into the dataset ####

affi_split <- function(y){
    y$Affi <- NA
    for (j in 1: nrow(y)){
        y$Affi[j] <- list(as.list(str_split_fixed(y$Affiliations[j], "; ", 
                                                    n =1+ str_count(y$Affiliations[j], "; "))))
    }
    return(y)
}

myfiles2 <- lapply(myfiles, affi_split)

# split into country

myfiles3 <- lapply(myfiles2, function(x) {x$Country <- x$Affi; return(x)})


country_split <- function(x){
    for (i in 1:nrow(x)){
        for (j in 1:length(x$Affi[[i]])){
            x$Country[[i]][[j]] <- as.list(str_split_fixed(x$Affi[[i]][[j]], ", ", n = 1 + str_count(x$Affi[[i]][[j]], ", ")))
            k <- length(x$Country[[i]][[j]])
            x$Country[[i]][[j]] <-  x$Country[[i]][[j]][[k]]
        }
    }
    return(x)
}


myfiles3 <- lapply(myfiles3, country_split)

#### looking for articles having authors from PC ####

# this lat long has wrong country name in it
lat_long2 <- read.csv("lat_long.csv")
lat_long2 <- lat_long2[,-1]


country_diff <- vector(mode = "list", 9)
check_country <- function(x, y){
    y <- setdiff(unique(unlist(x$Country)), lat_long2$Country)
    return(y)
}

country_diff <- map2(myfiles3, country_diff, check_country)
country_diff2 <- unique(unlist(country_diff))

# check with lat long with correct country name 

lat_long <- as.data.frame(gCentroid(getMap(resolution="high"), byid=TRUE))
lat_long$Country <- rownames(lat_long)
rownames(lat_long) <- c(1:nrow(lat_long))

country2_diff <- vector(mode = "list", 9)
country2_diff <- map2(myfiles3, country2_diff, check_country)
country2_diff2 <- unique(unlist(country2_diff))




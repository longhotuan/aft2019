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
lat_long <- bind_rows(lat_long, data.frame(x = 31.9522, y = 35.2332, Country = 'Palestine'))
lat_long <- bind_rows(lat_long, data.frame(x = 16.2650, y = 61.5510, Country = 'Guadeloupe'))

country2_diff <- vector(mode = "list", 9)
country2_diff <- map2(myfiles3, country2_diff, check_country)
country2_diff2 <- unique(unlist(country2_diff))

old_1 <- c("United States", "Serbia", "Viet Nam", "Russian Federation", 
           "Tanzania", "Syrian Arab Republic", "Congo", "Brunei Darussalam", "Macau", "Cote d'Ivoire",
           "Chinese Academy of Agricultural Sciences", "The Netherlands",
           "USA", "Bern Switzerland", "Côte d’Ivoire", "UK", "Ghent University", "KU Leuven",
           "Republic of South Sudan", "Brussels", "Southampton", "National Belge de la Recherche Scientifique FRS - FNRS", "Democratic Republic Republic of the Congo",
           "Territory Protection and Sea",
           "Schlumberger", "Mayotte", "INP", "Fond de la Recherche Scientifique FNRS","Universiteandacute", "Martinique", "University of Rennes I",
           "Institute for Research in Biomedicine", "Falkland Islands (Malvinas)","Universite de Toulouse", 
           "Université de Toulouse",
           "Libyan Arab Jamahiriya", "IUEM", 
           "Royal Netherlands Institute for Sea Research \\(NIOZ", 
           "Procter &amp",
           "Grenoble INP - Université de Savoie - Université J. Fourier", 
           "CNRS",
           "LMOPS", "Isotope Bioscience Laboratory Andndash", "SCRiPTS", 
           "Hong Kong", "VU University Medical Center", "Timor Leste", "Bahamas", "Reunion", "INPT",
           "IBMC", "Tunisia E-mail: hdbenmansour@gmail.com", "L'Oreal Research Andamp", "Universidad Autónoma de Madrid",
           "Bahir Dar University Bahir Dar Institute of Technology Bahir Dar Ethiopia",
           "UMR 7574 75005 Paris France", "China \\(e-mail: song.liu3000@hotmail.com\\).","Leuven Food Science and Nutrition", "Earth and Life Institute -Agronomy \\(ELI-A",
           "Deltares", "Ministry of Agriculture and Rural Affairs", "Satlantic","Vlaamse Milieumaatschappij", "Democratic Republic Congo",
           "KU Leuven \\(University of Leuven\\) &amp","Universite",
           "Royal Netherlands Institute for Sea Research \\(NIOZ-Yerseke",
           "Building Physics Section", "Procter Andamp", "University of Leuven &amp","China (e-mail: song.liu3000@hotmail.com).",
           "Bio Base Europe Pilot Plant Rodenhuizekaai 1 9042 Ghent Belgium",
           "AbbVie;", "Earth and Environment", "Ministry of Agriculture", "Campus International de Baillarguet", "Earth and Life Institute -Agronomy (ELI-A",
           "Leuven", "Ministry of Education",
           # new ones 13/12/19
           "Universit\\\\xe9 de Toulouse", "Grenoble INP - Universit\\\\xe9 de Savoie - Universit\\\\xe9 J. Fourier", 
           "North Macedonia", "Universidad Aut\\\\\xf3noma de Madrid", "Storengy"
)

new_1 <- c("United States of America", "Republic of Serbia", "Vietnam", "Russia",
           "United Republic of Tanzania", "Syria", "Republic of the Congo","Brunei", "Macau S.A.R", 
           "Ivory Coast", "China", "Netherlands", "United States of America",
           "Switzerland", "Ivory Coast","United Kingdom", "Belgium", "Belgium", "South Sudan", "Belgium",
           "United Kingdom", "France", "Democratic Republic of the Congo", "Belgium", "United States of America", "France", "Belgium", "France",
           "Belgium", "France", "France", "Spain", "Falkland Islands","France", 
           "France",
           "Libya", "Mexico", 
           "Netherlands",
           "Belgium", "France",
           "France", "France", "Belgium", "Belgium", 
           "Hong Kong S.A.R.", "Netherlands", "East Timor", "The Bahamas", "France", "France", "France",
           "Tunisia", "France", "Spain", "Ethiopia", "France", "China", "Belgium", "Belgium", "Netherlands", "China", "Canada", "Belgium", "Democratic Republic of the Congo",
           "Belgium", "Belgium", "Netherlands", "Belgium", "Belgium", "Belgium", "China", "Belgium", "Belgium","United Kingdom", "Belgium", "France", 
           "Belgium", "Belgium", "Belgium", 
           # new ones 13/12/19
           "France", "France", "Macedonia", "Spain", "United Kingdom"
)

# change name of the country in myfiles

change_name <- function(x){
    for(i in 1:nrow(x)){
        for(j in 1:length(x$Country[[i]])){
            for(k in 1:length(old_1)){
                if (x$Country[[i]][[j]] == old_1[k]){
                    x$Country[[i]][[j]] <- new_1[k]
                }
            }
        }
    }
    return(x)
}


myfiles4 <- lapply(myfiles3, change_name)

# Checking if the name is ok 

country3_diff <- vector(mode = "list", 9)
country3_diff <- map2(myfiles4, country3_diff, check_country)
country3_diff2 <- unique(unlist(country3_diff))
country3_new <- c("France", "Netherlands", "France", "Belgium", "Spain", "Netherlands")
change_name2 <- function(x){
    for(i in 1:nrow(x)){
        for(j in 1:length(x$Country[[i]])){
            for(k in 1:length(country3_diff2)){
                if (x$Country[[i]][[j]] == country3_diff2[k]){
                    x$Country[[i]][[j]] <- country3_new[k]
                }
            }
        }
    }
    return(x)
}
myfiles5 <- lapply(myfiles4, change_name2)

# checking again

country4_diff <- vector(mode = "list", 9)
country4_diff <- map2(myfiles5, country4_diff, check_country)
country4_diff2 <- unique(unlist(country4_diff)) 

#### ok 

#### add the names of the country as columns

add_namecolumn <- function(x){
    b <- as.data.frame(matrix(data = NA, nrow = nrow(x), ncol = length(levels(as.factor(unlist(x$Country))))))
    colnames(b) <- levels(as.factor(unlist(x$Country)))
    y <- bind_cols(x, b)
    return(y)
}

myfiles6 <- lapply(myfiles5, add_namecolumn)

# remove the country in lat long 

names_country <- lapply(myfiles6, function(x) {y<- unique(unlist(x$Country)); return(y)})

list_latlong <- vector(mode = 'list', 9)
list_latlong <- lapply(list_latlong, function(x) {x <- lat_long; return(x)})


create_latlong <- function(x, y){
    z <- x %>% filter(Country %in% y)
    z <- z[order(z$Country),]
    return(z)
}

for(i in 1:length(list_latlong)){
    list_latlong2[[i]] <- list_latlong[[i]] %>% filter(Country %in% names_country[[i]])
    list_latlong2[[i]] <- list_latlong2[[i]][order(list_latlong2[[i]]$Country),]
}

# list_latlong2 <- map2(list_latlong, names_country, create_latlong) # Doesn't work without reasons ?!?!?!?!

create_df <- function(x){
    y <- x[, (which(colnames(x) == 'Country')+1):ncol(x), drop = F] 
    y <- y[, order(names(y))]
    x[, (which(colnames(x) == 'Country')+1):ncol(x)] <- NULL
    x <- bind_cols(x,y)
    return(x)
}


myfiles7 <- lapply(myfiles6, create_df)

# Making new lat and long columns

create_df_latlong <- function(x,z){
    x_country <- x[, (which(colnames(x) == 'Country')+1):ncol(x), drop = F] 
    x_lat <- x[, (which(colnames(x) == 'Country')+1):ncol(x), drop = F]
    colnames(x_lat) <- paste("lat",colnames(x[,(which(colnames(x) == 'Country')+1):ncol(x)]), sep = "_")
    x_long <- x[, (which(colnames(x) == 'Country')+1):ncol(x), drop = F]
    colnames(x_long) <- paste("long",colnames(x[,(which(colnames(x) == 'Country')+1):ncol(x)]), sep = "_")
    
    for(i in 1:ncol(x_country)){
        for(j in 1:nrow(x_country)){
            if(!is.na(x_country[j,i])){
                x_country[j,i] <- z$Country[i]
                x_lat[j,i] <- z$x[i]
                x_long[j,i] <- z$y[i]
            }
        }
    }
    x[, (which(colnames(x) == 'Country')+1):ncol(x)] <- NULL
    x <- bind_cols(x, x_country, x_lat, x_long)
    return(x)
}

myfiles8 <- lapply(myfiles7, create_df_latlong)


#### Save the dataframe ####

# remove the list in the dataframe

remove_list <- function(x){
    x <- x %>% select(-c("Affi", "Country"))
}

myfiles9 <- lapply(myfiles8, remove_list)

save_df <- function(x, y){
    write.csv(x, file = paste(y, ".csv", sep=""), row.names = FALSE)
    write_feather(x, path = paste(y, ".feather", sep=""))
}

map2(myfiles9, names(myfiles8), save_df)

# lapply(names(myfiles9), function(x) write.csv(file = paste(x, ".csv", sep = ""), x = myfiles9[x], row.names = FALSE))
# lapply(names(myfiles9), function(x) write_feather(path = paste(x, ".feather", sep = ""), x = myfiles9[x]))



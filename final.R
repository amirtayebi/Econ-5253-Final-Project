# Set the required API code, I'm not allowed to share the API!
pvs.key = "Your API"



#Loading required packages
library(tidyverse)
library(qdapTools) 
library(pvsR)
library(plyr)
library(dplyr)
library(eeptools)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(stargazer)


##################################
#Data Scrapping                  #
##################################
#Create an empty data frame
Amir <- data.frame()

#Create a loop to get all the biographical data(This might take up to one week!)
for (i in (1:186230)) {

bio <- CandidateBio.getDetailedBio(i)
bio2 <- ldply (bio, data.frame)


#Create a function to shift cells
shift <- function(x, n){

  c(x[-(seq(n))], rep(NA, n))
  }

  
bio2$name <- shift(bio2$name, 1)
bio2$parties <- shift(bio2$parties, 1)
bio2$title <- shift(bio2$title, 1)
bio2$shortTitle <- shift(bio2$shortTitle, 1)
bio2$name.1 <- shift(bio2$name.1, 1)
bio2$type <- shift(bio2$type, 1)
bio2$status <- shift(bio2$status, 1)
bio2$firstElect <- shift(bio2$firstElect, 1)
bio2$lastElect <- shift(bio2$lastElect, 1)
bio2$district <- shift(bio2$district, 1)
bio2$districtId <- shift(bio2$districtId, 1)
bio2$stateId <- shift(bio2$stateId, 1)
bio2$committee <- shift(bio2$committee, 1)
bio2$committee.1 <- shift(bio2$committee.1, 1)
bio2$committee.2 <- shift(bio2$committee.2, 1)
bio2$committee.3 <- shift(bio2$committee.3, 1)
bio2$committee.4 <- shift(bio2$committee.4, 1)
bio2$committee.5 <- shift(bio2$committee.5, 1)
bio2$degree <- shift(bio2$degree, 2)
bio2$field <- shift(bio2$field, 2)
bio2$school <- shift(bio2$school, 2)
bio2$span <- shift(bio2$span, 2)
bio2$fullText <- shift(bio2$fullText, 2)
bio2$organization <- shift(bio2$organization, 4)





if (i==9090) {


Amir <- data.frame(bio2)

}else{ 

Amir <- bind_rows(Amir,bio2)
}
}

##################################
#Data Cleaning                   #
##################################
#Delete extra rows
Amir<-Amir[!(Amir$.id=="political"),]
Amir<-Amir[!(Amir$.id=="office"),]
Amir<-Amir[!(Amir$.id=="education"),]
Amir<-Amir[!(Amir$.id=="profession"),]
Amir<-Amir[!(Amir$.id=="congMembership"),]
Amir<-Amir[!(Amir$.id=="orgMembership"),]
y<- select (y,-c(nickName,crpId,photo,suffix, pronunciation, specialMsg, shortTitle, termStart, termEnd,span, gpa,special ))
y2 <- y[!is.na(y$firstElect),]


#Introduce the dummy variable for the resign to run law
Amir$resigntorun <- NULL
Amir$resigntorun[Amir$homeState == "AZ" |Amir$homeState == "FL" |Amir$homeState == "TX" |Amir$homeState == "HI" |Amir$homeState == "GA" ] = 1
Amir$resigntorun[Amir$homeState == "AR" |Amir$homeState == "CO" |Amir$homeState == "DE" |Amir$homeState == "ID" |Amir$homeState == "IL" |Amir$homeState == "IN" |Amir$homeState == "IA" |Amir$homeState == "KS"|Amir$homeState == "ME" |Amir$homeState == "MN" |Amir$homeState == "MS" |Amir$homeState == "MT"|Amir$homeState == "NE" |Amir$homeState == "NV" |Amir$homeState == "NM" |Amir$homeState == "NY" |Amir$homeState == "NH" |Amir$homeState == "NC" |Amir$homeState == "ND" |Amir$homeState == "OK" ] = 0
Amir$resigntorun[Amir$homeState == "OR"] = 0
Amir$resigntorun[Amir$homeState == "PA" |Amir$homeState == "RI" |Amir$homeState == "SC" |Amir$homeState == "SD" |Amir$homeState == "TN" |Amir$homeState == "UT" |Amir$homeState == "VT"] = 0
Amir$resigntorun[Amir$homeState == "VA" | Amir$homeState == "WA" |Amir$homeState == "WI" |Amir$homeState == "WY" |Amir$homeState == "AL" |Amir$homeState == "AL" |Amir$homeState == "CA" |Amir$homeState == "CT"|Amir$homeState == "KY" |Amir$homeState == "LA" |Amir$homeState == "MD" |Amir$homeState == "MI"|Amir$homeState == "MO" |Amir$homeState == "NJ" |Amir$homeState == "OH" |Amir$homeState == "WV"] = 0

#Introduce the dummy variable for the Dual-Emplyment Law

 Amir$dualemp <- NULL
Amir$dualemp[Amir$homeState == "AL" |Amir$homeState == "AK" |Amir$homeState == "AZ" |Amir$homeState == "CA" |Amir$homeState == "CT" |Amir$homeState == "GA" |Amir$homeState == "KY" |Amir$homeState == "LA" |Amir$homeState == "MD" |Amir$homeState == "MI" |Amir$homeState == "MA"|Amir$homeState == "MO" |Amir$homeState == "NJ" |Amir$homeState == "OH" |Amir$homeState == "WV"] = 1
Amir$dualemp[Amir$homeState == "AR" |Amir$homeState == "CO" |Amir$homeState == "DE" |Amir$homeState == "FL" |Amir$homeState == "HI" |Amir$homeState == "ID" |Amir$homeState == "IL" |Amir$homeState == "IN" |Amir$homeState == "IA" |Amir$homeState == "KS"|Amir$homeState == "ME" |Amir$homeState == "MN" |Amir$homeState == "MS" |Amir$homeState == "MT"|Amir$homeState == "NE" |Amir$homeState == "NV" |Amir$homeState == "NM" |Amir$homeState == "NY" |Amir$homeState == "NH" |Amir$homeState == "NC" |Amir$homeState == "ND" |Amir$homeState == "OK" |Amir$homeState == "OR"|Amir$homeState == "PA" |Amir$homeState == "RI" |Amir$homeState == "SC" |Amir$homeState == "SD"|Amir$homeState == "TN" |Amir$homeState == "TX" |Amir$homeState == "UT" |Amir$homeState == "VT" |Amir$homeState == "VA" |Amir$homeState == "WA" |Amir$homeState == "WI" |Amir$homeState == "WY" ] = 0

#Introduce the dummy variable for gender
Amir$genderr <- NULL
Amir$genderr[Amir$gender == "Male"] = 1
Amir$genderr[Amir$gender == "Female"] = 0

#Introduce the dummy variable for party
Amir$party <- NULL
Amir$party[Amir$parties == "Democratic"] = 1
Amir$party[Amir$parties == "Republican"] = 0

#Replace misiing observation with NAs
Amir$resigntorun[which(is.nan(Amir$resigntorun))] = NA
Amir$resigntorun[which(Amir$resigntorun==Inf)] = NA
Amir$genderr[which(is.nan(Amir$genderr))] = NA
Amir$genderr[which(Amir$genderr==Inf)] = NA


#Calculate the age of candidates
Amir$age <- substr(Amir$birthDate, 7, 10)
Amir$births <- as.factor(Amir$birthDate)
Amir$age <- as.Date(Amir$births, "%m/%d/%Y")
calc_age <- function(birthDate, refDate = Sys.Date()) {

    require(lubridate)

    period <- as.period(new_interval(birthDate, refDate),
                        unit = "year")

    period$year

}


Amir$ag <- calc_age(Amir$age)
#Introduce the dummy variables for different braches of Christianity and other religions
Amir$Catholic <- NULL
Amir$Catholic[Amir$religion == "Catholic" | Amir$religion=="Roman Catholic" | Amir$religion == "Christian-Catholic" | Amir$religion=="Catholic/Christian"] = 1
Amir$Catholic[!(Amir$religion == "Catholic" | Amir$religion=="Roman Catholic" | Amir$religion == "Christian-Catholic" | Amir$religion=="Catholic/Christian")] = 0

Amir$Baptist <- NULL
Amir$Baptist[Amir$religion == "Baptist" | Amir$religion=="Southern Baptist" | Amir$religion == "Christian (Baptist)" | 
Amir$religion=="Missionary Baptist"| Amir$religion=="Christian-Baptist" | Amir$religion == "American Baptist" |
Amir$religion=="First Baptist"| Amir$religion == "Independent Baptist" | Amir$religion=="Baptist/Christian"| 
Amir$religion=="Christian - Baptist" | Amir$religion=="Free Will Baptist"| Amir$religion=="Protestant (Baptist)" ] = 1

Amir$Baptist[!(Amir$religion == "Baptist" | Amir$religion=="Southern Baptist" | Amir$religion == "Christian (Baptist)" | 
Amir$religion=="Missionary Baptist"| Amir$religion=="Christian-Baptist" | Amir$religion == "American Baptist" |
Amir$religion=="First Baptist"| Amir$religion == "Independent Baptist" | Amir$religion=="Baptist/Christian"| 
Amir$religion=="Christian - Baptist" | Amir$religion=="Free Will Baptist"| Amir$religion=="Protestant (Baptist)" )] = 0


Amir$Methodist <- NULL
Amir$Methodist[Amir$religion == "Methodist" | Amir$religion=="United Methodist"  | 
Amir$religion=="United Methodist Church"| Amir$religion=="First United Methodist Church" | Amir$religion == "Methodist (United)"|
 Amir$religion == "Christian Methodist"| Amir$religion=="Free Methodist" | Amir$religion=="Christian--United Methodist" |
 Amir$religion=="Christian - Methodist" | Amir$religion=="Protestant (Methodist)" ] = 1
 
 Amir$Methodist[!(Amir$religion == "Methodist" | Amir$religion=="United Methodist"  | 
Amir$religion=="United Methodist Church"| Amir$religion=="First United Methodist Church" | Amir$religion == "Methodist (United)"|
 Amir$religion == "Christian Methodist"| Amir$religion=="Free Methodist" | Amir$religion=="Christian--United Methodist" |
 Amir$religion=="Christian - Methodist" | Amir$religion=="Protestant (Methodist)" )] = 0

 
 Amir$Protestant <- NULL
Amir$Protestant[Amir$religion == "Protestant" | Amir$religion=="Protestant Christian" | Amir$religion == "Protestant (United Church of Christ)" |
 Amir$religion=="Protestant-Presbyterian" | Amir$religion=="Presbyterian" | Amir$religion == "First Presbyterian Church" | Amir$religion == "Christian (Presbyterian)"] = 1
Amir$Protestant[!(Amir$religion == "Protestant" | Amir$religion=="Protestant Christian" | Amir$religion == "Protestant (United Church of Christ)" | Amir$religion=="Protestant-Presbyterian"| Amir$religion=="Presbyterian" 
| Amir$religion == "First Presbyterian Church" | Amir$religion == "Christian (Presbyterian)")] = 0

 Amir$Lutheran <- NULL
Amir$Lutheran[Amir$religion == "Lutheran" | Amir$religion=="Christian (Lutheran)" | Amir$religion == "Lutheran (ELCA)" | Amir$religion=="Lutheran Church--Missouri Synod"] = 1
Amir$Lutheran[!(Amir$religion == "Lutheran" | Amir$religion=="Christian (Lutheran)" | Amir$religion == "Lutheran (ELCA)" | Amir$religion=="Lutheran Church--Missouri Synod")] = 0

Amir$Jewish <- NULL
Amir$Jewish[Amir$religion == "Jewish" ] = 1
Amir$Jewish[!(Amir$religion == "Jewish" )] = 0

Amir$Episcopalian <- NULL
Amir$Episcopalian[Amir$religion == "Episcopalian" | Amir$religion == "Episcopal" | Amir$religion == "African Methodist Episcopal"] = 1
Amir$Episcopalian[!(Amir$religion == "Episcopalian" | Amir$religion == "Episcopal" | Amir$religion == "African Methodist Episcopal")] = 0

Amir$L.D.S. <- NULL
Amir$L.D.S.[Amir$religion == "L.D.S." | Amir$religion == "L.D.S. (Mormon)" | Amir$religion == "LDS" | Amir$religion == "Mormon" ] = 1
Amir$L.D.S.[!(Amir$religion == "L.D.S." | Amir$religion == "L.D.S. (Mormon)" | Amir$religion == "LDS" | Amir$religion == "Mormon")] = 0


Amir$Unitarian <- NULL
Amir$Unitarian[Amir$religion == "Unitarian" | Amir$religion == "Unitarian Universalist" | Amir$religion == "Unitarian-Universalist" | Amir$religion == "Unitarian/Universalist" ] = 1
Amir$Unitarian[!(Amir$religion == "Unitarian" | Amir$religion == "Unitarian Universalist" | Amir$religion == "Unitarian-Universalist" | Amir$religion == "Unitarian/Universalist")] = 0

Amir$Agnostic <- NULL
Amir$Agnostic[Amir$religion == "Agnostic" ] = 1
Amir$Agnostic[!(Amir$religion == "Agnostic" )] = 0

Amir$Atheist <- NULL
Amir$Atheist[Amir$religion == "Atheist" | Amir$religion == "None"] = 1
Amir$Atheist[!(Amir$religion == "Atheist" | Amir$religion == "None")] = 0

Amir$Muslim <- NULL
Amir$Muslim[Amir$religion == "Muslim" | Amir$religion == "Islam"] = 1
Amir$Muslim[!(Amir$religion == "Muslim" | Amir$religion == "Islam")] = 0

Amir$Buddhist <- NULL
Amir$Buddhist[Amir$religion == "Buddhist" | Amir$religion == "Buddhism"] = 1
Amir$Buddhist[!(Amir$religion == "Buddhist" | Amir$religion == "Buddhism")] = 0


Amir$Christian <- NULL
Amir$Christian[Amir$religion == "Christian" |Amir$religion == "Catholic" | Amir$religion=="Roman Catholic" | Amir$religion == "Christian-Catholic" 
| Amir$religion=="Catholic/Christian" |Amir$religion == "Baptist" | Amir$religion=="Southern Baptist" | Amir$religion == "Christian (Baptist)" | 
Amir$religion=="Missionary Baptist"| Amir$religion=="Christian-Baptist" | Amir$religion == "American Baptist" |
Amir$religion=="First Baptist"| Amir$religion == "Independent Baptist" | Amir$religion=="Baptist/Christian"| 
Amir$religion=="Christian - Baptist" | Amir$religion=="Free Will Baptist"| Amir$religion=="Protestant (Baptist)" |Amir$religion == "Methodist" | Amir$religion=="United Methodist"  | 
Amir$religion=="United Methodist Church"| Amir$religion=="First United Methodist Church" | Amir$religion == "Methodist (United)"|
 Amir$religion == "Christian Methodist"| Amir$religion=="Free Methodist" | Amir$religion=="Christian--United Methodist" |
 Amir$religion=="Christian - Methodist" | Amir$religion=="Protestant (Methodist)"|Amir$religion == "Protestant" | Amir$religion=="Protestant Christian" | Amir$religion == "Protestant (United Church of Christ)" |
 Amir$religion=="Protestant-Presbyterian" | Amir$religion=="Presbyterian" | Amir$religion == "First Presbyterian Church" | Amir$religion == "Christian (Presbyterian)"|Amir$religion == "Lutheran" | Amir$religion=="Christian (Lutheran)" 
 | Amir$religion == "Lutheran (ELCA)" | Amir$religion=="Lutheran Church--Missouri Synod"|Amir$religion == "Episcopalian" | Amir$religion == "Episcopal" 
 | Amir$religion == "African Methodist Episcopal"|Amir$religion == "L.D.S." | Amir$religion == "L.D.S. (Mormon)" | Amir$religion == "LDS" 
 | Amir$religion == "Mormon" |Amir$religion == "Unitarian" | Amir$religion == "Unitarian Universalist" | Amir$religion == "Unitarian-Universalist" |
 Amir$religion == "Unitarian/Universalist" ] = 1

 Amir$Christian[!(Amir$religion == "Christian" |Amir$religion == "Catholic" | Amir$religion=="Roman Catholic" | Amir$religion == "Christian-Catholic" 
| Amir$religion=="Catholic/Christian" |Amir$religion == "Baptist" | Amir$religion=="Southern Baptist" | Amir$religion == "Christian (Baptist)" | 
Amir$religion=="Missionary Baptist"| Amir$religion=="Christian-Baptist" | Amir$religion == "American Baptist" |
Amir$religion=="First Baptist"| Amir$religion == "Independent Baptist" | Amir$religion=="Baptist/Christian"| 
Amir$religion=="Christian - Baptist" | Amir$religion=="Free Will Baptist"| Amir$religion=="Protestant (Baptist)" |Amir$religion == "Methodist" | Amir$religion=="United Methodist"  | 
Amir$religion=="United Methodist Church"| Amir$religion=="First United Methodist Church" | Amir$religion == "Methodist (United)"|
 Amir$religion == "Christian Methodist"| Amir$religion=="Free Methodist" | Amir$religion=="Christian--United Methodist" |
 Amir$religion=="Christian - Methodist" | Amir$religion=="Protestant (Methodist)"|Amir$religion == "Protestant" | Amir$religion=="Protestant Christian" | Amir$religion == "Protestant (United Church of Christ)" |
 Amir$religion=="Protestant-Presbyterian" | Amir$religion=="Presbyterian" | Amir$religion == "First Presbyterian Church" | Amir$religion == "Christian (Presbyterian)"|Amir$religion == "Lutheran" | Amir$religion=="Christian (Lutheran)" 
 | Amir$religion == "Lutheran (ELCA)" | Amir$religion=="Lutheran Church--Missouri Synod"|Amir$religion == "Episcopalian" | Amir$religion == "Episcopal" 
 | Amir$religion == "African Methodist Episcopal"|Amir$religion == "L.D.S." | Amir$religion == "L.D.S. (Mormon)" | Amir$religion == "LDS" 
 | Amir$religion == "Mormon" |Amir$religion == "Unitarian" | Amir$religion == "Unitarian Universalist" | Amir$religion == "Unitarian-Universalist" |
 Amir$religion == "Unitarian/Universalist") ] = 0


##Introduce the dummy variable for marerial status
Amir$mar <- ifelse(grepl("Wife", Amir$family), "1", "0")
Amir$mar <- ifelse(grepl("Husband", Amir$family), "1", "0")

#Introduce the dummy variables for the level of eduactions
Amir$BA <- NULL
Amir$BA[Amir$degree == "BA" | Amir$degree == "BS"  | Amir$degree == "BBA" | Amir$degree == "Graduated"  | Amir$degree == "Bachelors" 
| Amir$degree == "AB" | Amir$degree == "BFA" | Amir$degree == "BSBA" | Amir$degree == "BSEE"
| Amir$degree == "BSN" | Amir$degree == "BSE" | Amir$degree == "BS/BA" | Amir$degree == "BGS"
| Amir$degree == "BSCE" | Amir$degree == "BSME" | Amir$degree == "BAS" | Amir$degree == "BASc"
| Amir$degree == "ADN" | Amir$degree == "BME" | Amir$degree == "BSW" | Amir$degree == "BA/BS"
| Amir$degree == "BSC" | Amir$degree == "CFP" | Amir$degree == "Bachelor<U+0092>s" | Amir$degree == "Bachelor of Arts"
| Amir$degree == "BM" | Amir$degree == "LPN" | Amir$degree == "Bachelors Degree" | Amir$degree == "BSc"| Amir$degree == "CPA"] = 1

Amir$BA[!(Amir$degree == "BA" | Amir$degree == "BS"  | Amir$degree == "BBA" | Amir$degree == "Graduated"  | Amir$degree == "Bachelors" 
| Amir$degree == "AB" | Amir$degree == "BFA" | Amir$degree == "BSBA" | Amir$degree == "BSEE"
| Amir$degree == "BSN" | Amir$degree == "BSE" | Amir$degree == "BS/BA" | Amir$degree == "BGS"
| Amir$degree == "BSCE" | Amir$degree == "BSME" | Amir$degree == "BAS" | Amir$degree == "BASc"
| Amir$degree == "ADN" | Amir$degree == "BME" | Amir$degree == "BSW" | Amir$degree == "BA/BS"
| Amir$degree == "BSC" | Amir$degree == "CFP" | Amir$degree == "Bachelor<U+0092>s" | Amir$degree == "Bachelor of Arts"
| Amir$degree == "BM" | Amir$degree == "LPN" | Amir$degree == "Bachelors Degree" | Amir$degree == "BSc"| Amir$degree == "CPA")] = 0


Amir$MS <- NULL
Amir$MS[Amir$degree == "MA" | Amir$degree == "MBA"  | Amir$degree == "MS" | Amir$degree == "Masters"  | Amir$degree == "MPA" 
| Amir$degree == "Graduate" | Amir$degree == "LLB" | Amir$degree == "LLM" | Amir$degree == "EdD"
| Amir$degree == "MSW" | Amir$degree == "MFA" | Amir$degree == "MAT" | Amir$degree == "MPH"
| Amir$degree == "EdS" | Amir$degree == "RN" | Amir$degree == "MPP" | Amir$degree == "Graduate Work"
| Amir$degree == "Graduate Studies" | Amir$degree == "MM" | Amir$degree == "MSE" | Amir$degree == "ME"
| Amir$degree == "MHA" | Amir$degree == "MLS" | Amir$degree == "MSN" | Amir$degree == "MSEE"
| Amir$degree == "MST" | Amir$degree == "MSA" | Amir$degree == "MSEd" | Amir$degree == "MLA"| Amir$degree == "MSc"
| Amir$degree == "CAGS"| Amir$degree == "MAS" | Amir$degree == "MPS"] = 1

Amir$MS[!(Amir$degree == "MA" | Amir$degree == "MBA"  | Amir$degree == "MS" | Amir$degree == "Masters"  | Amir$degree == "MPA" 
| Amir$degree == "Graduate" | Amir$degree == "LLB" | Amir$degree == "LLM" | Amir$degree == "EdD"
| Amir$degree == "MSW" | Amir$degree == "MFA" | Amir$degree == "MAT" | Amir$degree == "MPH"
| Amir$degree == "EdS" | Amir$degree == "RN" | Amir$degree == "MPP" | Amir$degree == "Graduate Work"
| Amir$degree == "Graduate Studies" | Amir$degree == "MM" | Amir$degree == "MSE" | Amir$degree == "ME"
| Amir$degree == "MHA" | Amir$degree == "MLS" | Amir$degree == "MSN" | Amir$degree == "MSEE"
| Amir$degree == "MST" | Amir$degree == "MSA" | Amir$degree == "MSEd" | Amir$degree == "MLA"| Amir$degree == "MSc"
| Amir$degree == "CAGS"| Amir$degree == "MAS" | Amir$degree == "MPS")] = 0

Amir$PHD <- NULL
Amir$PHD[Amir$degree == "PhD" | Amir$degree == "Doctorate"  | Amir$degree == "DMin" | Amir$degree == "ABD"  | Amir$degree == "DC" 
| Amir$degree == "LLD" | Amir$degree == "Phd" | Amir$degree == "Ded" ] = 1

Amir$PHD[!(Amir$degree == "PhD" | Amir$degree == "Doctorate"  | Amir$degree == "DMin" | Amir$degree == "ABD"  | Amir$degree == "DC" 
| Amir$degree == "LLD" | Amir$degree == "Phd" | Amir$degree == "Ded") ] = 0

Amir$MD <- NULL
Amir$MD[Amir$degree == "MD" | Amir$degree == "Med"  | Amir$degree == "MDiv" | Amir$degree == "DDS"  | Amir$degree == "DVM" 
| Amir$degree == "MED" | Amir$degree == "OD" | Amir$degree == "DMD" ] = 1

Amir$MD[!(Amir$degree == "MD" | Amir$degree == "Med"  | Amir$degree == "MDiv" | Amir$degree == "DDS"  | Amir$degree == "DVM" 
| Amir$degree == "MED" | Amir$degree == "OD" | Amir$degree == "DMD" )] = 0

Amir$JD <- NULL
Amir$JD[Amir$degree == "JD" ] = 1
Amir$JD[!(Amir$degree == "JD" )] = 0



Amir$ASS <- NULL
Amir$ASS[Amir$degree == "Attended" | Amir$degree == "AA"  | Amir$degree == "Attending" | Amir$degree == "AS"  | Amir$degree == "Certificate" 
| Amir$degree == "Associates" | Amir$degree == "AAS" | Amir$degree == "Certified" | Amir$degree == "Associate's"
| Amir$degree == "AD" | Amir$degree == "Certification" | Amir$degree == "Associates Degree" | Amir$degree == "CLU"
| Amir$degree == "Attends" | Amir$degree == "Degree" ] = 1

Amir$ASS[!(Amir$degree == "Attended" | Amir$degree == "AA"  | Amir$degree == "Attending" | Amir$degree == "AS"  | Amir$degree == "Certificate" 
| Amir$degree == "Associates" | Amir$degree == "AAS" | Amir$degree == "Certified" | Amir$degree == "Associate's"
| Amir$degree == "AD" | Amir$degree == "Certification" | Amir$degree == "Associates Degree" | Amir$degree == "CLU"
| Amir$degree == "Attends" | Amir$degree == "Degree" )] = 0


Amir$Diploma <- NULL
Amir$Diploma[Amir$degree == "Diploma" | Amir$degree == "GED"  ] = 1
Amir$Diploma[!(Amir$degree == "Diploma" | Amir$degree == "GED" ) ] = 0



##Introduce the variable of years of schooling
Amir$years <- NULL
Amir$years[Amir$degree == "Diploma" | Amir$degree == "GED"  ] = 12

Amir$years[Amir$degree == "Attended" | Amir$degree == "AA"  | Amir$degree == "Attending" | Amir$degree == "AS"  | Amir$degree == "Certificate" 
| Amir$degree == "Associates" | Amir$degree == "AAS" | Amir$degree == "Certified" | Amir$degree == "Associate's"
| Amir$degree == "AD" | Amir$degree == "Certification" | Amir$degree == "Associates Degree" | Amir$degree == "CLU"
| Amir$degree == "Attends" | Amir$degree == "Degree" ] = 14

Amir$years[Amir$degree == "JD" ] = 19

Amir$years[Amir$degree == "MD" | Amir$degree == "Med"  | Amir$degree == "MDiv" | Amir$degree == "DDS"  | Amir$degree == "DVM" 
| Amir$degree == "MED" | Amir$degree == "OD" | Amir$degree == "DMD" ] = 23


Amir$years[Amir$degree == "MA" | Amir$degree == "MBA"  | Amir$degree == "MS" | Amir$degree == "Masters"  | Amir$degree == "MPA" 
| Amir$degree == "Graduate" | Amir$degree == "LLB" | Amir$degree == "LLM" | Amir$degree == "EdD"
| Amir$degree == "MSW" | Amir$degree == "MFA" | Amir$degree == "MAT" | Amir$degree == "MPH"
| Amir$degree == "EdS" | Amir$degree == "RN" | Amir$degree == "MPP" | Amir$degree == "Graduate Work"
| Amir$degree == "Graduate Studies" | Amir$degree == "MM" | Amir$degree == "MSE" | Amir$degree == "ME"
| Amir$degree == "MHA" | Amir$degree == "MLS" | Amir$degree == "MSN" | Amir$degree == "MSEE"
| Amir$degree == "MST" | Amir$degree == "MSA" | Amir$degree == "MSEd" | Amir$degree == "MLA"| Amir$degree == "MSc"
| Amir$degree == "CAGS"| Amir$degree == "MAS" | Amir$degree == "MPS"] =18

Amir$years[Amir$degree == "BA" | Amir$degree == "BS"  | Amir$degree == "BBA" | Amir$degree == "Graduated"  | Amir$degree == "Bachelors" 
| Amir$degree == "AB" | Amir$degree == "BFA" | Amir$degree == "BSBA" | Amir$degree == "BSEE"
| Amir$degree == "BSN" | Amir$degree == "BSE" | Amir$degree == "BS/BA" | Amir$degree == "BGS"
| Amir$degree == "BSCE" | Amir$degree == "BSME" | Amir$degree == "BAS" | Amir$degree == "BASc"
| Amir$degree == "ADN" | Amir$degree == "BME" | Amir$degree == "BSW" | Amir$degree == "BA/BS"
| Amir$degree == "BSC" | Amir$degree == "CFP" | Amir$degree == "Bachelor<U+0092>s" | Amir$degree == "Bachelor of Arts"
| Amir$degree == "BM" | Amir$degree == "LPN" | Amir$degree == "Bachelors Degree" | Amir$degree == "BSc"| Amir$degree == "CPA"] = 16


Amir$years[Amir$degree == "PhD" | Amir$degree == "Doctorate"  | Amir$degree == "DMin" | Amir$degree == "ABD"  | Amir$degree == "DC" 
| Amir$degree == "LLD" | Amir$degree == "Phd" | Amir$degree == "Ded" ] = 21




#Introduce a categorical variable for eduacation
Amir$deg <- NULL
Amir$deg[Amir$degree == "Diploma" | Amir$degree == "GED"  ] = 1

Amir$deg[Amir$degree == "Attended" | Amir$degree == "AA"  | Amir$degree == "Attending" | Amir$degree == "AS"  | Amir$degree == "Certificate" 
| Amir$degree == "Associates" | Amir$degree == "AAS" | Amir$degree == "Certified" | Amir$degree == "Associate's"
| Amir$degree == "AD" | Amir$degree == "Certification" | Amir$degree == "Associates Degree" | Amir$degree == "CLU"
| Amir$degree == "Attends" | Amir$degree == "Degree" ] = 2

Amir$deg[Amir$degree == "JD" ] = 5

Amir$deg[Amir$degree == "MD" | Amir$degree == "Med"  | Amir$degree == "MDiv" | Amir$degree == "DDS"  | Amir$degree == "DVM" 
| Amir$degree == "MED" | Amir$degree == "OD" | Amir$degree == "DMD" ] = 7


Amir$deg[Amir$degree == "MA" | Amir$degree == "MBA"  | Amir$degree == "MS" | Amir$degree == "Masters"  | Amir$degree == "MPA" 
| Amir$degree == "Graduate" | Amir$degree == "LLB" | Amir$degree == "LLM" | Amir$degree == "EdD"
| Amir$degree == "MSW" | Amir$degree == "MFA" | Amir$degree == "MAT" | Amir$degree == "MPH"
| Amir$degree == "EdS" | Amir$degree == "RN" | Amir$degree == "MPP" | Amir$degree == "Graduate Work"
| Amir$degree == "Graduate Studies" | Amir$degree == "MM" | Amir$degree == "MSE" | Amir$degree == "ME"
| Amir$degree == "MHA" | Amir$degree == "MLS" | Amir$degree == "MSN" | Amir$degree == "MSEE"
| Amir$degree == "MST" | Amir$degree == "MSA" | Amir$degree == "MSEd" | Amir$degree == "MLA"| Amir$degree == "MSc"
| Amir$degree == "CAGS"| Amir$degree == "MAS" | Amir$degree == "MPS"] =4

Amir$deg[Amir$degree == "BA" | Amir$degree == "BS"  | Amir$degree == "BBA" | Amir$degree == "Graduated"  | Amir$degree == "Bachelors" 
| Amir$degree == "AB" | Amir$degree == "BFA" | Amir$degree == "BSBA" | Amir$degree == "BSEE"
| Amir$degree == "BSN" | Amir$degree == "BSE" | Amir$degree == "BS/BA" | Amir$degree == "BGS"
| Amir$degree == "BSCE" | Amir$degree == "BSME" | Amir$degree == "BAS" | Amir$degree == "BASc"
| Amir$degree == "ADN" | Amir$degree == "BME" | Amir$degree == "BSW" | Amir$degree == "BA/BS"
| Amir$degree == "BSC" | Amir$degree == "CFP" | Amir$degree == "Bachelor<U+0092>s" | Amir$degree == "Bachelor of Arts"
| Amir$degree == "BM" | Amir$degree == "LPN" | Amir$degree == "Bachelors Degree" | Amir$degree == "BSc"| Amir$degree == "CPA"] = 3


Amir$deg[Amir$degree == "PhD" | Amir$degree == "Doctorate"  | Amir$degree == "DMin" | Amir$degree == "ABD"  | Amir$degree == "DC" 
| Amir$degree == "LLD" | Amir$degree == "Phd" | Amir$degree == "Ded" ] = 6


#Load the new data set and merge it to the existiong one
y <- merge(Book1, Amir, by.x = "state", by.y = "homeState")
Book1 <- read.csv("C:/Users/Amir/Desktop/cities.csv")



##################################
#Anliyze the Data                #
##################################


#Map the States in which the Dual-employment law is in effect
c=c("ARIZONA", "ALASKA","ALABAMA","ARIZONA","CALIFORNIA", "CONNECTICUT", "GEORGIA", "KENTUCKY", "LOUSIANA", "MARYLAND", "MASSACHUSETTS", "MISSOURI", "MICHIGAN", "NEW JERSEY" , "OHIO", "WEST VIRGINIA" )
map(database = "state")
map(database = "state",regions = c,col = "blue",fill=T,add=TRUE)

#Summary Statistics
stargazer(total, nobs = FALSE, mean.sd = TRUE, median = FALSE,
+           iqr = FALSE, p75= FALSE, p75= FALSE)


#Estimations
mylogit1 <- glm(JD ~ as.factor(resigntorun) +as.factor(genderr)+ as.numeric(popmale) + popfem + White + Black +NativeIn + NativeHa + Asian + as.factor(party) + as.factor(mar) + as.numeric(LFP) + as.numeric(UR) + as.numeric(MeanInc) + as.numeric(MedianInc) + as.numeric(Poverty) + first.high +first.dip + first.some + first.BA + second.high + second.some + second.ass + second.BA + second.Grad + as.numeric(ag) , data = total, family = "binomial") mymodel <- lm( as.numeric(ag) ~ as.factor(resigntorun) +as.factor(genderr)+ as.numeric(popmale) + popfem + White + Black +NativeIn + NativeHa + Asian + as.factor(party) + as.factor(mar) + as.numeric(LFP) + as.numeric(UR) + as.numeric(MeanInc) + as.numeric(MedianInc) + as.numeric(Poverty) + first.high +first.dip + first.some + first.BA + second.high + second.some + second.ass + second.BA + second.Grad  , data = city)
mymodel1 <- lm( as.numeric(years) ~ as.factor(resigntorun) +as.factor(genderr)+ as.numeric(popmale) + popfem + White + Black +NativeIn + NativeHa + Asian + as.factor(party) + as.factor(mar) + as.numeric(LFP) + as.numeric(UR) + as.numeric(MeanInc) + as.numeric(MedianInc) + as.numeric(Poverty) + first.high +first.dip + first.some + first.BA + second.high + second.some + second.ass + second.BA + second.Grad  , data = total)
mylogit <- glm(PHD ~ as.factor(resigntorun) +as.factor(genderr)+ as.numeric(popmale) + popfem + White + Black +NativeIn + NativeHa + Asian + as.factor(party) + as.factor(mar) + as.numeric(LFP) + as.numeric(UR) + as.numeric(MeanInc) + as.numeric(MedianInc) + as.numeric(Poverty) + first.high +first.dip + first.some + first.BA + second.high + second.some + second.ass + second.BA + second.Grad + as.numeric(ag) , data = total, family = "binomial") mymodel <- lm( as.numeric(ag) ~ as.factor(resigntorun) +as.factor(genderr)+ as.numeric(popmale) + popfem + White + Black +NativeIn + NativeHa + Asian + as.factor(party) + as.factor(mar) + as.numeric(LFP) + as.numeric(UR) + as.numeric(MeanInc) + as.numeric(MedianInc) + as.numeric(Poverty) + first.high +first.dip + first.some + first.BA + second.high + second.some + second.ass + second.BA + second.Grad  , data = city)
mymodel <- lm( as.numeric(ag) ~ as.factor(resigntorun) +as.factor(genderr)+ as.numeric(popmale) + popfem + White + Black +NativeIn + NativeHa + Asian + as.factor(party) + as.factor(mar) + as.numeric(LFP) + as.numeric(UR) + as.numeric(MeanInc) + as.numeric(MedianInc) + as.numeric(Poverty) + first.high +first.dip + first.some + first.BA + second.high + second.some + second.ass + second.BA + second.Grad  , data = total)



stargazer(mylogit,mylogit1, mymodel, mymodel1)







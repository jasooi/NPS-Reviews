library(data.table)
library(cld2)
library(cld3)
library(rlist)
setwd("C://Users//Jasmine.Ooi//Downloads")
sg_18 <- fread("sg_18.csv")
sg_19 <- fread("sg_19.csv")
my_19 <- fread("my_19.csv")
summary(sg_18)
summary(sg_19)
summary(my_19)

##remove NA rows
sg_18 <- sg_18[!is.na(tracking_number)]
sg_19 <- sg_19[!(issue_1 == "")]
my_19 <- my_19[!(tracking_number == "")]

##additional cleaning of dirty cols
#for easy searching:
# searchphrase <- "good"
# sample <- my_19[issue_1 == searchphrase| issue_2 ==searchphrase| issue_3 == searchphrase]


##aligning issue mapping
lvls_sg_18 <- unique(sg_18[,c(issue_1,issue_2,issue_3)])
lvls_sg_19 <- unique(sg_19[,c(issue_1,issue_2,issue_3)])
lvls_my_19 <- unique(my_19[,c(issue_1,issue_2,issue_3)])
for (i in list(lvls_my_19,lvls_sg_19,lvls_sg_18)){
  print(i)
  print("--------------")
}

sg_19 <- sg_19[,country:="sg"]
sg_18 <- sg_18[,country:= "sg"]
my_19 <- my_19[,country:= "my"]
masterdata <- rbind(sg_18,sg_19,my_19)
lvls_names <- c("issue_1", "issue_2", "issue_3")
ln <- function(i){paste0(lvls_names[i],"_new")}
for (i in 1:3){
  
  masterdata <- masterdata[get(lvls_names[i]) %in% c("Driver - Driver could not find address","Driver - Did not deliver to doorstep","Driver - Delivery to wrong address","Deliver To Wrong Address","Did Not Deliver To Doorstep"),(ln(i)):="D1"]
  masterdata <- masterdata[get(lvls_names[i]) %in% c("Driver - Fake success/item not received","FakeSuccess/Item Not Received"),(ln(i)):="D2"]
  masterdata <- masterdata[get(lvls_names[i]) %in% c("Driver - Forgery","Forgery","Driver - No call before failing","Driver - No call before Failing","Fake Fail" ,"No Call Before Failing"),(ln(i)):="D3"]
  masterdata <- masterdata[get(lvls_names[i]) %in% c("Driver - Dumping","Driver - Rude/Poor attitude","Dumping","Rude/Poor attitude","Driver - Unprofessionalism"),(ln(i)):="D4"]
  masterdata <- masterdata[get(lvls_names[i]) %in% c("Driver - No call before OFD","No Call Before Out For Delivery","Driver - No picture of goods taken"),(ln(i)):="D5"]
  masterdata <- masterdata[get(lvls_names[i]) %in% c("Driver - Payment issue/Not returning balance","Payment"),(ln(i)):="D6"]
  
  masterdata <- masterdata[get(lvls_names[i]) %in% c("Service quality - Poor communication","Service quality - Customer service","Customer Service Agent","Poor Communication"),(ln(i)):="P1"]
  masterdata <- masterdata[get(lvls_names[i]) %in% c("Driver / Ops - Damage","Damaged/lost"),(ln(i)):="P2"]
  masterdata <- masterdata[get(lvls_names[i]) %in% c("Process - Parcel tracking","No Tracking ID","No Tracking Number","No TN#" ,"Technical issue (TN#/website/notification)" ,"Process - No ETA","No Timeslot"),(ln(i)):="P3"]
  masterdata <- masterdata[get(lvls_names[i]) %in% c("SLA - Late/slow/did not deliver within time slot" ,"Late/slow/did not deliver within timeslot/CMI","SLA - Late/slow/did not deliver within timeslot","SLA - Late/slow/did not deliverwithin timeslot","Pull out/Lost(Hubs)","Routed But Not Delivered"),(ln(i)):="P4"]
  masterdata <- masterdata[get(lvls_names[i]) %in% c("PUDO - Ninja Point","PUDO"),(ln(i)):="P5"]
  masterdata <- masterdata[get(lvls_names[i]) %in% c("Process - Reschedule w/o reason","Process - unable to make preferred arrangements","Process - Switched from delivery to self-collection"),(ln(i)):="P6"]
  
  masterdata <- masterdata[get(lvls_names[i]) %in% c("Seller Issue", "Invalid - Seller issue"),(ln(i)):="N0"]
  
  masterdata <- masterdata[get(lvls_names[i]) %in% c("Wrongly Rate", "Invalid - Wrongly rate/Positive"),(ln(i)):="W0"]
  
  masterdata <- masterdata[get(lvls_names[i]) %in% c("Invalid - Invalid complaint", "Others - Others", "Others - No comment", "No Comment", "Invalid"),(ln(i)):="I0"]
}

#remove the one NA row
masterdata <- masterdata[!(issue_1=="")]

#Remove rows where feedback do not contain alphanumeric chars
masterdata <- masterdata[grep(pattern = "[[:alnum:]].", x = masterdata$feedback, value = F)]
masterdata <- masterdata[, feedback := tolower(feedback)]

#Split data into only 1 issue, >1 issue
single_issue <- masterdata[!is.na(issue_1_new) & is.na(issue_2_new) & is.na(issue_3_new)]
multi_issue <- masterdata[!is.na(issue_1_new) & !is.na(issue_2_new)]

#Export to CSV for transfer to jupyter notebook
fwrite(single_issue, file = "C://Users//Jasmine.Ooi//Desktop//single_issue.csv")
fwrite(multi_issue, file = "C://Users//Jasmine.Ooi//Desktop//multi_issue.csv")


# masterdata$feedback <- gsub(pattern = "[\\?-\\$-\\&-\\.-\\)-\\(-\\/-\\:-\\-]",replacement = " ",x = masterdata$feedback)
view3 <- single_issue[grep(pattern = "&",x = single_issue$feedback,value =F, fixed=F)]


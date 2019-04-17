
# Define modifying function
# takes as input a file name, variable name, and split character and
# splits ID (study id) into parts and keeps only the relevant segment

# ex. df = modStudyID('InputFile.csv','StudyID',"-");
modStudyID <- function(inFile,ID,splitter) {
  
  df = read.csv(inFile,header=TRUE,stringsAsFactors = FALSE);
  splStudyID = strsplit(as.character(df[,ID]),splitter);
  df[,ID] = as.factor(sapply(splStudyID,"[",1));
  
  vs = list(df,splStudyID);
  return(vs);
  
  #return(df);
  
}

get_K_ID <- function(inList,k) {
  df = sapply(inList,"[",k);
  return(df);
}

get_ID_Parts <- function(inList) {
  df = sapply(inList,"[");
  return(df);
}

# get file names
Res = '/Users/oldmac/Desktop/EPI/TBRU & NK Helminth Results 2016_DEC_19.csv';  #882 is weird, 930 is short
QFT = '/Users/oldmac/Desktop/EPI/TBRU & NK QFT Results_2016_DEC_19.csv';

# modify StudyIDs to get rid of extraneous info
Res_Vars = modStudyID(Res,'StudyID',"-");
modRes = do.call("rbind",Res_Vars[1]);
modRes$SecChar = get_K_ID(do.call("rbind",Res_Vars[2]),2);

QFT_Vars = modStudyID(QFT,'StudyID','-');
modQFT = do.call("rbind", QFT_Vars[1]);
modQFT$SecChar = get_K_ID(do.call("rbind", QFT_Vars[2]),2)


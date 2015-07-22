# Define classifier models
# 
# WARNING: assume "ClassifierFunctions.R" is source already
###############################################################################

# some constants ...
MMR.VAR.NAME <- "MMR.IHC.4" # define the MMR variable to use

CLASS.NAME.MMR.POLE.P53MUT <- "MMR/POLE mut/p53 mut"
CLASS.NAME.MMR.POLE.FISH1  <- "MMR/POLE mut/Fish (threshold 1)"
CLASS.NAME.MMR.POLE.FISH2  <- "MMR/POLE mut/Fish (threshold 2)"

CLASS.NAME.MMR.POLE.PTEN.P53MUT <- "MMR/POLE+PTEN mut/p53 mut"
CLASS.NAME.MMR.POLE.PTEN.FISH1  <- "MMR/POLE+PTEN mut/Fish (threshold 1)"
CLASS.NAME.MMR.POLE.PTEN.FISH2  <- "MMR/POLE+PTEN mut/Fish (threshold 2)"

CLASS.NAME.POLE.PTEN.MMR.P53MUT <- "POLE+PTEN mut/MMR/p53 mut"
CLASS.NAME.POLE.MMR.P53MUT      <- "POLE mut/MMR/p53 mut"

CLASS.NAME.POLE.PTEN.MMR.P53IHC <- "POLE+PTEN mut/MMR/p53 IHC"
CLASS.NAME.POLE.MMR.P53IHC     <- "POLE mut/MMR/p53 IHC"

CLASS.NAME.MMR.POLE.PTEN.P53IHC <- "MMR/POLE+PTEN mut/p53 IHC"
CLASS.NAME.MMR.POLE.P53IHC     <- "MMR/POLE mut/p53 IHC"

CLASS.NAME.CLINICAL <- ""

# mapping of class names i.e. descriptions and variable names
# please note, this variable (CLASS.NAMES) also defines
# the order of the models appearing in the remark document
CLASS.NAMES <- c(
#	"class.pole.mmr.p53mut", CLASS.NAME.POLE.MMR.P53MUT,
	"class.mmr.pole.p53mut", CLASS.NAME.MMR.POLE.P53MUT,
	
	"class.mmr.pole.fish1",  CLASS.NAME.MMR.POLE.FISH1,
	"class.mmr.pole.fish2",  CLASS.NAME.MMR.POLE.FISH2,
	
	"class.mmr.pole.pten.p53mut", CLASS.NAME.MMR.POLE.PTEN.P53MUT,
	"class.mmr.pole.pten.fish1",  CLASS.NAME.MMR.POLE.PTEN.FISH1,
	"class.mmr.pole.pten.fish2",  CLASS.NAME.MMR.POLE.PTEN.FISH2,
	
#	"class.pole.pten.mmr.p53mut", CLASS.NAME.POLE.PTEN.MMR.P53MUT,

	
#	"class.pole.pten.mmr.p53ihc", CLASS.NAME.POLE.PTEN.MMR.P53IHC,
#	"class.pole.mmr.p53ihc",      CLASS.NAME.POLE.MMR.P53IHC,
	
	"class.mmr.pole.pten.p53ihc", CLASS.NAME.MMR.POLE.PTEN.P53IHC,
	
	"class.mmr.pole.p53ihc", CLASS.NAME.MMR.POLE.P53IHC
)
dim(CLASS.NAMES) <- c(2,length(CLASS.NAMES)/2)
CLASS.NAMES <- t(CLASS.NAMES)

CLASS.CHOSEN <- CLASS.NAMES[CLASS.NAMES[,2]==CLASS.NAME.MMR.POLE.P53IHC,1]
CLASS.CHOSEN.NAME <- CLASS.NAMES[CLASS.NAMES[,1]==CLASS.CHOSEN,2]

# set all missing value to NA because EndSub and EndSub expects input to be TRUE/FALSE/NA
temp.d <- emdb
temp.d$POLE.mut.consolidated.numeric[temp.d$POLE.mut.consolidated.numeric %in% ALL.MISSING.CODES] <- NA
temp.d$PTEN.mut.consolidated.numeric[temp.d$PTEN.mut.consolidated.numeric %in% ALL.MISSING.CODES] <- NA
temp.d$MMR.IHC.4[                    temp.d$MMR.IHC.4                     %in% ALL.MISSING.CODES] <- NA
temp.d$TP53.mut.consolidated.numeric[temp.d$TP53.mut.consolidated.numeric %in% ALL.MISSING.CODES] <- NA
temp.d$p53.ihc.consolidated.numeric [temp.d$p53.ihc.consolidated.numeric  %in% ALL.MISSING.CODES] <- NA

## Scenario 1:MMR/Pole Mut/P53
class.mmr.pole.p53mut=EndSub(PolePhen=temp.d$POLE.mut.consolidated.numeric==1,MSIPhen=temp.d[,MMR.VAR.NAME]==VALUE.CODING.MMR.MSI.HIGH,CNHPhen=temp.d$TP53.mut.consolidated.numeric==1)
## Scenario 2:MMR/Pole Mut/FishT1
class.mmr.pole.fish1 =EndSub(PolePhen=temp.d$POLE.mut.consolidated.numeric==1,MSIPhen=temp.d[,MMR.VAR.NAME]==VALUE.CODING.MMR.MSI.HIGH,CNHPhen=temp.d$CN1==1)
## Scenario 3:MMR/Pole Mut/FishT2
class.mmr.pole.fish2 =EndSub(PolePhen=temp.d$POLE.mut.consolidated.numeric==1,MSIPhen=temp.d[,MMR.VAR.NAME]==VALUE.CODING.MMR.MSI.HIGH,CNHPhen=temp.d$CN2==1)


## Scenario 4:MMR/Pole+PTEN Mut/p53
class.mmr.pole.pten.p53mut=EndSub(PolePhen=(temp.d$POLE.mut.consolidated.numeric==1&temp.d$PTEN.mut.consolidated.numeric==1),MSIPhen=temp.d[,MMR.VAR.NAME]==VALUE.CODING.MMR.MSI.HIGH,CNHPhen=temp.d$TP53.mut.consolidated.numeric==1)
## Scenario 5:MMR/Pole+PTEN Mut/FishT1
class.mmr.pole.pten.fish1 =EndSub(PolePhen=(temp.d$POLE.mut.consolidated.numeric==1&temp.d$PTEN.mut.consolidated.numeric==1),MSIPhen=temp.d[,MMR.VAR.NAME]==VALUE.CODING.MMR.MSI.HIGH,CNHPhen=temp.d$CN1==1)
## Scenario 6:MMR/Pole+PTEN Mut/FishT2
class.mmr.pole.pten.fish2 =EndSub(PolePhen=(temp.d$POLE.mut.consolidated.numeric==1&temp.d$PTEN.mut.consolidated.numeric==1),MSIPhen=temp.d[,MMR.VAR.NAME]==VALUE.CODING.MMR.MSI.HIGH,CNHPhen=temp.d$CN2==1)

## Scenario 7: (Switching order) Pole+PTEN Mut/MMR/p53
class.pole.pten.mmr.p53mut=EndSub2(PolePhen=(temp.d$POLE.mut.consolidated.numeric==1&temp.d$PTEN.mut.consolidated.numeric==1),MSIPhen=temp.d[,MMR.VAR.NAME]==VALUE.CODING.MMR.MSI.HIGH,CNHPhen=temp.d$TP53.mut.consolidated.numeric==1)
## Scenario 8: (Switching order) Pole Mut/MMR/p53
class.pole.mmr.p53mut     =EndSub2(PolePhen=(temp.d$POLE.mut.consolidated.numeric==1),                                        MSIPhen=temp.d[,MMR.VAR.NAME]==VALUE.CODING.MMR.MSI.HIGH,CNHPhen=temp.d$TP53.mut.consolidated.numeric==1)

## Scenario 9: (Switching order) Pole+PTEN Mut/MMR/p53 IHC
class.pole.pten.mmr.p53ihc=EndSub2(PolePhen=(temp.d$POLE.mut.consolidated.numeric==1&temp.d$PTEN.mut.consolidated.numeric==1),MSIPhen=temp.d[,MMR.VAR.NAME]==VALUE.CODING.MMR.MSI.HIGH,CNHPhen=temp.d$p53.ihc.consolidated.numeric!=1)
## Scenario 10: (Switching order) Pole Mut/MMR/p53 IHC
class.pole.mmr.p53ihc     =EndSub2(PolePhen=(temp.d$POLE.mut.consolidated.numeric==1),                                        MSIPhen=temp.d[,MMR.VAR.NAME]==VALUE.CODING.MMR.MSI.HIGH,CNHPhen=temp.d$p53.ihc.consolidated.numeric!=1)

## Scenario 11: MMR/Pole+PTEN Mut/p53 IHC
class.mmr.pole.pten.p53ihc=EndSub(PolePhen=(temp.d$POLE.mut.consolidated.numeric==1&temp.d$PTEN.mut.consolidated.numeric==1),MSIPhen=temp.d[,MMR.VAR.NAME]==VALUE.CODING.MMR.MSI.HIGH,CNHPhen=temp.d$p53.ihc.consolidated.numeric!=1)
## Scenario 12: MMR/Pole/p53 IHC - WARNING: cannot use CNHPhen=temp.d$p53.ihc.consolidated.numeric %in% c(0,2) ... since this will take NA as FALSE!!!!!
class.mmr.pole.p53ihc     =EndSub(PolePhen=(temp.d$POLE.mut.consolidated.numeric==1),                                        MSIPhen=temp.d[,MMR.VAR.NAME]==VALUE.CODING.MMR.MSI.HIGH,CNHPhen=temp.d$p53.ihc.consolidated.numeric!=1)

# cbind all to emdb 
emdb <- cbind(
	emdb,
	class.mmr.pole.p53mut,class.mmr.pole.fish1,class.mmr.pole.fish2,
	class.mmr.pole.pten.p53mut,class.mmr.pole.pten.fish1,class.mmr.pole.pten.fish2,
	class.pole.pten.mmr.p53mut,class.pole.mmr.p53mut,
	class.pole.pten.mmr.p53ihc,class.pole.mmr.p53ihc,
	class.mmr.pole.pten.p53ihc,class.mmr.pole.p53ihc
)


# per Jessica email 2015-04-03 ... relabel class ...
#emdb[,CLASS.CHOSEN] <- sapply(emdb[,CLASS.CHOSEN],as.character)
#emdb[!is.na(emdb[,CLASS.CHOSEN]) & emdb[,CLASS.CHOSEN]==CLASS.NAME.MSI,CLASS.CHOSEN   ] <- "MMR IHC abn"
#emdb[!is.na(emdb[,CLASS.CHOSEN]) & emdb[,CLASS.CHOSEN]==CLASS.NAME.CNHIGH,CLASS.CHOSEN] <- "p53 abn"
#emdb[!is.na(emdb[,CLASS.CHOSEN]) & emdb[,CLASS.CHOSEN]==CLASS.NAME.CNLOW,CLASS.CHOSEN ] <- "p53 wt"
#emdb[,CLASS.CHOSEN] <- factor(emdb[,CLASS.CHOSEN],levels=c("POLE","MMR IHC abn","p53 wt","p53 abn"))
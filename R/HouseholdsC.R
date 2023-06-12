DEPMEN <- read_delim("Library/CloudStorage/OneDrive-Personal/Université/LMD/Master E&P/M1/Mémoire/Dissertation/Donnees/Budget fam.csv/data/DEPMEN.csv", 
                     +     delim = ";", escape_double = FALSE, trim_ws = TRUE)

#---------------------------------
#Data Cleaning and organisation script -Check Item dictionary for var names details
#---------------------------------

#Utilities 
#---------------------------------
DEPMEN$MFAC_EAU1_D
DEPMEN$MFAC_ELEC1_D
DEPMEN$MFAC_GAZ1_D
DEPMEN$MFAC_EG1_D

#Relative to housings
#---------------------------------
DEPMEN$MFON_D
DEPMEN$MHAB_D
DEPMEN$MLOY_D #Last rent paid
DEPMEN$MQUI_D #

# Relative to healthcare 
#---------------------------------

#
DEPMEN$MLAPPARL_D
DEPMEN$MOCHAUS_D
DEPMEN$MODENT_D
DEPMEN$MOLUNET_D


# First necessity goods
#---------------------------------

DEPMEN$MPANIER_ALIMENTAIRE_D

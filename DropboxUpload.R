#This needs to be run in the TERMINAL not in the CONSOLE (R)!!!
# The terminal is the tab next to the console (R)
# This will upload all the results to dropbox

# THIS IS NOT R CODE, THIS IS WHY IT NEEDS TO BE COPY/PASTED INTO THE 
# TERMINAL, NOT INTO THE CONSOLE
C:/rclone-v1.45/rclone.exe sync local:"/data processing/FAKE_DROPBOX/eRegistry CRCT Dropbox/Data management eRegQual/Results_From_PNIPH" dropbox:"/Data Management eRegQual/Results_From_PNIPH" --progress --timeout=1m

# You will need to kill it at sometime
# using "ctrl-C"
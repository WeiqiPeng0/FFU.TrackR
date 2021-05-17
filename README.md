# FFUTrackR
On-going project (Croker Lab)

FFU.TrackR is a R-Shiny based application to do fast data visualization, especially tailored for biomedical data formats.

USE:
1. Install Rstudio
2. Install dependent packages (RShiny, tidyverse, DT, etc.)
3. run "runApp()" in the directory

OR check out the online version at https://croker.shinyapps.io/ffutrackr/


<img src="demo1.png" alt="drawing" width="500"/> <img src="demo2.png" alt="drawing" width="400"/>


Structure:
 - www: dir that contains all css files
 - ui.R: the user interface
 - server.R: the backend file

Notice:
 - The input file need to contain a header which is a row of variable names
 - Any row that contains one or more NA shall be removed
 - Only numeric columns shall be selected for plotting
 - All statistics are by default calculted on filtered data
 - The input treatment names won't display unless the input string, if splitted by semicolons, matches current number of treatments.

For questions (or suggestions and improvements), please contact us bcroker@ucsd.edu or wep012@ucsd.edu. This is a work in progress, so we welcome your feedback!

# TableauCS
Coursera Tableau Data Visualization Repo

The data cleanliness process included many steps, performed in R. The data is from the Sao Paulo Public Safety Department and the range is from the years 1996 to 2016. In sequence I had to:
1) Import the html data, from url links
2) Extract the table objects from html
 2a) Manually correct the tables with different structures
3) Filter the relevant cols and rows
4) Clean the numeric fields (remove dots and special characters)
5) Clean and Conform the dimensions, as the statics names changed during the 21 years
6) I choose to select only the statics that they collect since 1996. Newer ones were filtered out
7) Translate the terms from Portuguese to English
8) Finally export the data to a csv file.

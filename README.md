Mass 211 Data Analysis Project
------------------------------

This is the repository for all code related to the Mass 211 Data Analysis, including R code for
data cleaning and transformation, intermediate exploratory reports, and auxliary data files collected
from different sources.

## File Structure

```
-|- data/     : scripts for scraping and preprocessing additional datasets
 |- src/      : R source code all data cleaning and analysis work
 |- reports/  : intermediate and final reports
 |- Mass211/  : a WIP Shiny App
 |- ref       : literature references
```

You'd also need to install [Git Large File Storage](https://git-lfs.github.com/) if you would like to work with the source data.

## Start Working..

1. Double-click `Mass 211.Rproj` to open the project in R Studio. It automatically sets the root folder of this repository as the working directory. All file paths in the source code assume you have set the working directory as such.
2. Run `src/index.R` to process all data files. Make sure 211 data exported from the iCarol system is placed in the correponding `data/211` folder.

## Related

The source code for the Mass 211 Map web app can be found at [ktmud/mass211-map](https://github.com/ktmud/mass211-map).

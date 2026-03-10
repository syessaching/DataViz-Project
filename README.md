💻 **Installation & Running the App**

1️⃣ **Install Required Packages**

  Open R or RStudio and run:
  
> install.packages(c(
  "shiny",
  "readxl",
  "dplyr",
  "stringr",
  "plotly",
  "tibble",
  "tidyr",
  "writexl",
  "later"
))

2️⃣ **Make Sure the Dataset Is in the Project Folder**

Place the file:
> NYPD_Hate_Crimes_20260128.xlsx

in the same folder as:
> app.R

3️⃣ **Run the Application**

In VSCode, run :
  > R

  then, run :

  > shiny::runApp()

Or if you are already inside the project folder, run :
  > R

  then, run :

  > shiny::runApp("app.R")

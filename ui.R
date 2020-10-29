
Basic DataTable
Manufacturer:
  All
Transmission:
  All
Cylinders:
  All
Show 
10
entriesSearch:
  manufacturer	model	displ	year	cyl	trans	drv	cty	hwy	fl	class
1	audi	a4	1.8	1999	4	auto(l5)	f	18	29	p	compact
2	audi	a4	1.8	1999	4	manual(m5)	f	21	29	p	compact
3	audi	a4	2	2008	4	manual(m6)	f	20	31	p	compact
4	audi	a4	2	2008	4	auto(av)	f	21	30	p	compact
5	audi	a4	2.8	1999	6	auto(l5)	f	16	26	p	compact
6	audi	a4	2.8	1999	6	manual(m5)	f	18	26	p	compact
7	audi	a4	3.1	2008	6	auto(av)	f	18	27	p	compact
8	audi	a4 quattro	1.8	1999	4	manual(m5)	4	18	26	p	compact
9	audi	a4 quattro	1.8	1999	4	auto(l5)	4	16	25	p	compact
10	audi	a4 quattro	2	2008	4	manual(m6)	4	20	28	p	compact
Showing 1 to 10 of 234 entriesPrevious12345…24Next
server.R
ui.R
# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

fluidPage(
  titlePanel("Basic DataTable"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("man",
                       "Manufacturer:",
                       c("All",
                         unique(as.character(mpg$manufacturer))))
    ),
    column(4,
           selectInput("trans",
                       "Transmission:",
                       c("All",
                         unique(as.character(mpg$trans))))
    ),
    column(4,
           selectInput("cyl",
                       "Cylinders:",
                       c("All",
                         unique(as.character(mpg$cyl))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table")
)

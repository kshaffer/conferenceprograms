# conferenceprograms
Mining and analyzing humanities conference program data

Plain text files scraped from the original PDF conference programs can be found in the `CCCC_programs` and `SMT_AMS_programs` folders. These are unstructured.

Structured data that has been extracted from the programs can be found in the `parsed_programs` folder. Some sample plots of preliminary analytical data can be found in `visualizations`.

There is an R file for each program type ― `cccc.R` and `smt_parse.R` ― which contains code to parse the data. (Note that each year's program is slightly different in its structure, so some massaging of functions is necessary to get it to work. In some cases, you will also need to edit the source text files.) In all cases, I deleted information before and after the program proper from the text file before parsing (table of contents, index, advertisements, etc.).

Code to analyze and compare structured output data from the programs can be found in `compare.R`.

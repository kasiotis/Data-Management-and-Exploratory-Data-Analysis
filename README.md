# Instruction Manual

To load this project, you'll first need to `setwd()` into the directory
where this README file is located. Then you need to run the following two
lines of R code:

	library('ProjectTemplate')
	load.project()

After you enter the second line of code, you'll see a series of automated
messages as ProjectTemplate goes about doing its work. This work involves:
* Reading in the global configuration file contained in `config`.
* Loading any R packages you listed in the configuration file.
* Reading in any datasets stored in `data` or `cache`.
* Preprocessing your data using the files in the `munge` directory.

Once that's done, you can execute any of the analysis filese in the `src` 
directory. Before running any of the analysis files please make sure to
call the function called load.project().

Additionally, in order for the pre-processing to work you may need to
install some packages that were used. To do that, you can go into these files
in the `munge` folder from were you can see all the libraries that get imported.
To install these libraries please use the `install.packages('LibraryName')` 
function along with the name of the libraries you do not currently have
already installed on your device.

To run the analysis you would simply go to the `src` folder and call
each of the functions I created for the analysis. They all have comments
so you can easily find what you are looking for.
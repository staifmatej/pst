![FYI](FYI-pst.png)

# Semester Project Assignment


## Instructions for Completion

Form groups of no more than three members. Submit the assignment in the form of a *written report in .pdf format* by emailing it to your teaching assistant. The report should include solutions to the assigned tasks along with explanations of your methodology, graphs, discussion of results, and key parts of the code.

In addition to the correctness of the results, formatting and clarity will be evaluated. There is no need to create a software prototype; the report should be understandable even without the source code.

The deadline for submitting assignments is January 5, 2025, or 72 hours before your exam—whichever comes first.

**Note:** If you choose not to complete some of the tasks, you can receive zero points for them. Please indicate this accordingly.

## Software

You can solve the assignment either using the freeware statistical language R or any other software that has the necessary functionalities (Python, Matlab, Excel, etc.).

Calculations can be performed either manually using the appropriate formulas or by using statistical packages and functions. If you use library functions, explain how you applied them. *It is always necessary to explain the procedure and properly interpret the results.*

## Data and Parameters

Select a representative from your trio. Submit the assignment to the teaching assistant corresponding to your representative. Calculate the parameters of the assignment as follows:

- **K** = day of birth of the group representative (1-31),
- **L** = number of letters in the representative's last name,
- **M** = ((K + L) * 47) mod 11 + 1.

Load the data file from the R Sleuth2 library according to the following table:

| M  | Data File | Description                                  |
|----|-----------|----------------------------------------------|
| 1  | case0101  | creativity score based on type of motivation  |
| 2  | case0102  | salary based on gender                        |
| 3  | case0201  | length of humerus based on sparrow survival   |
| 4  | case0202  | hippocampus volume based on schizophrenia      |
| 5  | case0301  | precipitation based on cloud treatment        |
| 6  | case0302  | dioxin concentration based on military location |
| 7  | case0402  | time to solve task based on geometry teaching type |
| 8  | ex0112    | blood pressure based on diet                   |
| 9  | ex0211    | guinea pig survival based on experiment type  |
| 10 | ex0221    | weight based on sparrow survival               |
| 11 | ex0222    | cholesterol level based on environment         |

Each data file represents observed data from a real experiment where a certain continuous variable was measured separately for two groups.

You can download the library here: *[Sleuth2](https://cran.r-project.org/package=Sleuth2)*

Documentation for the data files is available here: *[Sleuth2 Documentation](https://cran.r-project.org/web/packages/Sleuth2/Sleuth2.pdf)*

## Tasks

- **(1b)** Load the data file and split the observed variable into the respective two observed groups. Briefly describe the data and the problem being investigated. For each group separately, estimate the mean, variance, and median of the respective distribution.
- **(1b)** For each group separately, estimate the density using a histogram and the distribution function using the empirical distribution function.
- **(3b)** For each group separately, find the closest distribution:
  - Estimate the parameters of the normal, exponential, and uniform distributions. Explain how you obtained the estimates.
  - Plot the corresponding densities with the estimated parameters on the histogram.
  - Discuss which distribution best matches the observed data.
- **(1b)** For each group separately, generate a random sample of 100 values from the distribution you chose as the closest, with the parameters estimated in the previous step. Compare the histogram of the simulated values with the observed data and discuss how similar they are.
- **(1b)** For each group separately, calculate the two-sided 95% confidence interval for the mean.
- **(1b)** For each group separately, test the hypothesis at the 5% significance level whether the mean is equal to the value K (assignment parameter, see above) against a two-sided alternative. You can use either the result from the previous step or the output from the relevant built-in function of your software.
- **(2b)** At the 5% significance level, test whether the observed groups have the same mean. Choose the type of test and alternative that best corresponds to the nature of the problem being investigated. Explain your choice and practically interpret the result.

## R Language

### Important Commands

- Installation of the library: `install.packages("Sleuth2")`
- Loading the library: `library(Sleuth2)`
- Exporting data: `write.table(ex0331, "D:/my_data.csv", row.names = FALSE, sep = ";", dec = ",")`
- Rows, columns, and elements from the table: `ex0331[k,]`, `ex0331[,l]`, `ex0331[k,l]`
- Other important commands: `subset()`, `mean()`, `ecdf()`, `hist()`, `t.test()`
- Documentation for a specific command: `?subset`

### References

- Installation: *[R Project](http://www.r-project.org/)*
- "How to use R; installation and basic commands" – Vladislav Bína, Arnošt Komárek, Lenka Komárková *[PDF](http://www.karlin.mff.cuni.cz/~komarek/vyuka/dataRko/Rmanual2.pdf)*
- "R Reference Card" by Tom Short *[PDF](http://cran.r-project.org/doc/contrib/Short-refcard.pdf)*
- "R Reference Card" by Jonathan Baron *[PDF](http://cran.r-project.org/doc/contrib/refcard.pdf)*
- "R Tutorial" at Clarkson University Dept. of Mathematics *[HTTP](http://www.cyclismo.org/tutorial/R/index.html)*
- "The R Guide" by Jason Owen *[PDF](http://cran.r-project.org/doc/contrib/Owen-TheRGuide.pdf)*
- "simpleR – Using R for Introductory Statistics" by John Verzani *[PDF](http://cran.r-project.org/doc/contrib/Verzani-SimpleR.pdf)*
- "An Introduction to R" by Longhow Lam *[PDF](http://cran.r-project.org/doc/contrib/Lam-IntroductionToR_LHL.pdf)*
- "IcebreakeR" by Andrew Robinson *[PDF](http://cran.r-project.org/doc/contrib/Robinson-icebreaker.pdf)*
- "An Introduction to R" from the R Project website *[HTML](http://cran.r-project.org/doc/manuals/R-intro.html)* or *[PDF](http://cran.r-project.org/doc/manuals/R-intro.pdf)*



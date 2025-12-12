Provided is the code and data to replicate "elk population dynamics when carrying capacities vary within and among herds" by Lisa J. Koetke, Adam Duarte & floyd W. Weckerly. Also provided is the code and data to extend this method to four elk herds in North West Colorado namely Cold Springs, Bears Ears, Blue Mountain, and Green River.


Analysis is performed through the package RJAGS (SOURCEFORGE: https://sourceforge.net/projects/mcmc-jags/files/). I ran the software on a mac book, a process to install JAGS on mac is listed here (https://gist.github.com/casallas/8411082)


Koteke code.R and Koetke et al 2020 data.xlsx contain the code and data for the original paper. The R file is structured in the order analysis. After data is uploaded you need to define a functional form for each herd's growth model. This is saved to a file in your computer and recalled via the cat and sink commands. Then you define the parameters to vary and the initial values for those parameters. With these defined you can perform MCMC through RJAGS


To extend this model to Colorado data Elk Population Data 2004-2024 provides the herd data and Replication and Extention.Rmd provides the code. Same process as before expect the data needs to be organized into herd units as its multiple worksheets in one excel work book.

The carrying capacity is defined by dividing our found alpha by the estimated beta. A median value and 95% confidence interval are included  

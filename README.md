Provided is the code and data to replicate "elk population dynamics when carrying capacities vary within and among herds" by Lisa J. Koetke, Adam Duarte & floyd W. Weckerly. Aslo provided is the code and data to extend this method to four elk herds in North West colorado namely Cold Springs, Bears Ears, Blue Mountain, and Green River.

Analysis is preformed through the package RJAGS (SOURCEFORGE: https://sourceforge.net/projects/mcmc-jags/files/). I ran the software on a mac book, a process to install JAGS on mac is listed here (https://gist.github.com/casallas/8411082)

Koteke code.R and Koetke et al 2020 data.xlsx contain the code and datas for original replication specificly. The R file is structured in the order anlysis occers. After data is upladed you need to define a functinal form for each herds growth model. This is saved to a file in your computer and recalled via the cat and sink commands. Then you defeine the parmarers to vary as well as the inital values for those parameters. With these defined you can perform MCMC through RJAGS

To extend this model to Colorado data Elk Popualtion Data 2004-2024 privides the herd data and Replication and Extention.Rmd provides the code. Same process as before expect the data needs to be orginized into herd units as its multiple worksheets in one excel work book.

The carying capacity is defined by dividing our fould alpha by the estiamted beta. A median value and 95% confidence interval are included  

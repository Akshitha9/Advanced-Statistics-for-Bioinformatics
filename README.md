# Advanced-Statistics-for-Bioinformatics
Lab Assignments

Consider a “loaded” die that has a 10% chance of getting a 1-5 and a 50% chance of getting a 6.

(This is from the dishonest casino in the Durbin et al. book; this example is used to define algorithms for Hidden Markov Models in the first few chapters of that text book.  We will see a lot more of this die in future labs and lectures).

What is the mean and variance for the loaded dice?

Make a function in R that “rolls” this dice; return a vector containing the rolls.

		So if I call:  myRolls <- rollLoadedDie(10000)
   
     I would get a vector of size 10,000 that contains the rolls of my loaded die.

Make a histogram of some large number of rolls.  Do the rolls of the loaded die approximate a uniform distribution?

Modify the code on Slide #58 of lecture #2 so that the means vs. trial size 
       plots are from the loaded die.  Generate these plots a few times.  How many rolls appear to be necessary to get convergence on the expected values for the mean and variance?

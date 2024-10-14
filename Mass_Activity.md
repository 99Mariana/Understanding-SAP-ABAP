# [SAP Mass Data Processing and Dynamic Job Launching](#Mass_Activity)


## [Content](#content)

- [SAP Mass Data Processing and Dynamic Job Launching](Mass_Activity.md):
    - [Introduction](#Introduction)
    - [Mass Activity](#Mass_activity)
    - [Dynamic Job Launching Programs](#parallelization)
    - [Comparison: Mass Activities in SAP vs Custom Job Launching Programs](#comparison)


### Introduction

> [SAP Mass Data Processing and Dynamic Job Launching](#Mass_Activity) > [Content](#content) > [This section](#Introduction)

In this chapter, we will explore possible ways to execute a process/program for a large quantity of data. To do this, we will take a deeper look at two tools, understanding how they work, how to implement and design them, and finally analyze them: Mass Activities and Dynamic Job Launching Programs.


### Mass Activity

> [SAP Mass Data Processing and Dynamic Job Launching](#Mass_Activity) > [Content](#content) > [This section](#Mass_activity)

Mass Activities are a tool used in FI-CA for parallelized job processing in a way that reduce the runtime. This is used in business processed that required a really large volume of data to be processed. A Mass activity split the dataset, according the parallelization object defined, such as business partners or contract accounts, and over several jobs that the system then processes in parallel.


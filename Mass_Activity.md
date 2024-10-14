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

#### How to execute a Mass Activity

The Mass activities has a uniform layout, which includes the tab pages: 
- General Selections
- Activity-Specific Selections
- Technical Settings
- Logs

In order to execute a mass activity the following steps must be followed:

1- Create a idenifier for the mass activity, for that is require to fill the fields: date ID and Idenfification

![image](https://github.com/user-attachments/assets/b3328917-3eb5-404e-a5cc-6362b8c856c6)

2- In the General Selections, define data to be processed. The parameters shown in this tab is defined in the moment of the parametrizacion of this trasation, and all the fields are include in a struture created for the Mass activity. 

![image](https://github.com/user-attachments/assets/3fd17950-8241-42ba-b7a7-04dc4ec1ff33)





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

3- Interval Creation, or parallelization of objects. The parallelization can be dynamic or statistic. In the Dynamic parallelization objects, the creation of the intervals is doing during the execution od the mass activity , the system adapt to the workload and resources in real time, adjusting the intervals as needed for optimal performance. In the Statistic parallelization objects, report RFKKDI01 needs to be scheduled before execution of mass activity (daily). For each combination of Object and Variant a standard variant needs to be created.The size of an interval determines how many objects will be included in one interval.

4- Definition of technical settings. That include the definition of the number of the jobs executed in parallel and the interval of records allocated. The split is controlled by variants in which you can specify the number and size of the intervals, and which key area each interval covers. The block size controls how many selected items are held in the main memory. In a automatic load distribution, that mean that the target host is not specified, the system distributes the number of jobs defined by the application servers available. In the Explicit Load Distribution method you can define the target host by the execution of the jobs. 

![image](https://github.com/user-attachments/assets/5c1a5754-5bb4-4e0b-bd24-5178ca02f8ba)

5- Log analysis: The job log records status information and any errors that occur during the program run. Some ajustements can be done in the application logs, for example, is possible to determine the importance a message must have to be output in the application log by a specification in the field Problem Class. Another feature related is the expiry date, that specifies the earliest date on which the log expires, and is relevant because from the application log, you can schedule jobs, and delete logs where the expiry date has been reached from the database.
The application logs are displayed in Enhanced Message Management (transaction EMMA). There you can create and monitor clarification cases for error messages. 

![image](https://github.com/user-attachments/assets/7bc15cd3-d360-4f25-8d1d-7421097cc1b0)









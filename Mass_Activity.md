# [SAP Mass Data Processing and Dynamic Job Launching](#Mass_Activity)


## [Content](#content)

- [SAP Mass Data Processing and Dynamic Job Launching](Mass_Activity.md):
    - [Introduction](#Introduction)
    - [Mass Activity](#Mass_activity)
      - How to Create a Mass Activity? 
      - How to Execute a Mass Activity?
      - How to Monitor a Mass Activity?
    - [Dynamic Job Launching Programs](#parallelization)
    - [Comparison: Mass Activities in SAP vs Custom Job Launching Programs](#comparison)


### Introduction

> [SAP Mass Data Processing and Dynamic Job Launching](#Mass_Activity) > [Content](#content) > [This section](#Introduction)

In this chapter, we will explore possible ways to execute a process/program for a large quantity of data. To do this, we will take a deeper look at two tools, understanding how they work, how to implement and design them, and finally analyze them: Mass Activities and Dynamic Job Launching Programs.


### Mass Activity

> [SAP Mass Data Processing and Dynamic Job Launching](#Mass_Activity) > [Content](#content) > [This section](#Mass_activity)

Mass Activities are a tool used in FI-CA for parallelized job processing in a way that reduce the runtime. This is used in business processed that required a really large volume of data to be processed. A Mass activity split the dataset, according the parallelization object defined, such as business partners or contract accounts, and over several jobs that the system then processes in parallel.

#### How to Create a Mass Activity

The creation of a new Mass Activity is a complex process that requires the creation of **FQEVENTS** to handle intervals and variants, the creation of a **Function Group**, and a transaction that will act as the trigger for the mass activity. It also involves the creation of a **Z-Structure** to define screen parameters, as well as the configuration in **SPRO** for group frames, layout, and the preparation of the Mass Activity.

The steps required in the creacion of a new mass activity process are described and explaned in the follow link: 
https://community.sap.com/t5/sap-for-utilities-blogs/create-custom-mass-activity-for-your-program-config/ba-p/13243855


#### How to Execute a Mass Activity

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

3- Interval Creation or Parallelization of Objects: Parallelization can be either dynamic or static. In dynamic parallelization, the creation of intervals occurs during the execution of the mass activity. The system adapts to the workload and available resources in real-time, adjusting the intervals as needed for optimal performance. In static parallelization, report RFKKDI01 must be scheduled before the execution of the mass activity (daily). A standard variant needs to be created for each combination of object and variant. The size of an interval determines how many objects will be included within that interval.

4- Definition of Technical Settings: This includes defining the number of jobs executed in parallel and the allocation of record intervals. The split is controlled by variants, where you can specify the number and size of intervals, and which key area each interval covers. The block size controls how many selected items are kept in the main memory. In automatic load distribution (where the target host is not specified), the system distributes the number of jobs across the available application servers. In the explicit load distribution method, you can specify the target host for the execution of the jobs.

![image](https://github.com/user-attachments/assets/5c1a5754-5bb4-4e0b-bd24-5178ca02f8ba)

5 - Log Analysis: The job log records status information and any errors that occur during the program run. Some adjustments can be made in the application logs. For example, it is possible to determine the importance a message must have to be included in the application log by specifying the **Problem Class** field. Another related feature is the **expiry date**, which specifies the earliest date on which the log can expire. This is important because from the application log, you can schedule jobs and delete logs from the database once the expiry date has been reached.

   The application logs are displayed in **Enhanced Message Management** (transaction EMMA), where you can create and monitor clarification cases for error messages.

![image](https://github.com/user-attachments/assets/7bc15cd3-d360-4f25-8d1d-7421097cc1b0)

#### How to Monitor a Mass Activity 

The Mass Run Analysis Tool (transaction ST13, also known as MassMan) is a toll that displays the main information about the mass runs, besides of that , the MassMan is also a data collector for SAP Solution Manager business process monitoring, where all the information from MassMan can be reported and used for automatic alerting.

To open MassMan, use transaction ST13 and choose the MASS_MAN_MONITORING application with the F4 help.

![image](https://github.com/user-attachments/assets/77bc08c2-c115-4523-8241-fc3ec3a25aa2)

Is recomended to enter a date( in the startdate parameter ) to ensure that old runs are not included in the list of runs.

As we can see in the image below in one screen we receive an overview of Start-/Endtime of each job, the overall duration the amount of Intervals and Jobs, as well as information about the performance, CPU and DB usage per server, the Counter and the Throughput/hour.

![image](https://github.com/user-attachments/assets/40fad574-dca3-4abe-809a-3e0314547e1b)

The main ideia of the usage of this tool is to speed up the recognition of problems, in order to be able to analyse and define necessary corrective actions depending on the situations. Some possible problems are: Abnormal runtimes (low throughput, expected runtime is out of range); Unusual data volumes; Run cancellations; Processing problems (data quality).














# Trace Analysis in SAP

## Content

-  [Trace Analysis in SAP](Trace_Analysis.md):
    - [Introduction](#introduction)
    - [SQL Trace VS ABAP Runtime Trace](#sql_abap)
    - [Overview Of Trace T_Codes](#t_codes)
    - [Single Transaction Analysis (ST12)](#st12)
    - [Trace analysis](#analysis)
      - SQL Trace
      - ABAP Trace

### Introduction

> [Trace Analysis in SAP](#Trace_Analysis_in_SAP) > [Content](#content) > [This section](#introduction)

Improving ABAP programs and related SQL queries is crucial for maintaining optimal SAP system performance. These optimizations lead to faster, more scalable systems, ensuring smooth operations, reduced downtime, and an improved user experience, which are essential for supporting business-critical processes. In this context, the ability to create traces to analyze these areas represents a powerful tool for developers.

A trace is a tool used to record detailed information about the system's execution and interactions, such as database calls, ABAP code execution, and other system activities. Traces capture specific events, including SQL queries, ABAP program runtime, memory usage, and communication between system components. They are primarily used for performance analysis, debugging, and identifying inefficiencies in the system.

In the following sections, we will explore the different types of traces available in SAP, understand their usage, and learn how to analyze them effectively.

### SQL Trace VS ABAP Runtime Trace

> [Trace Analysis in SAP](#Trace_Analysis_in_SAP) > [Content](#content) > [This section](#sql_abap)

There are two main types of trace: the SQL Trace and the ABAP Runtime Trace. In this section, we will take a closer look at each of these types and analyze their differences, main purposes, scope, and when to use them.

**SQL Trace**: This tool is designed to analyze the performance of interactions between ABAP and the database. Through this type of trace, it is possible to identify bottlenecks in SQL execution. SQL Traces are triggered by an ABAP program or transaction and cover statements such as `SELECT`, `INSERT`, `UPDATE`, and `DELETE`. SQL trace logs typically detail the time taken for each SQL statement, the database tables accessed, and the number of records fetched or modified. This type of trace is very useful for SQL optimization, as it helps identify redundant queries or poorly indexed tables.

**ABAP Runtime Trace**: Its main focus is analyzing performance at the ABAP level. Through this type of trace, it is possible to measure the execution time and memory consumption of ABAP programs or transactions. ABAP Runtime Traces are useful for identifying bottlenecks in ABAP code, such as expensive statements (`LOOPs`, function calls, etc.) and procedures that consume the most processing time.


### Overview of Trace T-Codes

> [Trace Analysis in SAP](#Trace_Analysis_in_SAP) > [Content](#content) > [This section](#t_codes)

| Transaction | Focus | Main Purpose | Best Use Case |
|-------------|-------|--------------|---------------|
| **SE30** | ABAP Runtime | ABAP code-level performance analysis | Identifying inefficient ABAP code, loops, memory usage |
| **SAT** | ABAP Runtime (modern) | Newer tool for ABAP runtime analysis with enhanced features | ABAP trace analysis with modern UI, call hierarchy, and advanced filtering |
| **ST05** | SQL Trace | Analyzing database performance, SQL queries, and table accesses | Investigating long-running SQL statements, inefficient joins, and missing indexes |
| **ST12** | Combined (ABAP & SQL) | End-to-end analysis of ABAP and SQL interactions | Full transaction analysis to find whether ABAP code or SQL queries are causing performance issues |


### Single Transaction Analysis (ST12)

> [Trace Analysis in SAP](#Trace_Analysis_in_SAP) > [Content](#content) > [This section](#st12)

First of all, to better understand the features of the ST12, we will explore its structure and the main options available. In the image below, we can see the structure of the ST12 transaction.

  ![Captura de ecrã 2024-09-27 142208](https://github.com/user-attachments/assets/53ed5209-1e91-4d87-9b17-c68f30b8c399)

As shown in the image, we can divide the ST12 into three sections:

- **Record the trace** – In this section, we define the parameters that will dictate the target object of evaluation.
- **Define the measurement** – Define the measurement scope and technical parameters to be considered in the ABAP trace and SQL trace.
- **Analyze and manage the traces** – Collect the traces for further analysis.

ST12 trace can be captured for “User/Tasks”, “Work Process”, “Current Mode” and “For a Schedule”.

- **User/Tasks:** allows the developer to select a User for whom the trace is to be captured, apart of the task for which the trace is to be captured( Dialog, batch ...)

   ![image](https://github.com/user-attachments/assets/f296a854-ade3-4b64-aa41-f5d3e38f612b)

  
- **Workprocess:** allows to select the server for which the trace is to be captured, if this is not specified all the servers will be captured.
  
   ![image](https://github.com/user-attachments/assets/ce16c435-26e9-43f9-9b80-6584b179b234)


- **Current Mode:** option is used trace the flow of a Transaction or a Program
  
  ![image](https://github.com/user-attachments/assets/05dbba94-add4-4ca0-8e04-613298f8aa93)


- **Schedule:** option is used to run the trace for a batch job for a varied selection criterion as Job name, User name, Program associated with the Job. Is also possible to define the timeframe with parameters such as the maximum duration, the check interval or the start delay of the trace schedule. Note that in this window is also to schedule a trace for Workprocess or for User/Tasks. 

  ![image](https://github.com/user-attachments/assets/f7eabdea-0405-409a-9148-3b317a412489)


### Trace analysis

> [Trace Analysis in SAP](#Trace_Analysis_in_SAP) > [Content](#content) > [This section](#analysis)

#### SQL Trace

In the following image, we can see a typical output for a SQL Trace. 

![sql](https://github.com/user-attachments/assets/9a3bb627-2645-4c3a-bc3b-cfd0f949898f)

The main information to analyze here is the following columns:

- **Executions**: This shows how many times this statement was executed.
- **Redundant#**: This indicates the number of executions that are redundant. If the number is greater than 0, it means that some executions are unnecessary because they perform the same function as previous executions.
- **Identical%**: This corresponds to the percentage of records obtained that are identical between executions.
- **Duration**: This is the total execution time of the statement.
- **Records**: This is the total number of records handled in the statement.
- **Time/Exec**: This represents the average time per execution.
- **Rec/Exec**: This indicates the average number of records handled in each execution.
- **BfTp**: This refers to the buffer type.
- **Table Name**: This indicates the target tables of the SQL statement.
- **Statement String with Placeholders**: This shows the statement under analysis.

For each line three functions are possible: 
    
  ![image](https://github.com/user-attachments/assets/7a529fa1-ed4b-4faf-9f04-9cfba0f76eef)

- **Statement Details**: This section shows the statement details, such as the actual values and the time for each execution.  

  ![sql detail](https://github.com/user-attachments/assets/63eae17b-f6e0-4b0d-b16b-522a0dcc20a8)

- **Explain**: This shows the SQL statement as it is sent to the database, along with the execution plan used by the database.
- **Table Info**: This provides useful information about the table, including its structure and technical settings.

#### Methodology to analyze a SQL trace

1- **Sort by Duration**- The trace normaly is displayed by the order of execution of the statement, sort the lines for duration in descending way can be very useful for identify the statements that are require more time. 

2- **Check for Expensive Statements**-  Ensure that the SQL queries are fetching only the required data, whether indexes are missing, or the query itself can be optimized.

3- **Check the Redundant SQL Calls**- if the Redundant# parameters is greater than 0, the statement is being executed multiple times unnecessarily. This can be avoid by restructuring the logic.


#### ABAP Trace


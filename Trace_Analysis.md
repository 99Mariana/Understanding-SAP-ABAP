# Trace Analysis in SAP

## Content

-  [Trace Analysis in SAP](Trace_Analysis.md):
    - [Introduction](#introduction)
    - [Overview Of Trace T_Codes](#t_codes)
    - [SQL Trace VS ABAP Runtime Trace](#sql_abap)
    - [Single Transaction Analysis (ST12)](#st12)

### Introduction

> [Trace Analysis in SAP](#Trace_Analysis_in_SAP) > [Content](#content) > [This section](#introduction)
.....

### Overview of Trace T-Codes

> [Trace Analysis in SAP](#Trace_Analysis_in_SAP) > [Content](#content) > [This section](#t_codes)

| Transaction | Focus | Main Purpose | Best Use Case |
|-------------|-------|--------------|---------------|
| **SE30** | ABAP Runtime | ABAP code-level performance analysis | Identifying inefficient ABAP code, loops, memory usage |
| **SAT** | ABAP Runtime (modern) | Newer tool for ABAP runtime analysis with enhanced features | ABAP trace analysis with modern UI, call hierarchy, and advanced filtering |
| **ST05** | SQL Trace | Analyzing database performance, SQL queries, and table accesses | Investigating long-running SQL statements, inefficient joins, and missing indexes |
| **ST12** | Combined (ABAP & SQL) | End-to-end analysis of ABAP and SQL interactions | Full transaction analysis to find whether ABAP code or SQL queries are causing performance issues |


### SQL Trace VS ABAP Runtime Trace

> [Trace Analysis in SAP](#Trace_Analysis_in_SAP) > [Content](#content) > [This section](#sql_abap)




### Single Transaction Analysis (ST12)

In the image bellow we can see the structure of the transaction ST12 
    ![image](https://github.com/user-attachments/assets/0bdb06f4-572e-4674-baf2-3c97c8f9027b)

Now we will take a deeper look on these analysis of the different trace parameters.

#### Traces For

> [Trace Analysis in SAP](#Trace_Analysis_in_SAP) > [Content](#content) > [This section](#st12)

ST12 trace can be captured for “User/Tasks”, “Work Process”, “Current Mode” and “For a Schedule”.

- **User/Tasks:** allows the developer to select a User for whom the trace is to be captured and a task for which the trace is to be captured. Task can vary from Dialog, batch etc. Selecting * in Tasks indicate all the tasks will be captured.
  
  ![image](https://github.com/user-attachments/assets/8306add5-12f3-489a-9d8e-cc4d6c2691e7)
  
- **Workprocess:** allows to select the server for which the trace is to be captured. In general all the servers will be captured when not specified.
  
  ![image](https://github.com/user-attachments/assets/8f9810de-07dd-49ef-8495-e3e5dfc8ed17)

- **Current Mode:** option is used trace the flow of a Transaction or a Program
  
  ![image](https://github.com/user-attachments/assets/d7360613-3af5-403a-b61c-5bb3b26e2b96)

- **Schedule:** option is used to run the trace for a batch job for a varied selection criterion as Job name, User name, Program associated with the Job.

  ![image](https://github.com/user-attachments/assets/481b80cb-712d-4c66-9b2a-afd9629872e1)

#### Type of Trace  

 In the lower part of the screen you define the measurement scope. The left part is for the ABAP trace (equivalent to the transaction SE30) and the right part for the Performance Trace (equivalent to the transaction ST05).


# Data Selection Process

## Content

-  [Data Selection Process](Data_Selection_Process.md):
    - [Select Statement](#select-statement)
    - [What Is Happening During The Data Selection Process?](#what-is-happening)
    - [What Can You Do In a Select?](#what-can-you-do)
    - [Dynamic Select Statement](#dynamic_select)
    - [Online vs. Batch Execution Modes: How To Handle Large Datasets In SAP ABAP?](#large_datasets)
    - [How To Use a Cursor?](#cursor)
    - [SQL Query Optimization/Performance Considerations](#performance)
 

### Select Statement

> [Data Selection Process](#Data_Selection_Process) > [Content](#content) > [This section](#select-statement)

In simple terms, a `SELECT` statement in SAP ABAP is an Open SQL command used to retrieve data from database tables based on specific conditions and store the results in appropriate ABAP data objects.


### What Is Happening During The Data Selection Process?

> [Data Selection Process](#Data_Selection_Process) > [Content1](#content) > [This section](#what-is-happening)

What happens during the data retrieval process performed in a select statement? This question has been a big enigma for me for a long time, and researching and really understanding the process has helped me improve the way I've been programming from then on. The process of data retrieval is summarized in the diagram below:

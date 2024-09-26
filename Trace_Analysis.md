# Trace Analysis in SAP

## Content

-  [Trace Analysis in SAP](Trace_Analysis.md):
    - [Introdution](#introduction)
    - [Trace T_Codes](#t_codes)

 

### Trace T_Codes

> [Trace Analysis in SAP](#Trace_Analysis_in_SAP) > [Content](#content) > [This section](#t_codes)

| Transaction | Focus | Main Purpose | Best Use Case |
|-------------|-------|--------------|---------------|
| **SE30** | ABAP Runtime | ABAP code-level performance analysis | Identifying inefficient ABAP code, loops, memory usage |
| **SAT** | ABAP Runtime (modern) | Newer tool for ABAP runtime analysis with enhanced features | ABAP trace analysis with modern UI, call hierarchy, and advanced filtering |
| **ST05** | SQL Trace | Analyzing database performance, SQL queries, and table accesses | Investigating long-running SQL statements, inefficient joins, and missing indexes |
| **ST12** | Combined (ABAP & SQL) | End-to-end analysis of ABAP and SQL interactions | Full transaction analysis to find whether ABAP code or SQL queries are causing performance issues |


### Select Statement

> [Data Selection Process](#Data_Selection_Process) > [Content](#content) > [This section](#select-statement)

> ![image](https://github.com/user-attachments/assets/0bdb06f4-572e-4674-baf2-3c97c8f9027b)


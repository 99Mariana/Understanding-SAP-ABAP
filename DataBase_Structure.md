# DataBase Structure in SAP

## Content

-  [DataBase Structure in SAP](DataBase_Structure.md):
    - [Introduction](#Introduction)
    - [Tables T_codes](#t-codes)
    - [How Is Data Stored In SAP HANA?](#hana)
    - [Colocation Groups](#cc)
    - [Queries in SAP](#Query)
 

### Introduction

> [DataBase Structure in SAP](#Data_Structure) > [Content](#Content) > [This section](#Introduction)

The main goal of this chapter is to understand how data is structured and stored in the database and how this can affect the performance of SQL queries executed in SAP programs. With this goal in mind, we will explore how to relate data from different tables, create, visualize, and modify their structure and data. Finally, we will explore the concept of colocation groups and how we can perform some queries without needing to use a program.

First of all, is important to differentiate the to existent types of tables:

-**Parametric Tables**- the function of these can of tables is store configuration data or reference data that doesn't change frenquently. That's help to standardize inputs and provide constrains for others tables. 

-**Transactional Tables**- These tables store dynamic, operational data that is frequently updated. Transactional Tables represents the actual business activities and events conducted within an organization.

Regardless of the type of table, they are constructed with different fields, some of which can be defined as key fields. These key fields distinguish one record from another, ensuring that no two records have the same key values. This is fundamental in relational databases, including SAP, because they establish relationships between tables and allow for efficient data retrieval and management.

### Tables T-Codes

> [DataBase Structure in SAP](#Data_Structure) > [Content](#Content) > [This section](#t-codes)

There are many t-code transacions that allows the sap users to visualize the data from diferent tables. 
| Transaction Code      | Description       |
|-----------------------|--------------------------------------------------------------|
| **SE11 (Data Dictionary)**  | Mainly used for developers to manage and create database objects such as tables, views, data types, structures, and indexes.                    |
| **SE16 (Data Browser)**     | A read-only interface used to view the data stored in tables.                                                                                 |
| **SE16N (General Table Display)** | Similar to SE16, but with a modern, user-friendly interface. Used for browsing and interacting with table data.                              |
| **SE16H (Advanced Table Display)** | Provides features for quicker and more flexible data retrieval, such as the option to join multiple tables without creating a view, along with enhanced filtering and display options, including derived fields. |
| **SE14 (Database Utility)** | It allows developers to reorganize, adjust, or delete database structures without affecting the data. This is particularly usefull after changes made in the table definitions in SE11. "*"|
| **SM30 (Table Maintenance)** | It is widely used for parametric tables, allow users to maintain data in specific database tables through a user-friendly interface |
| **SPRO (SAP Project Reference Object)** | Often used to configure settings that may involve parametric tables, especially in customizing SAP modules. |


"*" Same tables have a view related and that should be ajust( in SE14 ) when a table struture is changed, in order to maintain data integrity. A view is a virtual table , it does not store data itself but instead provides a way to logically combine and display data from different tables, allowing users to query, filter, and analyze data without directly modifying the underlying tables. Not every table have a view, the decision depens on the specific needs of the organization, many tables have them to improve data access, ensure consistency, maintain security, and encapsulate logic. 



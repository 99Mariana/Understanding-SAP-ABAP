# DataBase Structure in SAP

## Content

-  [DataBase Structure in SAP](DataBase_Structure.md):
    - [Introduction](#Introduction)
    - [Tables T_Codes](#t-codes)
    - [How Is Data Stored?](#hana)
      - Relational Database Management System (RDBMS)
      - In-Memory Databases (IMDB)
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


### How Is Data Stored In SAP?

> [DataBase Structure in SAP](#Data_Structure) > [Content](#Content) > [This section](#hana)

In this chapter we will take a look at two types of storage in SAP:
   -Relational Database Management System(RDBMS)
   -In-Memory Databases(IMDB)

#### Relational Database Management System(RDBMS)

Some popular RDBMS platforms include Oracle Database, MySQL, Microsoft SQL Server, PostgreSQL, and IBM Db2.

In an RDBMS, tables are stored on disk, with each table included in a single database schema and assigned to a particular tablespace or partition for storage. Tables store data in rows or columns, depending on the database design.  RDBMS follows traditional relational database concepts, where relationships between tables are defined using primary and foreign keys, enabling structured data storage and efficient querying.


#### In-Memory Databases(IMDB)

SAP HANA uses an in-memory database (IMDB), which means that data is primarily stored in the system's main memory (RAM) rather than on disk, with changes written back to disk at the end of the process. This allows for much faster data access and processing. In SAP HANA, tables are stored in a columnar format, and large tables can be partitioned across multiple nodes/servers, enabling parallel processing and scalability.

However, if partitions are spread widely across multiple nodes, cross-node operations can slow down performance. Colocating related tables on the same node reduces the overhead of these operations. This is where colocation groups become important.

When you create tables without specifying colocation groups, SAP HANA distributes them across the available nodes based on its internal algorithms. This distribution is generally aimed at balancing load and optimizing resource usage. While tables can still be partitioned, without defined colocation groups, there is no guarantee that related data will be stored on the same node.


### Colocation Groups

> [DataBase Structure in SAP](#Data_Structure) > [Content](#Content) > [This section](#cc)

When defining tables in SAP HANA, colocation groups can be explicitly set to ensure that certain tables reside on the same physical node or partition. This ensures that related tables, which frequently join or reference each other, remain physically close, improving performance by minimizing cross-node communication.

Colocation groups should be used when records in different large tables are often accessed together, so these tables should be grouped in the same colocation group to optimize access. Additionally, when parallel processing is needed, the correct use of colocation groups can reduce the number of remote accesses, further improving performance.

In SAP IS-U, the class `CL_FKK_COLOGRP_UTIL` can be used to determine the colocation groups from different entities. When using the class `CL_FKK_COLOGRP_UTIL`, only the installation values and the values of the partner are accepted. An example of the code could be:

``` ABAP

  types:begin of ty_val,
             nr type nrfrom,
           end of ty_val ,
         begin of ty_cologrp,
             cologrp type cologrp_kk,
           end of ty_cologrp .

    data: lv_nr_cg   type e_cologrp_inst,
          lt_cologrp type table of ty_cologrp,
          lt_table_values type table of ty_val.

      loop at lt_table_values assigning field-symbol(<fs_table_values>) .
        call method cl_fkk_cologrp_util=>get_instance( )->get_group_for_id( exporting iv_id = <fs_table_values>-nr
                                                                            importing ev_cologrp = lv_nr_cg ).
        append value #( cologrp = lv_nr_cg ) to lt_cologrp.
      endloop.

      sort lt_cologrp by cologrp.
      delete adjacent duplicates from lt_cologrp comparing cologrp.

      ro_colgr = value #( for ls_cologrp in lt_cologrp ( sign   = 'I'
                                                         option = 'EQ'
                                                         low    = ls_cologrp-cologrp ) ).
```

If the colocation group is already defined as a field in a table, you can access it directly and use it in queries to improve performance in operations such as data retrieval.

It is important to note that creating a colocation group field in an existing table is more complex than just adding a field, it involves the creation of a script to balance the data in the database. The processor must be prepared to work with colocation group blocks.





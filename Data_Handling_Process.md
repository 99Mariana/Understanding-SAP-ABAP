# Data Handling Process

## Content

-  [Data Handling Process](Data_Handling_Process.md):
    - [Introduction](#Introduction)
    - [Handling Data in Database Tables](#sap_tables)
    - [Handling Data in Internal Tables](#internal_tables)
    - [Transaction Handling in ABAP](#transaction)
    - [Data Transfer Techniques](#data_transfer)
    - [Best Practices for Data Manipulation and Performance](#performance)

 
### Introduction

> [Data Handling Process](#Data_Handling_Process) > [Content](#Content) > [This section](#Introduction)

In this section, we will explore different commands that allow us to handle data. Data handling in SAP ABAP is a sensitive and very important process. It ensures **accurate and reliable data** for business decisions, keeps the system running **efficiently**, and protects sensitive information. It helps with **quick data retrieval**, supports **real-time updates**,  ensures that data from different systems is handled smoothly, and helps automate business processes while keeping the data **consistent and secure**.

### Handling Data in Database Tables

> [Data Handling Process](#Data_Handling_Process) > [Content](#Content) > [This section](#sap_tables)

In a simple way, the process that occurs when you execute a command for handling data involves the following steps:

1. **Open Database Connection:** ABAP interacts with the SAP database through a database interface. This interface is responsible for sending SQL commands to the database system.

2. **Check Syntax and Prepare the SQL Statement:** The SQL statement is checked for syntax errors and prepared for execution.

3. **Send and Process the SQL Statement:** The prepared SQL statement is then sent to the database system, which processes the statement and updates the relevant table(s) in the database.

4. **Locking Mechanism:** During the ```UPDATE``` operation, for instance, the database places a lock on the records being modified. This lock prevents other transactions from accessing the same records simultaneously, ensuring data consistency.

5. **Commit or Rollback:** If the operation is successful, a commit statement is issued, making the changes permanent in the database. If an error occurs during the execution of the SQL commands, a rollback is issued, which reverts the database to its previous state.

6. **Write-Ahead Logging (WAL):** Many databases use a write-ahead logging mechanism to ensure that changes can be recovered in case of a failure. The operation is recorded in the log before it is committed to the database.

7. **Release Locks:** Once the transaction is committed, the locks placed on the records are released, making them available for other transactions.

8. **Return Status:** The ABAP program receives a status return code from the database indicating the success or failure of the command executed.

#### SAP ABAP Database Operations

| **Operation**      | **ABAP Statement Example**                              | **Description**                                                        |
|--------------------|---------------------------------------------------------|------------------------------------------------------------------------|
| **Retrieve Data**   | `SELECT * FROM mara INTO TABLE @lt_mara WHERE matnr = '1000'.` | Retrieves all fields from the `mara` table for material number `1000` and stores them in an internal table `lt_mara`.( see more in [Data Selection Process](Data_Selection_Process.md) )|
| **Insert Data**     | `INSERT mara FROM TABLE lt_mara_insert.`                | Inserts multiple records into the `mara` table from the internal table `lt_mara_insert`. |
|                    | `INSERT mara FROM ls_mara.`                             | Inserts a single record into the `mara` table using the work area `ls_mara`. |
| **Update Data**     | `UPDATE mara SET mtart = 'FERT' WHERE matnr = '1000'.`  | Updates the material type (`mtart`) for the material number `1000` to `'FERT'`. |
|                    | `MODIFY mara FROM TABLE lt_mara_update.`                | Modifies multiple records in the `mara` table using data from the internal table `lt_mara_update`. |
| **Delete Data**     | `DELETE FROM mara WHERE matnr = '1000'.`               | Deletes a record from the `mara` table where the material number is `1000`. |
|                    | `DELETE mara FROM TABLE lt_mara_delete.`                | Deletes multiple records from the `mara` table using an internal table `lt_mara_delete`. |
| **Modify Data**     | `MODIFY mara FROM ls_mara.`                            | Modifies a single entry in the `mara` table using the data from work area `ls_mara`. |
|                    | `MODIFY mara FROM TABLE lt_mara_update.`                | Modifies multiple entries in the `mara` table using data from the internal table `lt_mara_update`. |

#### Performance considerations

To achieve the best possible performance in data handling operations, the following recommendations apply:

- ***INSERT***: Prefer inserting multiple rows at once rather than one-by-one. Indexes can help speed up data handling operations. However, be aware that indexes slightly slow down insertions, as each new entry must be indexed.
- ***DELETE***: Avoid deleting large volumes at once to minimize lock time and resource usage. Define the WHERE condition properly to speed up the operation.
- ***UPDATE***: Avoid updating large volumes at once to minimize lock time and resource usage. Define the WHERE condition properly to optimize the operation, and update only the necessary fields.
- ***MODIFY***: Prefer modifying multiple rows at once rather than one-by-one. If you know beforehand which records already exist in the database and which ones are new, splitting MODIFY into separate INSERT and UPDATE operations is more efficient.


### Handling Data in Internal Tables

> [Data Handling Process](#Data_Handling_Process) > [Content](#Content) > [This section](#internal_tables)

Unlike database tables, internal tables are handled entirely in memory within the SAP application server. Some key differences between these two processes include: Locking is not applied to internal tables since they are not shared resources outside of the current ABAP program or session; No commit or rollback operations are required or applicable for internal tables, as they are temporary and exist only for the duration of the program’s execution.

#### SAP ABAP Internal Table Operations

| **Operation**       | **ABAP Statement Example**                                 | **Description**                                                                 |
|---------------------|------------------------------------------------------------|---------------------------------------------------------------------------------|
| **Append Data**      | `APPEND ls_mara TO lt_mara.`                               | Appends a single work area `ls_mara` to the internal table `lt_mara`.            |
| **Insert Data**      | `INSERT ls_mara INTO lt_mara INDEX 2.`                     | Inserts the work area `ls_mara` into the internal table `lt_mara` at index 2.    |
| **Modify Data**      | `MODIFY lt_mara FROM ls_mara INDEX 3.`                     | Modifies the entry at index 3 in the internal table `lt_mara` with `ls_mara`.    |
| **Read Data**        | `READ TABLE lt_mara INTO ls_mara WITH KEY matnr = '1000'.` | Reads the entry in `lt_mara` where the key `matnr` equals '1000' into `ls_mara`. |
|       | `lt_mara = value #( for ls_line in lt_mara where ( martnr > '1000' ) (ls_line ))` | Reads the entry in `lt_mara` where the `matnr` field is greater than '1000'. |
| **Loop Through Data**| `LOOP AT lt_mara INTO ls_mara.`                            | Loops through all entries in the internal table `lt_mara` into the work area.    |
| **Delete Data**      | `DELETE lt_mara WHERE matnr = '1000'.`                     | Deletes the entry in `lt_mara` where the key `matnr` equals '1000'.              |
| **Sort Data**        | `SORT lt_mara BY matnr.`                                   | Sorts the internal table `lt_mara` by the field `matnr`.                         |
| **Clear Table**      | `CLEAR lt_mara.`                                           | Clears all entries from the internal table `lt_mara`.                            |
| **Free Table**       | `FREE lt_mara.`                                            | Frees up memory by completely deallocating the internal table `lt_mara`.         |
| **Delete Adjacent Duplicates** | `DELETE ADJACENT DUPLICATES FROM lt_mara COMPARING matnr.` | Deletes consecutive duplicate entries from the internal table based on `matnr`.  |



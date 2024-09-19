# Data Handling Process

## Content

-  [Data Handling Process](Data_Handling_Process.md):
    - [Introduction](#Introduction)
    - [Handling Data in Database Tables](#sap_tables)
      - Process Description
      - SAP ABAP Database Operations
      - Performance Considerations
    - [Handling Data in Internal Tables](#internal_tables)
      - SAP ABAP Internal Table Operations
      - Performance Considerations
    - [String Manipulation](#string)
    - [Numeric Data Manipulation](#numeric)
    - [Transaction Handling in ABAP](#transaction)
      - COMMIT
      - ROLLBACK
    - [Error Handling](#error)
    - [Data Transfer Techniques](#data_transfer)
       - CALL FUNCTION (RFC)
       - SUBMIT
       - CALL TRANSACTION

 
### Introduction

> [Data Handling Process](#Data_Handling_Process) > [Content](#Content) > [This section](#Introduction)

In this section, we will explore different commands that allow us to handle data. Data handling in SAP ABAP is a sensitive and very important process. It ensures **accurate and reliable data** for business decisions, keeps the system running **efficiently**, and protects sensitive information. It helps with **quick data retrieval**, supports **real-time updates**,  ensures that data from different systems is handled smoothly, and helps automate business processes while keeping the data **consistent and secure**.

### Handling Data in Database Tables

> [Data Handling Process](#Data_Handling_Process) > [Content](#Content) > [This section](#sap_tables)

#### Process Description

In a simple way, the process that occurs when you execute a command for handling data involves the following steps:

1. **Open Database Connection:** ABAP interacts with the SAP database through a database interface. This interface is responsible for sending SQL commands to the database system.

2. **Check Syntax and Prepare the SQL Statement:** The SQL statement is checked for syntax errors and prepared for execution.

3. **Send and Process the SQL Statement:** The prepared SQL statement is then sent to the database system, which processes the statement and updates the relevant table(s) in the database.

4. **Locking Mechanism:** During the ```UPDATE``` operation, for instance, the database places a lock on the records being modified. This lock prevents other transactions from accessing the same records simultaneously, ensuring data consistency.

5. **Commit or Rollback:** If the operation is successful, a commit statement is issued, making the changes permanent in the database. If an error occurs during the execution of the SQL commands, a rollback is issued, which reverts the database to its previous state.

6. **Release Locks:** Once the transaction is committed, the locks placed on the records are released, making them available for other transactions.

7. **Return Status:** The ABAP program receives a status return code from the database indicating the success or failure of the command executed.

![image](https://github.com/user-attachments/assets/c9edb8c5-0d34-4c02-9da9-912e9a51f926)



#### SAP ABAP Database Operations

| **Operation**      | **ABAP Statement Example**                              | **Description**                                                        |
|--------------------|---------------------------------------------------------|------------------------------------------------------------------------|
| **SELECT**   | `SELECT * FROM mara INTO TABLE @lt_mara WHERE matnr = '1000'.` | Retrieves all fields from the `mara` table for material number `1000` and stores them in an internal table `lt_mara`.( Read more in the section: [Data Selection Process](Data_Selection_Process.md) )|
| **INSERT**     | `INSERT mara FROM TABLE lt_mara_insert.`                | Inserts multiple records into the `mara` table from the internal table `lt_mara_insert`. |
|                    | `INSERT mara FROM ls_mara.`                             | Inserts a single record into the `mara` table using the work area `ls_mara`. |
| **UPDATE**     | `UPDATE mara SET mtart = 'FERT' WHERE matnr = '1000'.`  | Updates the material type (`mtart`) for the material number `1000` to `'FERT'`. |
|                | `UPDATE mara FROM TABLE lt_mara_update.`                | Modifies multiple records in the `mara` table using data from the internal table `lt_mara_update`. |
| **DELETE**     | `DELETE FROM mara WHERE matnr = '1000'.`               | Deletes a record from the `mara` table where the material number is `1000`. |
|                    | `DELETE mara FROM TABLE lt_mara_delete.`                | Deletes multiple records from the `mara` table using an internal table `lt_mara_delete`. |
| **MODIFY**     | `MODIFY mara FROM ls_mara.`                            | Modifies a single entry in the `mara` table using the data from work area `ls_mara`. |
|                    | `MODIFY mara FROM TABLE lt_mara_update.`                | Modifies multiple entries in the `mara` table using data from the internal table `lt_mara_update`. |


**NOTE:** SAP IS-U offers several functions that allow us to UPDATE, MODIFY, DELETE, and INSERT data. It is recommended to use them when handling critical tables or complex related data. These functions ensure data integrity and create centralized error handling


#### Performance Considerations

To achieve the best possible performance in data handling operations, the following recommendations apply:

- **INSERT**: Prefer inserting multiple rows at once rather than one-by-one. Indexes can help speed up data handling operations. However, be aware that indexes slightly slow down insertions, as each new entry must be indexed.
- **DELETE**: Avoid deleting large volumes at once to minimize lock time and resource usage. Define the ```WHERE``` condition properly to speed up the operation.
- **UPDATE**: Avoid updating large volumes at once to minimize lock time and resource usage. Define the ```WHERE``` condition properly to optimize the operation, and update only the necessary fields.
- **MODIFY**: Prefer modifying multiple rows at once rather than one-by-one. If you know in advance which records already exist in the database and which ones are new, splitting ```MODIFY``` into separate ```INSERT``` and ```UPDATE``` operations can be more efficient.
- **SELECT**: Read more in the section: [Data Selection Process](Data_Selection_Process.md)




### Handling Data in Internal Tables

> [Data Handling Process](#Data_Handling_Process) > [Content](#Content) > [This section](#internal_tables)

Unlike database tables, internal tables are handled entirely in memory within the SAP application server. Some key differences between these two processes include: Locking is not applied to internal tables since they are not shared resources outside of the current ABAP program or session; No commit or rollback operations are required or applicable for internal tables, as they are temporary and exist only for the duration of the program’s execution.

#### SAP ABAP Internal Table Operations

| **Operation**       | **ABAP Statement Example**      | **Description**    |
|---------------------|-----------------------------|----------------------------------------|
| **APPEND**      | `APPEND ls_mara TO lt_mara.`                               | Appends a single work area `ls_mara` to the internal table `lt_mara`.            |
| **INSERT**      | `INSERT ls_mara INTO lt_mara INDEX 2.`                     | Inserts the work area `ls_mara` into the internal table `lt_mara` at index 2.    |
| **MODIFY**      | `MODIFY lt_mara FROM ls_mara INDEX 3.`                     | Modifies the entry at index 3 in the internal table `lt_mara` with `ls_mara`.    |
| **READ**        | `READ TABLE lt_mara INTO ls_mara WITH KEY matnr = '1000'.` | Reads the entry in `lt_mara` where the key `matnr` equals '1000' into `ls_mara`. |
|       | `lt_mara = value #( for ls_line in lt_mara where ( martnr > '1000' ) (ls_line ))` | Reads the entry in `lt_mara` where the `matnr` field is greater than '1000'. |
| **LOOP**| `LOOP AT lt_mara INTO ls_mara.`                            | Loops through all entries in the internal table `lt_mara` into the work area.    |
| **DELETE**      | `DELETE lt_mara WHERE matnr = '1000'.`                     | Deletes the entry in `lt_mara` where the key `matnr` equals '1000'.              |
| |`DELETE ADJACENT DUPLICATES FROM lt_mara COMPARING matnr.` | Deletes consecutive duplicate entries from the internal table based on `matnr`.  |
| **SORT**        | `SORT lt_mara BY matnr.`                                   | Sorts the internal table `lt_mara` by the field `matnr`.                         |
| **CLEAR**      | `CLEAR lt_mara.`                                           | Clears all entries from the internal table `lt_mara`.                            |
| **FREE**       | `FREE lt_mara.`                                            | Frees up memory by completely deallocating the internal table `lt_mara`.         |
| **COLLECT**           | `COLLECT wa INTO lt_table.`                                       | Groups records by key fields of `wa` and adds up numeric fields into `lt_table`.                    |
| **FILTER**            | `lt_filtered = FILTER #( lt_table WHERE status = 'Active' ).`     | Filters `lt_table` for entries where the `status` field equals `'Active'`.                           |
| **REDUCE**            | `lv_total = REDUCE i( INIT sum = 0 FOR wa IN lt_table NEXT sum = sum + wa-amount ).` | Aggregates (sums) the `amount` field of each entry in `lt_table`.                                   |
| **MOVE-CORRESPONDING** | `MOVE-CORRESPONDING wa_src TO wa_dest.`                           | Copies matching fields from `wa_src` to `wa_dest`.                                                  |
| **VALUE** | `VALUE ty_type( for <f_z> in it_table ( field1 = <f_z>-field1 field2 = lv_value..... )` |  Possible to define a table base in values of other tables and variables  |


#### Performance considerations

- ```HASHED``` tables are usefull when we are working with large data tables that are filled in a single step, don´t need to be modified but they are read often by their key. Basically in the ```HASHED``` tables when they are filled generate a hash value, which determines the position (or "bucket") in memory where the corresponding record is stored, then when we use the key in a read statement directly the record is accessed.

- ```SORTED``` tables are automatically kept in sorted order based on the key when entries are inserted. Sorted tables demonstrate their value only for large numbers of read accesses. Use a binary search in a read operation can have good repercussions in improving performance in the read accesses process.

- Prefer ```LINE_EXISTS``` to ```READ TABLE``` or ```LOOP AT``` when the intention is to verify if some record exist in the internal table.
- Prefer ```READ TABLE``` to ```LOOP AT``` when the intention is to verify get the data from a specific record of the table .
- Consider to include the ```WHERE``` condition in a ```LOOP``` instead of the usage of the ```IF``` inside the ```LOOP``` iteration.


### String Manipulation

> [Data Handling Process](#Data_Handling_Process) > [Content](#Content) > [This section](#string)

| **Command**           | **ABAP Statement Example**           | **Description**                                                                                       |
|-----------------------|------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------|
| **SPLIT**             | `SPLIT 'AB-CD-EF' AT '-' INTO lv_part1 lv_part2 lv_part3.`    | Splits 'AB-CD-EF' into `lv_part1 = 'AB'`, `lv_part2 = 'CD'`, `lv_part3 = 'EF'`.                  |
| **CONCATENATE**       | `CONCATENATE lv_part1 lv_part2 INTO lv_result.`                | Combines `lv_part1` and `lv_part2`. If `lv_part1 = 'AB'` and `lv_part2 = 'CD'`, then `lv_result = 'ABCD'`. |
| **REPLACE**           | `REPLACE 'AB' WITH 'XY' INTO lv_string.`                       | Replaces 'AB' with 'XY' in `lv_string`. If `lv_string = 'ABCD'`, it becomes `lv_string = 'XYCD'`. |
| **SUBSTRING**         | `lv_substring = lv_string+2(3).`                                | Extracts a substring starting at position 2 with length 3. If `lv_string = 'ABCDEFG'`, then `lv_substring = 'CDE'`. |
| **SUBSTRING_AFTER**   | `lv_result = substring_after( val = 'A-B-C' sub = '-' ).`     | Returns the part of a string after '-'. If `val = 'A-B-C'`, then `lv_result = 'B-C'`.              |
| **SUBSTRING_BEFORE**  | `lv_result = substring_before( val = 'A-B-C' sub = '-' ).`    | Returns the part of a string before '-'. If `val = 'A-B-C'`, then `lv_result = 'A'`.              |
| **TRANSLATE**         | `TRANSLATE lv_string TO UPPER CASE.`                            | Converts `lv_string` to uppercase. If `lv_string = 'abcd'`, it becomes `lv_string = 'ABCD'`.      |
| **SHIFT**             | `SHIFT lv_string LEFT BY 3 PLACES.`                             | Shifts `lv_string` left by 3 places. If `lv_string = 'abcdef'`, it becomes `lv_string = 'def'`.   |
| **CONDENSE**          | `CONDENSE lv_string.`                                           | Removes extra spaces. If `lv_string = ' AB CD '`, it becomes `lv_string = 'AB CD'`.               |
| **CONV**              | `lv_num = CONV i( '123' ).`                                     | Converts '123' (string) to an integer `123`.      |

### Numeric Data Manipulation

> [Data Handling Process](#Data_Handling_Process) > [Content](#Content) > [This section](#numeric)

| **Command**           | **ABAP Statement Example**           | **Description**       |
|-----------------------|------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------|
| **ROUND**             | `lv_result = ROUND( val = 123.456 precision = 2 ).`            | Rounds 123.456 to 123.46.                                                                            |
| **FLOOR**             | `lv_result = FLOOR( 123.9 ).`                                   | Rounds down 123.9 to 123.                                                                           |
| **CEIL**              | `lv_result = CEIL( 123.1 ).`                                    | Rounds up 123.1 to 124.                                                                             |
| **TRUNC**             | `lv_result = TRUNC( 123.456 ).`                                 | Truncates 123.456 to 123.                                                                           |
| **ABS**               | `lv_result = ABS( -123 ).`                                     | Converts -123 to 123.                                                                                |                                                 |
| **MOD**               | `lv_remainder = 10 MOD 3.`                                     | Divides 10 by 3, remainder is 1.                                                                    |
| **ADD**               | `lv_result = lv_num1 + lv_num2.`                               | Adds `lv_num1` and `lv_num2`.                                                                       |
| **SUBTRACT**          | `lv_result = lv_num1 - lv_num2.`                              | Subtracts `lv_num2` from `lv_num1`.                                                                 |
| **MULTIPLY**          | `lv_result = lv_num1 * lv_num2.`                               | Multiplies `lv_num1` by `lv_num2`.                                                                   |
| **DIVIDE**            | `lv_result = lv_num1 / lv_num2.`                               | Divides `lv_num1` by `lv_num2`.                                                                     |
| **CONVERSION_EXIT_ALPHA_INPUT**   | `CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' EXPORTING input = '123' IMPORTING output = lv_out.` | Converts the input '123' to '0000000123' by adding leading zeros.                                  |
| **CONVERSION_EXIT_ALPHA_OUTPUT**  | `CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT' EXPORTING input = '0000000123' IMPORTING output = lv_out.` | Converts '0000000123' to '123' by removing leading zeros.                                          |


### Transaction Handling in ABAP

> [Data Handling Process](#Data_Handling_Process) > [Content](#Content) > [This section](#transaction)

In this section, we will explore the `COMMIT` and `ROLLBACK` statements. These transactions are crucial for maintaining data integrity and preventing data inconsistencies, especially when multiple operations depend on each other. For example, when updating a customer's information and related billing data, transactions ensure that either both updates succeed or neither does.

Both statements include ISU functions( `DB_COMMIT`, `DB_ROLLBACK` ) that can achieve the same goals as the simple `COMMIT` and `ROLLBACK` commands. These functions can be very useful in processes that use a `CURSOR`, for instance, because the `COMMIT` and `ROLLBACK` commands inherently close the `CURSOR`, whereas the ISU functions do not.

#### COMMIT
The `COMMIT` command is responsible for saving all changes made during the transaction to the database permanently.

It is important to note that the `COMMIT` command can have a negative impact on performance because each commit requires database interaction. Therefore, it is recommended to group multiple database operations and issue a single `COMMIT` at the end. This reduces the number of commits and optimizes resource usage.

#### ROLLBACK
The `ROLLBACK` command reverts any changes made during a transaction. It is very useful when an error occurs during the execution of operations and helps maintain data consistency and integrity.

The `ROLLBACK` command can negatively affect performance because this operation requires considerable resources, as it involves reading the original values from disk and writing them back. This can generate significant input/output (I/O) activity and slow down performance due to locking.

Effective error handling can help mitigate these challenges. For instance, validating data before committing can reduce the need for rollbacks.

### Error Handling

> [Data Handling Process](#Data_Handling_Process) > [Content](#Content) > [This section](#error)






### Data Transfer Techniques

> [Data Handling Process](#Data_Handling_Process) > [Content](#Content) > [This section](#data_transfer)

In this section, we will take a deeper look at some of the data transfer techniques between different entities, such as programs, reports, function modules, and more. Various methods are available depending on specific requirements, such as performance, reliability, or the need for real-time communication. Two of the most common methods are using `CALL FUNCTION` and `SUBMIT`. In this section, we will also cover `CALL TRANSACTION`.

#### CALL FUNCTION - Remote Function Calls and Data Transfer

The `CALL FUNCTION` statement is widely used to execute Function Modules, which can be called either locally (within the same system) or remotely. For Remote Function Calls (RFC), the destination must be defined. These calls can be either synchronous or asynchronous:

- **Synchronous** means that the program calling the function module waits for the function's execution to complete before continuing its own processing.

- **Asynchronous** means that the program does not wait for the function to complete. The function is executed in parallel, and the response is received later in the program’s process. To define an asynchronous RFC, the `STARTING NEW TASK` clause must be used. It is possible to specify what should be executed `ON END OF TASK`, either by calling a method of a class using the `CALLING` statement or executing a subroutine using the `PERFORMING` statement.

```` ABAP

CALL FUNCTION 'REMOTE_FUNCTION_MODULE'
  "DESTINATION 'DESTINATION_NAME'
  STARTING NEW TASK 'TASK_NAME'
  PERFORMING callback_subroutine ON END OF TASK
  EXPORTING
    param1 = value1
  CHANGING
   pio_table = lw_table.
    
  EXCEPTIONS
    system_failure = 1
    communication_failure = 2.

FORM callback_subroutine USING taskname.
 " Process the result if the 'REMOTE_FUNCTION_MODULE' function after the asynchronous call finishes
   RECEIVE RESULTS
    FROM FUNCTION 'REMOTE_FUNCTION_MODULE'
    CHANGING pio_table = lw_table.

ENDFORM.

````

#### SUBMIT - Executing Reports and Transferring Data

The goal of the `SUBMIT` statement is to execute an ABAP report program from within another program. It is possible to pass parameters and selection criteria to the report using the `WITH` clause.

By default, the `SUBMIT` statement displays the output of the report on the screen. However, it is possible to capture the output, store it in memory using the `EXPORTING LIST TO MEMORY` clause, and later retrieve it using the `LIST_FROM_MEMORY` clause, and even display it using the `DISPLAY_LIST` statement.

```ABAP
SUBMIT z_my_report WITH p_date = sy-datum
  EXPORTING LIST TO MEMORY.

CALL FUNCTION 'LIST_FROM_MEMORY'
  IMPORTING listobject = it_list.

CALL FUNCTION 'DISPLAY_LIST'
  TABLES listobject = it_list.
```

#### CALL TRANSACTION - Executing Transactions with Data Transfer

The `CALL TRANSACTION` statement is used to execute an SAP transaction code (T-code) from within an ABAP program. Similar to RFCs, the transaction can be executed either synchronously or asynchronously. To define the values used in the SAP transaction, a special structure called `BDCDATA` is used. Take a look at the example below:

```ABAP
DATA: bdcdata TYPE TABLE OF bdcdata.

PERFORM bdc_dynpro USING 'SAPMM06E' '0101'.  " Transaction Screen Details
PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
PERFORM bdc_field  USING 'RM06E-BUKRS' '1000'.  " Input Data for Company Code

CALL TRANSACTION 'ME21N' USING bdcdata
     MODE 'A'  " Processing Mode: A (Display), N (Background), E (Error Screen)
     UPDATE 'S'.  " Update Mode: S (Synchronous), A (Asynchronous)
```



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

> [Data Selection Process](#Data_Selection_Process) > [Content](#content) > [This section](#what-is-happening)

What happens during the data retrieval process performed in a select statement? This question has been a big enigma for me for a long time, and researching and really understanding the process has helped me improve the way I've been programming from then on. The process of data retrieval is summarized in the diagram below:

<p align="center">
  <img src="https://github.com/user-attachments/assets/0bdc5293-0867-47d6-826e-ea08ce3d1002" alt="" style="width:30%";>
  <br>
  <em>SELECT Statements in SAP ABAP: Step-by-Step Data Retrieval Process</em>
</p>


Let’s take a deeper look at the process:

 1. **ABAP Code Execution:** ABAP programs are executed by the Application Server, which allocates resources (like work processes) to run the code.

 2. **Database Interface Processing:** This interface acts as a bridge between the ABAP application and the database. It performs two key tasks: converting ABAP SQL to native SQL and optimizing the query. In ABAP, when you write a `SELECT` statement, you are using ABAP Open SQL, a standardized SQL language provided by SAP that allows developers to write database queries without worrying about the specific SQL dialect of the underlying database (such as Oracle, SAP HANA, or Microsoft SQL Server). Therefore, converting ABAP SQL to native SQL is an essential step in database communication. Before sending the SQL query to the database, the database interface optimizes the query for performance, including the use of buffers or indexes.

 3. **Query Execution on the Database Server:** The primary function of the database server is to store and manage tables, which are saved in the database's disk storage but accessed in memory when queried. The database management system (DBMS) executes the query, evaluates the conditions specified in the `WHERE` clause, and uses the query processor to filter, sort, and aggregate the requested data.

 4. **Data Transfer:** In this step, the DBMS returns the resulting data to the application server. This communication process occurs via a network connection.

 5. **Storage in ABAP Memory and Program Use:** The application server receives the information and stores it in suitable ABAP objects (such as internal tables, work areas, or variables) for further processing in the ABAP program.


### What Can You Do In a Select?

> [Data Selection Process](#Data_Selection_Process) > [Content](#content) > [This section](#what-can-you-do)

When using the `SELECT` statement is possible to adapt it by incorporating aggregation functions, conditions, filters, string manipulations, field operations, grouping, and sorting. Additionally, multiple tables can join multiple tables and limited the amount of data retrieved in the end of the query execution. 

#### AGGREGATION FUNCTIONS

| Functionality | Example  | Description    |
| :----------- | :--------------: | -------------------------: |
| COUNT | ```SELECT COUNT(*) FROM table INTO @data(lv_count)```  | Counts the number of records that match the condition |                  | SUM   | ```SELECT SUM(field) FROM table INTO @data(lv_sum)```  | Calculates the sum of the values of a numeric field   |
| AVG   | ```SELECT AVG(field) FROM table INTO @data(lv_avg)```  | Calculates the average of the values of a numeric field |
| MIN   | ```SELECT MIN(field) FROM table INTO @data(lv_min)```  | Finds the smallest value of a numeric field |
| MAX   | ```SELECT MAX(field) FROM table INTO @data(lv_max)```  | Finds the largest value of a numeric field |

#### FILTERING AND CONDITIONS

| Functionality   | Example          | Description   
| :----------- | :--------------: | -------------------------: |
| WHERE        | ```SELECT * FROM table WHERE field = 'value' ```  | Filters records based on a condition |
| CASE in WHERE | ```SELECT * FROM table WHERE CASE WHEN field1 = 'value1' THEN field2 ELSE field3 END = 'value4' ``` | Uses CASE conditions within a WHERE clause for conditional logic |
| MOD             | ```SELECT * FROM table WHERE field MOD 2 = 0 ```| Filters records based on divisibility (modulus operation) |            
| BETWEEN         | ```SELECT * FROM table WHERE field BETWEEN 'value1' AND 'value2' ``` | Filters records where a field's value is between two specified values |
| LIKE   | ```SELECT * FROM table WHERE field LIKE 'ABC%' ```  | Searches for patterns in string fields |
| IN     | ```SELECT * FROM table WHERE field IN ('value1', 'value2')``` | Checks if a field's value is within a specified set of values.|


#### STRING MANIPULATIONS

| Functionality   | Example        | Description        |
| :----------- | :--------------: | -------------------------: |
| SUBSTRING     | ```SELECT SUBSTRING(field, 1, 3) AS sub_field FROM table``` | Extracts parts of a string field. |
| LEFT / RIGHT  | ```SELECT LEFT(field, 5) AS left_part FROM table```| Extracts characters from the left or right side of a string field.  |
| UPPER / LOWER | ```SELECT UPPER(field) AS upper_field FROM table.``` | Converts string fields to upper or lower case. |
| CONCATENATE   | ```SELECT field1, field2, CONCAT(field1, field2) AS new_field FROM table.``` | Creates return fields by concatenating values from other fields |


#### FIELD OPERATIONS

| Functionality       | Example       | Description   |
| :----------- | :--------------: | -------------------------: |
| CASE in SELECT  | ```SELECT field, CASE WHEN field > 10 THEN 'High' ELSE 'Low' END AS category FROM table.``` | Uses CASE to create conditional fields directly in the SELECT statement. |
| COALESCE   | ```SELECT COALESCE(field1, field2, 'default') AS coalesce_field FROM table.``` | Returns the first non-null value in a list of fields.   |
| CAST    | ```SELECT CAST(field AS INTEGER) AS int_field FROM table.```   | Changes the data type of a field within the query. |
| SQL Functions   | ```SELECT ABS(field) AS abs_field FROM table.```  | Uses SQL functions like ABS, ROUND, CEIL, FLOOR on fields. |


#### GROUPING AND SORTING

| Functionality | Example   | Description    |
| :----------- | :--------------: | -------------------------: |
| GROUP BY    | ```SELECT field, SUM(field2) FROM table GROUP BY field.``` | Groups records based on one or more fields.    |
| HAVING      | ```SELECT field, COUNT(*) FROM table GROUP BY field HAVING COUNT(*) > 1.``` | Filters groups of records after grouping.|
| ORDER BY    | ```SELECT * FROM table ORDER BY field ASCENDING.``` | Sorts the records by one or more fields.  |


#### JOINS

| Functionality  | Example    | Description      |
| :----------- | :--------------: | -------------------------: |
| INNER JOIN    | ```SELECT * FROM table1 INNER JOIN table2 ON table1~field = table2~field.```  | Joins tables, returning only records that have matches.                      |
| LEFT OUTER JOIN   | ```SELECT * FROM table1 LEFT OUTER JOIN table2 ON table1~field = table2~field.``` | Joins tables, returning all records from the left table and matches from the right. |
| RIGHT OUTER JOIN  | ```SELECT * FROM table1 RIGHT OUTER JOIN table2 ON table1~field = table2~field.``` | Joins tables, returning all records from the right table and matches from the left. |

#### ADVANCED QUERIES

| Functionality     | Example    | Description      |
| :----------- | :--------------: | -------------------------: |
| FOR ALL ENTRIES   | ```SELECT * FROM table WHERE field IN @it_table.```| Optimizes the selection to work with a set of external records. |
| INNER SELECT      | ```SELECT field FROM table WHERE field = (SELECT MAX(field) FROM table2).``` | Uses a subquery within the main SELECT statement.   |
| EXISTS   | ```SELECT * FROM table WHERE EXISTS (SELECT * FROM other_table WHERE field = table~field).``` | Checks the existence of records in a subquery. |


#### LIMIT AND OFFSET

| Functionality | Example                                                          | Description                               |
| :----------- | :--------------: | -------------------------: |
| UP TO n ROWS  | ```SELECT * FROM table UP TO 10 ROWS INTO TABLE @it_table.```        | Limits the number of records returned.    |
| OFFSET        | ```SELECT * FROM table OFFSET 5 INTO TABLE @it_table.```            | Shifts the start of the records returned. |


### Dynamic Select statement

> [Data Selection Process](#Data_Selection_Process) > [Content](#content) > [This section](#dynamic_select)

Dynamic programming techniques can pose significant security risks if not handled correctly. Therefore, it's important to be careful and validate any content coming from external sources that is used in dynamic statements. This can be done using the ```CL_ABAP_DYN_PRG``` system class.

You can create dynamic SELECT statements at different levels, such as:

-	**Fields/Columns**: Dynamically specifying which fields to select from the database.
-	**Table Names**: Dynamically choosing which database table to query.
-	**Conditions**: Dynamically building the WHERE clause based on runtime conditions.

**Code example:**

``` ABAP
  data: lv_table      type dd02l-tabname,
        lv_fields     type string,
        itab(100)     occurs 0 with header line,
        lv_conditions type string,
        lt_result     type table of sflight.

  " Framework takes table name, fields, and conditions as input
  lv_table      = 'SFLIGHT'.
  lv_fields     = 'CARRID, CONNID'.

  move 'CARRID = ''LH'' AND CONNID = 455' to itab.
  append itab.

  " Dynamic SELECT for a generic data extraction framework
  select (lv_fields)
    from (lv_table)
    where (itab)
    into table @lt_result.

  if sy-subrc ne 0.
    clear lt_result.
  endif.

"Declaration inline is not posible with the fields or tables define in a variavel.
  select carrid,
         connid
    from sflight
    where (itab)
    into table @data(lt_result2).

  if sy-subrc = 0.
    clear lt_result2.
  endif.
```


Main possible reasons to create a dynamic `SELECT`:

- **Flexible Queries Based on User Input:** When there is a need to create a tool that adapts to user specifications entered in the selection screen.

- **Building Generic Reports:** To add flexibility in the columns displayed as the output of the retrieval process. Dynamic SQL allows the report to adapt based on the user’s preferences or configuration, making it more versatile.

- **Handling Different Database Tables:** In applications where the database table to query from is determined at runtime (e.g., based on user choices or configuration), dynamic SQL enables the program to adjust which table it selects data from without hardcoding all possible table names.

- **Optimizing Performance for Specific Scenarios:** By using dynamic SQL, you can tailor your queries to fetch only the necessary fields based on specific conditions. This optimization reduces the amount of data transferred and processed, leading to better performance.


### Online vs. batch execution modes: How to handle large data sets in SAP ABAP?

> [Data Selection Process](#Data_Selection_Process) > [Content](#content) > [This section](#large_datasets)

First, it is important to know that the online and batch execution of programs depends on the resources that are allocated to the application server part, which are defined by parameters such as:

- ```abap/heap_area_dia```: Defines the maximum amount of private memory that can be used by each individual user in online mode.
- ```abap/heap_area_nondia```: Defines the maximum amount of private memory that can be used by each individual user in batch mode.
- ```abap/heap_area_total```: Specifies the maximum amount of memory that can be used across all processes (both online and batch) in the entire SAP instance.
- ```ztta/roll_area```: Determines how much memory is reserved for each user session to store their context data (e.g., data, variables).
- ```ztta/roll_extension```: Specifies the size of the extended memory, which is a shared memory area used by all processes to store user data.

These parameters dictate how much memory can be used in different execution modes (online and batch) and impose limitations on memory usage. On the other hand, execution time is also a limitation to consider in online mode.

The main reason for exceeding these limits is the handling of large amounts of data, which can lead to memory errors and very long response times. To address this, several practices can be implemented:

- **Use Cursors**: Process data in batches to avoid overloading memory in a single operation.
- **COMMIT WORK Commands**: In some cases, these commands can help free up memory.
- **Optimize SQL Queries**: Reduce the amount of data selected by handling only the necessary data.
- **Create Decentralized/Parallel Processing**: Divide data processing into batches and execute them in parallel jobs.
  
In the following sections, we will delve deeper into one of these proposed practices to better understand how they work and how to use them effectively.

### How To Use a Cursor?

> [Data Selection Process](#Data_Selection_Process) > [Content](#content) > [This section](#cursor)

First of all is important to understand what is a cursor. Cursor is a feature that allows to access to the data from database in a progressive or iterative way. The main goal of a cursor is to help the system maintain the current position within the result set, reading row by row efficiently.

There are two types of cursors: implicit and explicit. The implicit cursors are opened automatically( when some query is executed) and managed by Open SQL. The explicit cursors are open through abap code, using the statement ```OPEN CURSOR```. Then the cursor advances to the next line of the result each time the ```FETCH NEXT CURSOR``` command is used. It is important to note that it is possible to define a package size, which basically means defining the number of rows that will be retrieved in each iteration. Finally, the cursor closed with the ```CLOSE CURSOR``` statement. The explicit cursor are particularly useful when the volume of data is too large to load into memory all at once. The cumulative number of rows retrieved by the ```FETCH``` statement is hold in a system fields ```sy-dbcnt```.


### SQL Query Optimization/Performance Considerations

> [Data Selection Process](#Data_Selection_Process) > [Content](#content) > [This section](#performance)

There are some points that should be taken into consideration in order to build the most optimized select possible.

- Used the ```Where``` statement, to restrict the volume of data retrieved.
- Avoid extra database processing, for example by avoiding the use of statements like ```ORDER BY```, instead prefer to sort the resulting internal table.    
- Use ```FOR ALL ENTRIES``` when some data is already in a internal table. 

-  
falar de colocation grupos, while there may be many ABAP servers.
Avoid using nested SELECT statement and SELECT within LOOPs, better use JOINs or FOR ALL ENTRIES. Use FOR ALL ENTRIES when  the internal table is already there or the end of some processing. Try JOINs if the SELECT are right behind each other.
Use HASHED tables where-ever possible. Otherwise SORTED tables. STANDARD tables should be the last choice.
ver site https://community.sap.com/t5/application-development-blog-posts/optimizing-abap-performance-techniques-for-efficient-code-execution/ba-p/13550016

















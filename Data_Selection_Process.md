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

Adicionar diagrama---- importante


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
| COUNT | ```SELECT COUNT(*) FROM table INTO @data(lv_count)```  | Counts the number of records that match the condition |                   | SUM   | ```SELECT SUM(field) FROM table INTO @data(lv_sum)```  | Calculates the sum of the values of a numeric field   |
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











# [Infotypes](#Infotypes)


## [Content](#content)

- [Infotypes](Infotypes.md):
    - [Definition & Concept](#concept)
      - Understanding the Concept
      - Why Infotypes Exist?
      - Infotypes vs Tables
    - [Structure of an Infotype](#structure)
    - [Types of Infotypes](#types)
       - Personnel Administration (PA)
       - Payroll (PY)
       - Time Management (PT)
       - Organizational Management (OM)
    - [Access & Maintenance](#access)
      - Transactions
      - How to create a infotype?
      - Add Custom Fields to an Infotype


### Definition & Concept

> [Infotypes](#Infotypes) > [Content](#content) > [This section](#concept)

#### Understanding the Concept

An infotype is a group of object-based pieces of information on a particular area. It represents a unit of information in the SAP system, where related data fields are logically grouped together. From a user’s perspective, an infotype acts as an input screen that allows the creation, modification, copying, delimiting, or deletion of employee-related or business data that belongs together.

Technically, an infotype is a data structure. Each record in an infotype is always linked to either the personnel number of an employee or the applicant number of an applicant, ensuring that every record belongs to exactly one employee or applicant. This structure allows SAP to manage and organize employee data efficiently while maintaining accurate associations between records and individuals.

Conceptually, an infotype can be thought of as a folder or form that stores specific information about an employee. Each infotype is identified by a unique four-digit number, which ensures that it can be referenced unambiguously within the system. The number range from 9000 to 9999 is reserved for customer-specific infotypes, allowing organizations to create their own data categories without interfering with standard SAP infotypes.

#### Why Infotypes Exist?

Employee data is extensive, including information such as names, addresses, salaries, benefits, working hours, and skills. Storing all of this information in a single table would be chaotic and inefficient. Instead, SAP splits this giant sheet into logical mini-sheets (infotypes), each handling one category of data. Infotypes exist to organize this data into manageable chunks, with each infotype handling a specific category of information.

This approach provides: 
  - **Clarity** →  users always know where to find a particular type of data, such as an employee’s address in Infotype 0006 or salary details in Infotype 0008
  - **History tracking** → Many infotypes are time-dependent, which allows SAP to store historical data alongside current data, such as previous addresses or past salaries.
  - **Efficiency** → SAP only needs to load the relevant category of data when it is required, avoiding unnecessary processing of unrelated information.

#### Infotypes vs Tables

| Feature | Infotypes | Database Tables |
|---------|-----------|----------------|
| **Definition** | Logical grouping of HR data in SAP, designed for specific business purposes | Physical storage structure in the database storing raw data in rows and columns |
| **Purpose** | Designed for HR processes such as personnel administration, payroll, and time management | Stores data for all kinds of applications; general-purpose storage |
| **User Interaction** | Users interact directly via SAP screens (create, change, view, delete records) | Typically accessed indirectly through programs, transactions, or reports; not user-friendly |
| **Data Organization** | Organized logically by HR category (personal info, payroll, addresses, etc.) | Organized physically as rows and columns, without inherent business logic |
| **Time-Dependence** | Often time-dependent; maintains historical data with validity dates | Not inherently time-dependent; history must be handled manually |

### Structure of an Infotype

> [Infotypes](#Infotypes) > [Content](#content) > [This section](#structure)


Every infotype is assigned a **unique four-digit number**, which is how SAP identifies it internally. This number is not random — it is standardized across SAP systems so that HR data remains consistent.  

**Standard infotypes** use numbers from `0000` to `0999`. Example: `0002 = Personal Data`, `0006 = Address`  
**Customer-specific infotypes** use numbers from `9000` to `9999` . This allows companies to create their own forms when they need additional or specialized data fields.  

Each record within an infotype must be unique. To ensure this uniqueness, SAP relies on certain key fields, the most important being:  

- **Personnel Number (PERNR):** The employee’s unique ID in SAP, which ensures that the data belongs to the correct person.  
- **Validity Dates (BEGDA and ENDDA):** The start and end dates that define when the data is valid. This allows SAP to maintain history without overwriting old records.  
- **Additional Keys (if needed):** Some infotypes allow multiple records at the same time. Example: Infotype `0014 (Recurring Payments and Deductions)` may include different types of allowances. In such cases, a **subtype field** acts as an additional key to distinguish between the records.  

Although users see infotypes as **screens or forms**, the actual data is stored in **transparent tables** in the SAP database. Each infotype has a corresponding transparent table, usually named **PA + infotype number**. Example: Infotype `0002 (Personal Data)` → Table `PA0002`  

We can think of it like this:  
- The **infotype** is the *“front-end form”* that the HR user works with.  
- The **transparent table** is the *“back-end database”* where the actual rows of data are saved.

### Types of Infotypes

> [Infotypes](#Infotypes) > [Content](#content) > [This section](#types)

SAP HCM (Human Capital Management) **manages all aspects of human resources** within an organization. It is divided into several key areas, each focusing on a specific aspect of HR management. These areas work together to ensure that employee data is accurate, organizational structures are clear, time and payroll are correctly managed, and personnel development aligns with company goals. The main areas of SAP HCM include **Personnel Administration (PA), Payroll (PY), Time Management (PT), and Organizational Management (OM).**


#### Personnel Administration (PA)

Personnel Administration focuses on the **core employee master data**. It handles all information that identifies the employee and determines where they fit in the organization. This includes personal details, organizational assignments, addresses, employment contracts, and other payroll-relevant information. Common activities in PA include hiring new employees, recording promotions or transfers, and managing terminations. Essentially, PA acts as the **employee’s central file** in SAP, storing the foundational data required for all HR processes.  

**Important Infotypes:**  
- **0000 – Actions:** Records major HR events like hiring, termination, transfers, promotions, or retirements.  
- **0001 – Organizational Assignment:** Stores the employee’s position, job, cost center, and organizational unit.  
- **0002 – Personal Data:** Contains personal details such as name, date of birth, marital status, and gender.  
- **0006 – Address:** Keeps the employee’s permanent and temporary addresses.  

#### Payroll (PY)

Payroll focuses on **compensating employees accurately and on time**. It relies on information from Personnel Administration and Time Management to calculate salaries, deductions, benefits, and taxes. PY handles gross-to-net calculations, one-time and recurring payments, and ensures compliance with legal and statutory requirements.  

**Important Infotypes:**  
- **0007 – Planned Working Time:** Defines the employee’s work schedule, shifts, and hours.  
- **0008 – Basic Pay:** Stores salary, pay scale group, pay grade, and wage type information.  
- **0014 – Recurring Payments and Deductions:** Captures recurring allowances (e.g., car allowance) or deductions.  
- **0015 – Additional Payments:** Used for one-time payments like bonuses or expense reimbursements.  
- **0169 – Savings Plans:** Stores employee and employer contributions to retirement or savings schemes.  


#### Time Management (PT)

Time Management deals with **working time, absences, and schedules**. It records when employees work, when they are absent, and how their time is allocated. PT tracks attendance, absences, overtime, leave quotas, and work schedules. The data captured in PT is also essential for payroll processing, as it affects wage calculations and benefits.  

**Important Infotypes:**  
- **2001 – Absences:** Records vacation, sick leave, maternity leave, or other absence types.  
- **2002 – Attendances:** Logs worked hours, overtime, or training attendance.  
- **2003 – Substitutions:** Captures temporary role changes, e.g., when an employee covers for a colleague.  
- **2006 – Absence Quotas:** Manages entitlements such as annual leave balances or compensatory time.  


#### Organizational Management (OM)

Organizational Management focuses on the **structure of the company itself**, rather than individual employees. OM defines organizational units, jobs, positions, and reporting relationships, creating the framework into which employees are assigned through PA. It also manages aspects such as planned compensation, cost distribution, and position vacancies. OM acts as the **blueprint of the organization**, providing a clear picture of roles, responsibilities, and reporting lines across the company.  

**Important Infotypes:**  
- **1000 – Object:** Stores the basic information (name, abbreviation, validity period) of an OM object.  
- **1001 – Relationships:** Defines how OM objects are related (e.g., “Position reports to Org Unit”).  
- **1002 – Description:** Holds detailed descriptions of objects such as jobs or positions.  
- **1005 – Planned Compensation:** Stores planned salary data for jobs or positions.  
- **1007 – Vacancy:** Indicates whether a position is vacant.  
- **1011 – Work Schedule:** Defines default work schedules for positions.  
- **1018 – Cost Distribution:** Assigns costs for positions or organizational units to cost centers.  

### Access & Maintenance

> [Infotypes](#Infotypes) > [Content](#content) > [This section](#access)

#### Transactions

#### How to create an infotype

- **1. Access Transaction PM01 and create PS Structure**
Enter transaction code `PM01` in the SAP command field.
Enter a four-digit number for the Infotype (e.g., 9001–9999).
Select **Employee Infotype** and choose **PS Structure**.
Click **Create** to define the structure.
Add the necessary fields (e.g., `PERNR`, `BEGDA`, `ENDDA`, plus any custom fields) and save and activate the structure.
<img width="769" height="565" alt="image" src="https://github.com/user-attachments/assets/fb12f823-1d56-41a3-a4f1-9d21cd78b003" />

- **2. Generate Required Objects**
  Return to the PM01 initial screen.
  Click **Generate Objects** to create the necessary database tables and views.

- **3. Maintain Infotype Characteristics**
  Click on **Infotype Characteristics**.
  Enter the Infotype number and description.
  Set the **Time Constraint** and other relevant attributes.
  Save the entries.
  <img width="798" height="583" alt="image" src="https://github.com/user-attachments/assets/b90aa284-78d7-4d57-940e-6fdd6c6c040b" />


- **4. Maintain Technical Attributes**
  Click on **Technical Attributes**.
  Assign the appropriate database table and structure.
  Save and assign the development to a transport request.
  <img width="718" height="604" alt="image" src="https://github.com/user-attachments/assets/400aa868-3ec2-494d-a11e-5fdadf030115" />


- **5. Modify Screen Layout**
  In PM01, select the **Screen** radio button and enter screen number `2000`.
  Click **Edit** to access the layout editor.
  Make the necessary changes to the screen layout.

- **6. Test the Infotype**
  Use transaction code `PA30` to test the new Infotype.
  Enter an employee number and verify the functionality.


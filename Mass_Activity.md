# [SAP Mass Data Processing and Dynamic Job Launching](#Mass_Activity)


## [Content](#content)

- [SAP Mass Data Processing and Dynamic Job Launching](Mass_Activity.md):
    - [Introduction](#Introduction)
    - [Mass Activity](#Mass_activity)
      - How to Create a Mass Activity? 
      - How to Execute a Mass Activity?
      - How to Monitor a Mass Activity?
    - [Dynamic Job Launching Programs](#parallelization)
      - How to Code Job Launch of a Program Trigger by a Program Father?
      - How to Schedule, Execute, Analyse a Job ( SM36/SM37 )?
    - [Comparison: Mass Activities in SAP vs Custom Job Launching Programs](#comparison)


### Introduction

> [SAP Mass Data Processing and Dynamic Job Launching](#Mass_Activity) > [Content](#content) > [This section](#Introduction)

In this chapter, we will explore possible ways to execute a process/program for a large quantity of data. To do this, we will take a deeper look at two tools, understanding how they work, how to implement and design them, and finally analyze them: Mass Activities and Dynamic Job Launching Programs.


### Mass Activity

> [SAP Mass Data Processing and Dynamic Job Launching](#Mass_Activity) > [Content](#content) > [This section](#Mass_activity)

Mass Activities are a tool used in FI-CA for parallelized job processing in a way that reduce the runtime. This is used in business processed that required a really large volume of data to be processed. A Mass activity split the dataset, according the parallelization object defined, such as business partners or contract accounts, and over several jobs that the system then processes in parallel.

#### How to Create a Mass Activity

The creation of a new Mass Activity is a complex process that requires the creation of **FQEVENTS** to handle intervals and variants, the creation of a **Function Group**, and a transaction that will act as the trigger for the mass activity. It also involves the creation of a **Z-Structure** to define screen parameters, as well as the configuration in **SPRO** for group frames, layout, and the preparation of the Mass Activity.

The steps required in the creation of a new mass activity process are described and explaned in the follow link: 
https://community.sap.com/t5/sap-for-utilities-blogs/create-custom-mass-activity-for-your-program-config/ba-p/13243855


#### How to Execute a Mass Activity

The Mass activities has a uniform layout, which includes the tab pages: 
- General Selections
- Activity-Specific Selections
- Technical Settings
- Logs

In order to execute a mass activity the following steps must be followed:

1- Create a idenifier for the mass activity, for that is require to fill the fields: date ID and Idenfification

![image](https://github.com/user-attachments/assets/b3328917-3eb5-404e-a5cc-6362b8c856c6)

2- In the General Selections, define data to be processed. The parameters shown in this tab is defined in the moment of the parametrizacion of this trasation, and all the fields are include in a struture created for the Mass activity. 

![image](https://github.com/user-attachments/assets/3fd17950-8241-42ba-b7a7-04dc4ec1ff33)

3- Interval Creation or Parallelization of Objects: Parallelization can be either dynamic or static. In dynamic parallelization, the creation of intervals occurs during the execution of the mass activity. The system adapts to the workload and available resources in real-time, adjusting the intervals as needed for optimal performance. In static parallelization, report RFKKDI01 must be scheduled before the execution of the mass activity (daily). A standard variant needs to be created for each combination of object and variant. The size of an interval determines how many objects will be included within that interval.

4- Definition of Technical Settings: This includes defining the number of jobs executed in parallel and the allocation of record intervals. The split is controlled by variants, where you can specify the number and size of intervals, and which key area each interval covers. The block size controls how many selected items are kept in the main memory. In automatic load distribution (where the target host is not specified), the system distributes the number of jobs across the available application servers. In the explicit load distribution method, you can specify the target host for the execution of the jobs.

![image](https://github.com/user-attachments/assets/5c1a5754-5bb4-4e0b-bd24-5178ca02f8ba)

5 - Log Analysis: The job log records status information and any errors that occur during the program run. Some adjustments can be made in the application logs. For example, it is possible to determine the importance a message must have to be included in the application log by specifying the **Problem Class** field. Another related feature is the **expiry date**, which specifies the earliest date on which the log can expire. This is important because from the application log, you can schedule jobs and delete logs from the database once the expiry date has been reached.

   The application logs are displayed in **Enhanced Message Management** (transaction EMMA), where you can create and monitor clarification cases for error messages.

![image](https://github.com/user-attachments/assets/7bc15cd3-d360-4f25-8d1d-7421097cc1b0)

#### How to Monitor a Mass Activity 

The Mass Run Analysis Tool (transaction ST13, also known as MassMan) is a toll that displays the main information about the mass runs, besides of that , the MassMan is also a data collector for SAP Solution Manager business process monitoring, where all the information from MassMan can be reported and used for automatic alerting.

To open MassMan, use transaction ST13 and choose the MASS_MAN_MONITORING application with the F4 help.

![image](https://github.com/user-attachments/assets/77bc08c2-c115-4523-8241-fc3ec3a25aa2)

Is recomended to enter a date( in the startdate parameter ) to ensure that old runs are not included in the list of runs.

As we can see in the image below in one screen we receive an overview of Start-/Endtime of each job, the overall duration the amount of Intervals and Jobs, as well as information about the performance, CPU and DB usage per server, the Counter and the Throughput/hour.

![image](https://github.com/user-attachments/assets/40fad574-dca3-4abe-809a-3e0314547e1b)

The main ideia of the usage of this tool is to speed up the recognition of problems, in order to be able to analyse and define necessary corrective actions depending on the situations. Some possible problems are: Abnormal runtimes (low throughput, expected runtime is out of range); Unusual data volumes; Run cancellations; Processing problems (data quality).


### Dynamic Job Launching Programs

> [SAP Mass Data Processing and Dynamic Job Launching](#Mass_Activity) > [Content](#content) > [This section](#parallelization)

An alternative to creating a Mass Activity to parallelize a process, split the data, and execute it in independent jobs is to create a program that launches the original program in jobs. In this section, we will explore how to implement and manage the process using this solution.


#### How to Code Job Launch of a Program Trigger by a Program Father

In the following example is show how to code a launch of different jobs, dividing the data in different batches. This process include the creacion of variants for each job with the informacion of the parameters requested in the program target, also the creacion of the respective jobs and their activation, according to the maximum number of simultaneous jobs defined.

````abap
    data: mo_job          type ref to zxxx_cl_job_launch,
          lt_params       type ty_rsparams,
          lv_cursor       type cursor,
          lv_package_size type i,
          lt_tb_temp      type table of erch-belnr.

    "the method constructor save the name of the program to launch in a atribute of the class
    mo_job = new zxxx_cl_job_launch( 'ZMIG_R_DATOS_CR' ).

    "save the information if is required to only plan the job or submit aswell
    mo_job->set_submit( abap_false ).

    lv_package_size = p_njob.

    open cursor @lv_cursor for
      select distinct b~belnr
          from [table]

    do.

      fetch next cursor @lv_cursor into table @lt_tb_temp package size @lv_package_size.

      if sy-subrc = 0.
        describe table lt_tb_temp lines data(lv_lines).

        "fill the parameters needed in the program that we need to launch
        loop at lt_tb_temp assigning field-symbol(<lfs_dc>).
        
          append value #( selname   = 'S_DC'
                          kind      = 'S'
                          sign      = 'I'
                          option    = 'EQ'
                          low       =  <lfs_dc> )
                          to lt_params.

        endloop.

        "this method create the variarants calling the function 'RS_CREATE_VARIANT'.
        "For that the name of program, a estructure of the type varid, a table of the paramameters(it_params)
        "and a table of type varit is informed
        mo_job->create_variant( it_params = lt_params iv_control = abap_true ).

        clear lt_params.

        if lv_lines < lv_package_size.
          exit.
          close cursor @lv_cursor.
        endif.

      else.
        exit.
        close cursor @lv_cursor.
      endif.

    enddo.

    " create jobs using the varients created , for that the function 'JOB_OPEN' is called for each of the variants,
    " for that the job name and jobcount is informed. Then the function 'JOB_SUBMIT' is called by informing also
    " the program name, variant, job name, job count and username.
    mo_job->create_job_control( ).

    "verify the number of pending jobs by the status of the variants, get the information of the next job to launch,
    " and with 'JOB_CLOSE' function Close Background Request With COMMIT WORK, in this way the job became active

    while mo_job->get_pending_jobs( ) > 0.
      mo_job->job_control_launch( iv_n_max_jobs ).
    endwhile.
````

In order to know when all planned jobs endend and so the process is finished, the status of the jobs show be verified in the table tbtco. In the following example it is verified that none of the jobs are active and if there are some jobs that are canceled.

```` ABAP

    select single count( * )
        from tbtco
       where ( jobname like @iv_progid
         and jobname ne @iv_progid_var )
         and ( status = 'S'
          or   status = 'R'
          or   status = 'Y' )
       into @data(lv_jobs_activos).

    if sy-subrc ne 0.
      clear lv_jobs_activos.

      select single status
       from tbtco
       where ( jobname like @iv_progid
       and jobname ne @iv_progid_var )
       and status = 'A'
       and sdlstrtdt ge @sy-datum
       and sdlstrttm ge @sy-uzeit
       and sdluname  eq @sy-uname
      into @data(lv_canc).

      if sy-subrc ne 0.
        clear lv_canc.
      endif.
    endif.

````

#### How to Schedule, Execute, Analyse a Job ( SM36/SM37 )

In this section we will explore a little bit the jobs features and how can they can be schedule, create a jobs with multiple steps and how additionaly associate that job with a event. the tutorial below is based on information from the website: 
https://sapcodes.com/2015/11/23/running-background-job-by-triggering-an-event/

**Step1.** Create a program/programs that you need for to execute in the job.

![image](https://github.com/user-attachments/assets/1e7add04-aaf8-4ac8-b4e6-69f270e6135b)

**Step2.** In Tx- SM36 and create a job. Provide the job step- i.e the report that will run when job will be executed. Multiple steps can be define in a single job. 

![image](https://github.com/user-attachments/assets/9ef7b892-84eb-44ee-9c13-6d418be62560)

**Step3.** Go back and Select the Job Start Condition. Here you can define how the job will be trigger.

![image](https://github.com/user-attachments/assets/3a8a9cda-4d14-4242-a3a6-84b8a86e3e02)


**Step4.** In Tx- SM37 you can se the job and the status of that. Here is posible to consult the job logs and the output generated.

![image](https://github.com/user-attachments/assets/c774292b-a7ed-4f4c-b981-239895abff91)

![image](https://github.com/user-attachments/assets/dd984d36-49ed-4e0d-baa6-811eb08c3668)

![image](https://github.com/user-attachments/assets/360e8d33-d34b-48de-b897-1bd484fa5b7e)

Sometimes it is useful to debbug a job to understand the row of problems of some process. In order to debbug a job, create the break point and then in the SM37 select the JOB and in the Command Box write "jdbg".

![image](https://github.com/user-attachments/assets/6f57a6e3-a21c-4160-8096-fc68c886cfec)


### Comparison: Mass Activities in SAP vs Custom Job Launching Programs

> [SAP Mass Data Processing and Dynamic Job Launching](#Mass_Activity) > [Content](#content) > [This section](#Comparison)

In SAP, mass activities and custom job launching programs are both approaches for executing batch jobs or processing large amounts of data. However, they differ in functionality, flexibility, and use cases. In the following table, the advantages and disadvantages of the two options are summarized:

| **Criteria**                      | **Mass Activities in SAP**                   | **Custom Job Launching Programs**          |
|------------------------------------|----------------------------------------------|--------------------------------------------|
| **Development**                    | **No development needed**: Mass activities are part of SAP's standard functionality. You don’t need to write any code or create new programs—everything is pre-built by SAP. | **Requires ABAP development**: Custom programs need to be developed from scratch using ABAP. Developers must design, code, test, and implement the program based on the specific business requirements. |
| **Flexibility**                    | **Limited, restricted to standard SAP processes**: Mass activities have predefined functionality, meaning you can only execute the processes SAP has built. There is limited room for customization beyond standard configurations. | **Highly flexible, handles complex logic**: Since custom programs are developed in ABAP, they can include any type of business logic or data processing. You have complete control over the flow, data handling, and outcomes, allowing for complex scenarios not covered by standard SAP functions. |
| **Performance**                    | **Optimized by SAP**: Since mass activities are designed by SAP, they are performance-optimized. SAP ensures that these processes handle large datasets efficiently, using built-in parallel processing and optimized data handling techniques. | **Performance depends on developer’s coding**: The efficiency of custom job launching programs relies on how well the ABAP code is written. Poorly optimized code can lead to performance issues, especially when handling large volumes of data or complex logic. |
| **Scheduling**                     | **Built-in and easy to manage**: Mass activities often come with standard scheduling tools. You can use SAP’s job scheduling (e.g., via `SM37`) to easily set up and automate the execution of these mass jobs. Mass activities are typically equipped for parallel processing, improving speed and efficiency. | **Requires custom configuration for advanced scheduling**: Custom programs can be scheduled using standard SAP tools, but you may need to add extra coding for advanced scheduling options, such as parallel processing or dependencies between jobs. Developers need to handle specific scheduling logic in the program if needed. |
| **Maintenance**                    | **Low, maintained by SAP**: SAP takes care of updates and bug fixes for mass activities as part of their system updates. This means there is minimal maintenance required on your part, and compatibility with new SAP versions is handled automatically. | **High, requires ongoing maintenance**: Custom programs require ongoing maintenance by the organization’s development team. Any changes to SAP structures, data models, or updates in the SAP system may require modifications to the custom program to ensure it continues to function properly. |
| **Error Handling**                 | **Built-in, standard error logs**: Mass activities have built-in error handling mechanisms. If something goes wrong during processing, SAP provides standard logs and tools to track and correct errors, making troubleshooting easier. | **Requires custom implementation**: Developers must build error handling into custom programs. You have control over how errors are managed, but it requires more effort to ensure logging and error resolution mechanisms are effective. Custom error handling can offer more flexibility but also requires proper design. |
| **Use Case**                       | **Standard processes like mass master data updates or payroll**: Mass activities are ideal for standard, repetitive business processes such as updating master data (customers, materials), executing payroll runs, or posting invoices in bulk. These are common tasks already covered by SAP’s standard functionality. | **Custom processes, integration with custom fields/tables, or specific business logic**: Custom job launching programs are used when your business needs go beyond standard SAP processes. For example, if you need to update custom fields, perform complex calculations, or handle specific business rules that standard SAP cannot accommodate, a custom program would be necessary. |


Mass activities are great if you need a standardized, low-maintenance, and performance-optimized way to handle common business scenarios.
Custom job launching programs offer more flexibility and customization but require more effort to develop and maintain.








 

 













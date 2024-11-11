# [User Dialog](#User)


## [Content](#content)

- [User Dialog](User.md):
    - [Introduction](#introduction)
    - [Selection Screen](#SS)
    - [Dynpros](#Dynpros)
    - [SAP GUI (Graphical User Interface)](#interface)
    - [ALV (ABAP List Viewer)](#alv)
    - [Email](#email)
    - [Handle files](#files)
      

### Introduction

> [User Dialog](#User) > [Content](#content) > [This section](#Introduction)

User dialogs refer to the mechanisms created to facilitate interaction with the user, including interactive interfaces and series of screens. In ABAP, user dialogs are designed to guide users through specific workflows, allowing them to input data, make selections, and trigger various business processes within the system. These dialogs are essential for providing a seamless, user-friendly experience in complex SAP applications.

This chapter focuses on various methods for creating user dialogs, including mechanisms for users to select or load required data during the process. We will also discuss approaches for displaying results and recording logs.


### Selection Screen 

> [User Dialog](#User) > [Content](#content) > [This section](#SS)

Selection screens allow users to enter selection criteria required by the program for it to continue, this is important to 
restrict the amount of data read from the database to the essencial, in this way the performance will be better.
Is posible to translate selection texts into other languages so that they are then displayed in the language in which the user is logged on. The system performs a type check: If you enter a value with an incorrect type, the system displays an error message and makes the field ready for new input. 

In the table bellow, resume the elements that can be used in a selection screen, their function and an example of code in SAP ABAP.


| **Element**        | **Description**                                                                                     | **Example Code**                                          |
|--------------------|-----------------------------------------------------------------------------------------------------|-----------------------------------------------------------|
| **Parameters**     | Single-field input for a single value.                                                              | `PARAMETERS: p_date TYPE d.`                              |
| **Select-Options** | Allows range input (from-to), multiple values, exclusions, or patterns.                             | `SELECT-OPTIONS: s_matnr FOR mara-matnr.`                 |
| **Checkboxes**     | Binary option for yes/no or true/false choices.                                                     | `PARAMETERS: p_check AS CHECKBOX.`                        |
| **Radio Buttons**  | Mutually exclusive options within a group.                                                          | `PARAMETERS: p_option1 RADIOBUTTON GROUP rad1.`           |
| **Pushbuttons**    | Button that triggers actions, linked to `USER-COMMAND` event.                                       | `SELECTION-SCREEN PUSHBUTTON /10(20) btn1 USER-COMMAND cmd1.` |
| **Comment Lines**  | Text labels or descriptions for guiding users on the screen.                                        | `SELECTION-SCREEN COMMENT /1(30) 'Enter Date Range'.`     |
| **Block Groups**   | Logical grouping of fields for organization.                                                        | `SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.`          |
| **Tab Strips**     | Organizes elements into tabs for complex screens.                                                   | `SELECTION-SCREEN BEGIN OF TABBED BLOCK tab1 FOR 10 LINES.` |
| **Drop-Down Lists**| Predefined list of selectable values (list box).                                                    | `PARAMETERS: p_plant TYPE mara-werks AS LISTBOX.`         |


The user can save a specific set of values for the selection screen elements by creating a variant. This way, when starting the program in future executions, the user can load these values from the variant and display them on the selection screen.


#### Selection Screen Events 

Selection screens have specific events that allow developers to manage user interactions, validate input, control screen behavior, and execute code at different stages, ensuring a smooth and dynamic user experience. In the table below, resume


| **Event**                                         | **Description**                                                                                                      |
|---------------------------------------------------|----------------------------------------------------------------------------------------------------------------------|
| `INITIALIZATION`                                  | Triggered before the selection screen is displayed for the first time. Commonly used to set default values for fields. |
| `AT SELECTION-SCREEN`                             | Triggered every time the user interacts with the selection screen (e.g., entering or modifying values). Used for input validation. |
| `AT SELECTION-SCREEN OUTPUT`                      | Triggered before the selection screen is displayed on the screen. Used to dynamically modify screen elements (e.g., hiding/showing fields). |
| `AT SELECTION-SCREEN ON <FIELD/BLOCK/RADIOBUTTON GROUP/END OF SELECT-OPTION>` | Triggered when a specific field, block, radio button group, or select-option range is modified, allowing targeted validation and logic. |
| `START-OF-SELECTION`                              | Triggered after the user clicks "Execute" and input has been validated. This is where the main program logic begins.   |


### Dynpros

> [User Dialog](#User) > [Content](#content) > [This section](#Dynpros)

In SAP ABAP, a Dynpro(short for "Dynamic Program," ) is a type of screen/user interface component that enables user interactions in SAP applications. It’s an integral part of the SAP GUI (Graphical User Interface) and is essential for creating standard SAP transactions. Each Dynpro is part of an ABAP program, consisting of both the visual screen elements and the logic that controls how users interact with these elements. In this way we can say that a Dynpro is made up of two primary components:

1. **Screen Layout (Layout Editor)**:
   This part defines the interactive elements of the user interface, such as input fields, buttons, checkboxes, and tables, that users engage with on the screen. The layout editor in SAP is used to organize and style the layout and appearance of these screen elements.

2. **Flow Logic**:
   Flow logic consists of the ABAP code that governs how the screen responds to user interactions. It includes specific events, like PBO (Process Before Output) and PAI (Process After Input), which determine what actions occur before the screen appears and what happens after user input is received.

#### Dynpro Events

Dynpros are controlled through events that initiate specific actions in the program flow:

- **PBO (Process Before Output)**: This event occurs right before the screen is shown to the user. It’s often used to initialize values or fill in screen fields.

- **PAI (Process After Input)**: This event is triggered after the user interacts with the screen, such as by pressing Enter or clicking a button. At this stage, the data entered by the user is processed.


#### How to Create a Screen

Each screen is assigned a Screen Number, which is used to navigate between screens within the same application.

A Dynpro program typically includes multiple screens that work together, passing data between each other to accomplish various tasks within the application.

There are several ways to create a screen:
- **Forward navigation** -> In the ABAP Editor, double-click the screen number. The system opens the Screen Painter automatically.
- **Object Navigator** -> You can create a new program object screen for your program directly from the object list in the navigation area.

In this images show how can we create a screen:

![image](https://github.com/user-attachments/assets/a029a2da-ec91-400c-aff4-b71cfbf6c86c)
![image](https://github.com/user-attachments/assets/6c65c940-59ed-4df8-a4a1-be04c01e0af7)


the we should create the PBO and PAI includes:
![image](https://github.com/user-attachments/assets/6f15ce1f-8175-4cbb-b882-b306e5e594c4)

In the PBO we should create: 
![image](https://github.com/user-attachments/assets/2f5ed45c-b5b5-42e8-899d-9c58f5f4d7cf)

in "TELA" is posible to choose de buttons that will appear in the menu bar, for instance:
![image](https://github.com/user-attachments/assets/4e2d20f7-de92-4262-865e-dcf11b864638)

In the PAI, we create the code flow for the options available for the user to choose. This can include functions defined in the menu bar, such as "EXIT," as well as other buttons created in the screen layout:
![image](https://github.com/user-attachments/assets/bd3fda05-211d-4de3-aa64-6e27b035f431)

By double-clicking on the screen number and then clicking the "Layout" button, the Screen Painter opens, allowing you to design the screen. Using the Graphical Screen Painter tool, you can easily create various screen elements, such as input and output fields, labels, and borders. 
![image](https://github.com/user-attachments/assets/5f7b6ee7-28a4-4670-899e-2af815e30fdf)


There are two methods for setting field attributes for the screen fields:

1. **Retrieve from ABAP Dictionary**:
   You can import field types and attributes directly from existing ABAP Dictionary structures. This approach provides access to all relevant information about the fields, including semantic details, data element definitions, and foreign key relationships. The field names are also sourced directly from the ABAP Dictionary.

2. **Retrieve from Program**:
   You can also use field attributes from data objects that are already defined within the program. However, for this to work, the program must have an existing compiled version. The name of the data object then serves as the field name on the screen.


### SAP GUI (Graphical User Interface)

> [User Dialog](#User) > [Content](#content) > [This section](#interface)





### ALV (ABAP List Viewer)

> [User Dialog](#User) > [Content](#content) > [This section](#alv)





### Email

> [User Dialog](#User) > [Content](#content) > [This section](#email)

In some contexts, it may be useful to notify a user by email about the result of a process. The code below shows an example of how to create an email body message. In this example, the program is designed to determine the number of print documents created on a specific date and send this information by email to the responsible parties.

```` abap

    types: BEGIN OF ty_email,
             subject TYPE sodocchgi1,
             texts   TYPE STANDARD TABLE OF soli WITH DEFAULT KEY,
           END OF ty_email.

    DATA: lv_text      TYPE soli,
          lv_numero    TYPE char10,
          lv_total     TYPE char10,
          lv_fecha     TYPE string,
          lv_sociedad  TYPE bukrs,
          lv_total_aux TYPE integer,
          ls_email     type ty_email. 


    ls_email-subject-obj_name = 'DIs_email'.
    ls_email-subject-obj_langu = sy-langu.
    ls_email-subject-obj_descr = 'Numero de DIs contabilizados'.

    CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4) INTO lv_fecha SEPARATED BY ' / '.
    CONCATENATE 'Facturas del día:' lv_fecha INTO lv_text SEPARATED BY space.
    APPEND lv_text TO ls_email-texts.
    APPEND ' ' TO ls_email-texts.

    lv_total_aux =  VALUE #( it_di_contab[ sociedad = c_total ]-ndoc OPTIONAL ) .
    lv_total = CONV #( lv_total_aux ).
    CONCATENATE 'Faturados:' lv_total  'DIs' INTO lv_text SEPARATED BY space.
    APPEND lv_text TO ls_email-texts.
    APPEND ' ' TO ls_email-texts.

    LOOP AT it_di_contab ASSIGNING FIELD-SYMBOL(<lfs_di_cont>).
      lv_numero = CONV #( <lfs_di_cont>-ndoc ).
      lv_sociedad = <lfs_di_cont>-sociedad.
      if lv_sociedad <> c_total.
      CONCATENATE '->' lv_sociedad ':' lv_numero  'DIs' INTO lv_text SEPARATED BY space.
      APPEND lv_text TO ls_email-texts.
      APPEND ' ' TO ls_email-texts.
      else.
      APPEND ' ' TO ls_email-texts.
      CONCATENATE 'Total:' lv_numero  'DIs' INTO lv_text SEPARATED BY space.
      APPEND lv_text TO ls_email-texts.
      endif.

    ENDLOOP.

    lv_text = 'Saludo.'.
    APPEND lv_text TO ls_email-texts.

````

For to send the email to the recipients teh function `SO_NEW_DOCUMENT_SEND_API1` can be used: 
```` abap

    data: lt_recipients TYPE STANDARD TABLE OF somlrec90 WITH DEFAULT KEY.
    " Thelt_recipients table is filled with the email recipients.

    CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
      EXPORTING
        document_data              = ls_email-subject
      TABLES
        object_content             = ls_email-texts
        receivers                  = lt_recipients
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        document_type_not_exist    = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        enqueue_error              = 7
        OTHERS                     = 8.

    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
````

The result of this action can be seen in the modulo function `ALINK_CALL_TRANSACTION` for that the code `SOST` shoud entered into the field named `TRANSACTIONNAME` and in debbug the autorizacion check should be jump. 

![image](https://github.com/user-attachments/assets/a9d40f7e-8d7b-4803-9bce-2fdd5e6ce543)

By pressing the "Preview" button, you can see how the email will appear when the program is in IEP mode. In the coded example, the result should look something like this:

![image](https://github.com/user-attachments/assets/01c44d71-d126-4749-a2e5-d3d49ac61ab7)



### Handle files

> [User Dialog](#User) > [Content](#content) > [This section](#files)

ABAP allows file handling across various environments, including directories on the application server, the user’s local computer (presentation server), and through integrations like web services. 

The application server hosts the SAP system itself, making it a suitable place for batch processing or managing large data imports and exports. Transaction AL11 allows you to view the directories available on the application server and manage files within them. Although primarily an administrative tool, it’s helpful for directly inspecting or verifying file structures. Apps like WinSCP and FileZilla are very useful for uploading and downloading files to directories in an easy way.

| **Command**       | **Description**                                               |
|-------------------|---------------------------------------------------------------|
| **OPEN DATASET**  | Opens a file for reading, writing, or appending.              |
| **FOR INPUT**     | Opens a file for reading.                                     |
| **FOR OUTPUT**    | Opens a file for writing (truncates existing content).        |
| **FOR APPENDING** | Opens a file for writing without truncating existing content. |
| **READ DATASET**  | Reads data from an open file.                                 |
| **TRANSFER**      | Writes data to an open file.                                  |
| **CLOSE DATASET** | Closes the file.                                              |

It is possible to create search help when selecting the file path. To do this, you should use the function '/SAPDMC/LSM_F4_SERVER_FILE':

```` ABAP
    constants lc_dir type char9 value 'directory'.

    call function '/SAPDMC/LSM_F4_SERVER_FILE'
      exporting
        filemask         = lc_dir
      importing
        serverfile       = p_ruta
      exceptions
        canceled_by_user = 1
        others           = 2.
    if sy-subrc ne 0.
      clear p_ruta.
    endif.

````

Code example for a file for reading process:

```` abap
    "abrir el fichero
    open dataset lv_fichero for input in text mode encoding default with smart linefeed.
    if sy-subrc eq 0.
      do.
        read dataset lv_fichero into ls_data.
        if sy-subrc eq 0.
          append ls_data to lt_data.
        elseif sy-subrc ne 0.
          exit.
        endif.
      enddo.

      close dataset lv_fichero.
      if sy-subrc eq 0.
        clear lv_error.
      endif.

      if lt_data[] is initial.
        lv_error = abap_true.
      endif.

    else.
      lv_error = abap_true.
    endif.

    if lv_error = abap_false.
      if lt_data[] is not initial.
        loop at lt_data into ls_data.

          split ls_data at c_semicolon
           into ls_fichero-crm_product
                ls_fichero-complemento
                ls_fichero-text30.

          append ls_fichero to lt_fichero.

        endloop.

      endif.

    endif.
````
Code example for a file for writing process:

```` abap
    data: ls_line  type string.
    "file name
    concatenate 'ATUALIZA_AEAT' '_' sy-datum '_' sy-uzeit '.csv' into data(lv_filename).
    data(lv_target) = conv saepfad( p_ruta && '/' && lv_filename ).

    "open file
    open dataset lv_target for output in text mode encoding default.

    if sy-subrc eq 0.
      "header line
      concatenate 'Z_CFACTURA'
                  'STATUS'
                  'MSG'
       into ls_line separated by ';'.
      transfer ls_line to lv_target.

      loop at it_log assigning field-symbol(<fs_line>).

        concatenate <fs_line>-z_cfactura
                    <fs_line>-status
                    <fs_line>-msg
         into ls_line separated by ';'.
        transfer ls_line to lv_target.

      endloop.

      "close de file
      close dataset lv_target.

    endif.

```` 

In contrast, the presentation server refers to the client-side machine where the SAP GUI is accessed, typically the end user’s computer. Reading and writing files on the presentation server is often necessary for user-driven tasks like downloading or uploading files directly from their local system. In this context, to create search help when selecting the file path, the `CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE` method is called. For file uploads, the method used is `CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD`, and for downloads, the method `CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD` is available.



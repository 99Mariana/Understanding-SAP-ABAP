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


# SAP Enhancements


## Content 

- [SAP Enhancements](SAP_Enhancements.md):
    - [What is a SAP Enhancement?](#what)
    - [Enhancement Framework](#framework)
    - [Types of Enhancement Techniques](#types)
    - [User Exits](#Exits)
    - [Customer Exits](#customer)
    - [Business Add-Ins (BADIs)](#Badis)
 

### What is a SAP Enhancement?

> [SAP Enhancements](#SAP_Enhancements) > [Content](#content) > [This section](#what)

SAP enhancements are mechanism that allows the developers to customize and extend standard SAP functionalities without modifying the core system code, that's crucial for adapting the system to specific busniess needs while maintaining the integrity of the original SAP, ensuring smooth upgrades and system stability. 

### Enhancement Framework

> [SAP Enhancements](#SAP_Enhancements) > [Content](#content) > [This section](#framework)

Enhancement Framework ia a paradigm to bring all enhancement techniques under one roof. 
Some important concepts in the Enhancement Framework are:

**Enhancement options**: Can be explicit and implicit. They are defined as positions in repository objects where you can make enhancements. Implicit enhancement options are available directly through the framework, and do not have to belong to a container(an enhancement spot). Explicit enhancement options must be added to the source code. These are created within the original system and must be made visible to developers in other systems using enhancement spots.

**Enhancement spots**:  Are containers for explicit enhancement options.

**Enhancement implementation elements**: Contain the actual enhancement, for example, the source code to be added.

**Enhancement implementations**: Containers for both enhancement implementation elements of both implicit and explicit enhancement options. 

All explicit enhancement options and all enhancement implementation elements must be part of containers, and they must not be grouped together as they belong to different stages of development. In the next image we can visualize the framework for this to types of entities:

   ![image](https://github.com/user-attachments/assets/171380ba-eca1-4236-8e85-c521a4dc7394)


When an enhancement is created the respective container have to be create first, and in a similar way a Badi not exist independently, they are part of an enhancement spot. 


### Types of Enhancement Techniques

> [SAP Enhancements](#SAP_Enhancements) > [Content](#content) > [This section](#types)

There are different types of enhancement techinques:

- **User-Exit** - These are predefined locations in SAP's standard code where custom code can be inserted, implemented as subroutines. Note that a **User-Exit** is considered a modification, not an enhancement, because it involves changing the existing code. 

- **Customer-Exit** -  Are created for specific programs, screens, and menus within standard SAP applications, and allow developers to enhance the SAP standard objects without modifying SAP Object itself. They are created/viewed on t-codes: CMOD/SMOD.

- **Business Transaction Events (BTEs)** - BTEs allow you to attach additional components, such as function modules, in a reusable way. Unlike Customer-Exits, BTEs support multiple types of additional logic by providing an interface that can be used flexibly. Is basically developed for FI module. There are 2 types of BTE interface: Process Interface and Publish and subscribe interface. Is possible to find both in T-code – FIBF

- **Business Add-Ins (BAdIs)** - BAdIs are object-oriented enhancements that allow you to implement custom logic within standard SAP applications. They support multiple implementations with limited filter options, providing a more flexible enhancement method than traditional exits.



### User Exits

> [SAP Enhancements](#SAP_Enhancements) > [Content](#content) > [This section](#Exits)

As we saw before these type of enhancement are code inserted in SAP's standard code, implemented using subroutines. 

The easiest way to identify the user exits is To find a user exit, is using the,  transaction code using SE93 to identify the correspondent SAP program. Then, go to the program's hierarchy and navigate to the subroutines section, where you'll find all available user exits( these subrotines names begins with USEREXIT ). A classic example are the FORM USEREXIT_SAVE_DOCUMENT_PREPARE in Include MF45AFZZ. 

For the step by step in the process of the identification of the user exits, take a look on:
https://sapcodes.com/2015/11/23/how-to-search-user-exit/

For add code in the user exits is required to create a block of rows like we can see in the image below: 
![image](https://github.com/user-attachments/assets/2b2f2e6c-67e2-45d9-922d-67169ee99815)


### Customer Exits

> [SAP Enhancements](#SAP_Enhancements) > [Content](#content) > [This section](#customer)

Customer exit as we see before, is a type of SAP enhancement that allow develops to enhance the SAP standard objects without modifying SAP Object itself.

There are four type of customer exit:
1. **Functional Exit** -  Here we use the statement ```CALL CUSTOMER-FUNCTION i program```, develops to enhance the SAP standard program without modifying SAP Object. Since the custom code written for these Customer-Exits is separated from the original SAP code, maintenance is easier compared to User-Exits. Is important to notice that the exits already exist, there are created by SAP, in order to be possible to modify and add custom code is required to activit the function module exit.  See an example in: https://sapcodes.com/2015/11/20/function-module-exit/
   
2. **Menu Exit** - Provides an option to the customer/user to enhance the menu option on the standard screen. See an example of it on: https://sapcodes.com/2015/11/20/menu-exit/

3. **Screen Exit** - It allows the user to add fields to a screen in the SAP program via sub-screen which is called within standard screen’s flow logic. See an example of it on: https://sapcodes.com/2015/11/20/screen-exit-me21nme22nme23n-header/
   
4. **Field Exit** - Allows for instance to update the description of this standard data element. See an example of it on: https://sapcodes.com/2015/11/20/field-exit/


All Enhancements provided by SAP stored in the table: MODSAP.
![image](https://github.com/user-attachments/assets/8538f710-d1c0-4c2f-9a82-2ed0a64b7dec)


### Business Add-Ins (BADIs)

> [SAP Enhancements](#SAP_Enhancements) > [Content](#content) > [This section](#Badis)












# SAP Enhancements


## Content 

- [SAP Enhancements](SAP_Enhancements.md):
    - [What is a SAP Enhancement?](#what)
    - [Enhancement Framework](#framework)
    - [Types of Enhancement Techniques](#types)
    - [User Exits](#Exits)
    - [Business add-ins (BADIs)](#Badis)
 

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

- **User-Exit** - These are predefined locations in SAP's standard code where custom code can be inserted, implemented as subroutines. To find a user exit, first identify the main program of the transaction code using SE93. Then, go to the program's hierarchy and navigate to the subroutines section, where you'll find all available user exits. Note that a **User-Exit** is considered a modification, not an enhancement, because it involves changing the existing code.

- **Customer-Exit** - Custom code is added inside a function module, referred to as a "Function Exit". Customer Exits is function Exits , here we use the statement Call customer-function. Customer-Exits are created for specific programs, screens, and menus within standard SAP applications. Since the custom code written for these Customer-Exits is separated from the original SAP code, maintenance is easier compared to User-Exits.

- **Business Transaction Events (BTEs)** - BTEs allow you to attach additional components, such as function modules, in a reusable way. Unlike Customer-Exits, BTEs support multiple types of additional logic by providing an interface that can be used flexibly.

- **Business Add-Ins (BAdIs)** - BAdIs are object-oriented enhancements that allow you to implement custom logic within standard SAP applications. They support multiple implementations with limited filter options, providing a more flexible enhancement method than traditional exits.







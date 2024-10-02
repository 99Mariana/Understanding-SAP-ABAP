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


Some important concepts in the Enhancement Framework are:

**Enhancement options**: Can be explicit and implicit. They are defined as positions in repository objects where you can make enhancements. Implicit enhancement options are available directly through the framework, and do not have to belong to a container(an enhancement spot). Explicit enhancement options must be added to the source code. These are created within the original system and must be made visible to developers in other systems using enhancement spots.

**Enhancement spots**:  Are containers for explicit enhancement options.

**Enhancement implementation elements**: Contain the actual enhancement, for example, the source code to be added.

**Enhancement implementations**: Containers for both enhancement implementation elements of both implicit and explicit enhancement options. 

All explicit enhancement options and all enhancement implementation elements must be part of containers, and they must not be grouped together as they belong to different stages of development. In the next image we can visualize the framework for this to types of entities:

   ![image](https://github.com/user-attachments/assets/171380ba-eca1-4236-8e85-c521a4dc7394)


When an enhancement is created the respective container have to be create first, and in a similar way a Badi not exist independently, they are part of an enhancement spot









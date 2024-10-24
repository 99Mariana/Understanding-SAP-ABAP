# [Object Oriented Programming in ABAP](#ABAP_OO)


## [Content](#content)

- [Object Oriented Programming in ABAP](ABAP_OO.md):
    - [Introduction](#introduction)
    - [Key OOP Concepts and Principles](#core)
    - [Instance vs. Static Attributes and Methods](#instance_vs_static)
    - [Static Class vs Singleton Class](#singleton)
 

### Introduction

> [Object Oriented Programming in ABAP](#ABAP_OO) > [Content](#content) > [This section](#Introduction)

Object-Oriented Programming (OOP) in SAP ABAP offers a modern, structured approach to software development, focusing on the organization of data and functionality into reusable components called objects.

In ABAP, OOP enables developers to design software around real-world objects, focusing on encapsulating data (attributes) and behavior (methods) into classes. This approach enhances flexibility and scalability, making it easier to manage large, complex SAP systems. Key principles of OOP, such as inheritance, polymorphism, encapsulation, and abstraction, are fully supported in ABAP, allowing developers to create dynamic, extensible applications that seamlessly integrate with SAP’s core functionalities.

In this section we will take a deep look of the concepts of OOP in ABAP, such as defining classes, creating objects, and understanding important concepts like inheritance, interfaces, and exception handling. We will also explore practical applications, highlighting how OOP in ABAP improves code quality comparing to the tradiotional procedural programing. 

### Key OOP Concepts and Principles

> [Object Oriented Programming in ABAP](#ABAP_OO) > [Content](#content) > [This section](#core)

#### OOP Concepts

**Class** - is a template of objects which can handle more than one characteristics. It defines the properties (attributes) and actions (methods) that the object will have.

**Objects** - is an instance of a class. It represents a specific example of a class with its own unique set of attribute values.

**Attributes** - Attributes are variables,structures or tables, defined in a class to hold the object’s data.

**Methods** - Methods are actions or functions that objects of a class can perform. 


#### OOP Principles

**Encapsulation** - By hiding internal data and exposing only necessary parts of an object, encapsulation ensures that the integrity of the data is protected, reducing the likelihood of unintended side effects. In ABAP, the visibility can be controlled using the PUBLIC, PRIVATE, and PROTECTED sections of a class. 

   - PUBLIC section defines attributes and methods that are accessible from anywhere in the program,  this means that can directly accessed and interacted by the outside. 
   - In the PRIVATE section are defined attributes and methods that are accessible only within the class itself( but not from other classes or programs, not even by subclasses that inherit from the class).
   - Finally the PROTECTED allows subclasses to extend the behavior of the parent class while keeping some control over how the data is accessed, this means that can be accessed by the class itself and any subclasses.

**Inheritance** - It allows one class, known as the child class or subclass, to inherit the attributes and methods of another class, known as the parent class or superclass. This feature promotes code reuse and simplifies the addition of new functionalities
by creating new child classes without modifying the parent class.

```` ABAP
"
CLASS cake DEFINITION.
  PUBLIC SECTION.
    METHODS: start,
             stop.
  PROTECTED SECTION.
    DATA: speed TYPE i.
ENDCLASS.

CLASS cake IMPLEMENTATION.
  METHOD start.
    WRITE: 'Cake starting'.
  ENDMETHOD.
  METHOD stop.
    WRITE: 'Cake stopping'.
  ENDMETHOD.
ENDCLASS.

"This class inherits from cake. It reuses the start and stop methods and defines an additional
" method open_cooker and a new attribute number_of_doors.

CLASS brownie DEFINITION INHERITING FROM cake.
  PUBLIC SECTION.
    METHODS: open_cooker.
  PROTECTED SECTION.
    DATA: number_of_doors TYPE i.
ENDCLASS.

CLASS car IMPLEMENTATION.
  METHOD open_cooker.
    WRITE: 'Opening cooker'.
  ENDMETHOD.
ENDCLASS.

````

**Polymorphism** - Enables objects of different classes to be treated as objects of a common superclass. This is useful when you want to create flexible and generic code. In ABAP, polymorphism is achieved using interfaces and method overriding. his is useful when you want to call the same method on objects from different classes, but expect them to behave differently based on their class.

   - Method Overriding (when a subclass provides a specific implementation of a method that is already defined in the superclass).
     
```` ABAP
 CLASS person DEFINITION.
  PUBLIC SECTION.
    METHODS: move.
ENDCLASS.

CLASS person IMPLEMENTATION.
  METHOD move.
    WRITE: 'The person is moving.'.
  ENDMETHOD.
ENDCLASS.

CLASS woman DEFINITION INHERITING FROM person. 
  PUBLIC SECTION.
    METHODS: move REDEFINITION.
ENDCLASS.

CLASS woman IMPLEMENTATION.
  METHOD move.
    WRITE: 'The woman is walking on the road'.
  ENDMETHOD.
ENDCLASS.

CLASS man DEFINITION INHERITING FROM person. 
  PUBLIC SECTION.
    METHODS: move REDEFINITION.
ENDCLASS.

CLASS woman IMPLEMENTATION.
  METHOD move.
    WRITE: 'The man is walking on the road'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: obj_person TYPE REF TO person,
        obj_woman TYPE REF TO woman,
        obj_man TYPE REF TO man.

  obj_person = NEW person( ).
  obj_person->move( ).  " Calls person's move method

  obj_person = NEW woman( ). 
  obj_person->move( ).  " Calls woman's move method (polymorphism)

  obj_person = NEW man( ).
  obj_person->move( ).  " Calls man's move method (polymorphism)

````

   - Interfaces (which allows different classes to implement the same set of methods in their own way, while still being treated as objects of the interface type).

```` ABAP

INTERFACE if_movable.
  METHODS move.
ENDINTERFACE.

CLASS woman DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_movable.
ENDCLASS.

CLASS woman IMPLEMENTATION.
  METHOD if_movable~move.
    WRITE: 'The woman is moving on the road.'.
  ENDMETHOD.
ENDCLASS.

CLASS man DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_movable.
ENDCLASS.

CLASS boat IMPLEMENTATION.
  METHOD if_movable~move.
    WRITE: 'The man is moving on the road.'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: movable_obj TYPE REF TO if_movable.

  movable_obj = NEW woman( ).
  movable_obj->move( ).  " Calls woman's implementation of move

  movable_obj = NEW man( ).
  movable_obj->move( ).  " Calls man's implementation of move

````

### Instance vs. Static Attributes and Methods

> [Object Oriented Programming in ABAP](#ABAP_OO) > [Content](#content) > [This section](##instance_vs_static)

#### Attributes 

Instance attributes are declared using the DATA command within the class definition block. Each instance attribute declares an independent variable for each created object.

Static attributes are commonly said to belong to classes. These attributes can be used regardless of whether there are objects created or not.

Note that a static attribute is not a constant, as is often mistaken. A constant is an immutable data object, while a static attribute can have its value change normally. However these changes are reflected in all instances of the class because in fact, there is only one variable shared by all objects.


####  Methods









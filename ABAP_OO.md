# [Object Oriented Programming in ABAP](#ABAP_OO)


## [Content](#content)

- [Object Oriented Programming in ABAP](ABAP_OO.md):
    - [Introduction](#introduction)
    - [Key OOP Concepts and Principles](#core)
    - [Instance vs. Static Attributes and Methods](#instance_vs_static)
    - [Advanced OOP Concepts: Final Class & Abstract Class](#advance)
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
by creating new child classes without modifying the parent class. To make a class a child of another, it is necessary to add the `INHERITING FROM` variation in the definition of the child class. Methods defined in the parent class can be called by the child class by using the `SUPER->` statement. 

```` ABAP
"
CLASS cake DEFINITION.
  PUBLIC SECTION.
    METHODS: start,
             stop,
             constructor.
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
  METHOD constructor.
    me->speed = 1.
  ENDMETHOD.
ENDCLASS.

"This class inherits from cake. It reuses the start and stop methods and defines an additional
" method open_cooker and a new attribute number_of_doors.

CLASS brownie DEFINITION INHERITING FROM cake.
  PUBLIC SECTION.
    METHODS: open_cooker,
             constructor.
  PROTECTED SECTION.
    DATA: number_of_doors TYPE i.
ENDCLASS.

CLASS car IMPLEMENTATION.
  METHOD open_cooker.
    WRITE: 'Opening cooker'.
  ENDMETHOD.
  METHOD constructor.
    super->start( ).
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

> [Object Oriented Programming in ABAP](#ABAP_OO) > [Content](#content) > [This section](#instance_vs_static)

#### Attributes 

Instance attributes are declared using the `DATA` command within the class definition block. Each instance attribute declares an independent variable for each created object.

Static attributes are commonly said to belong to classes. These attributes can be used regardless of whether there are objects created or not.

Note that a static attribute is not a constant, as is often mistaken. A constant is an immutable data object, while a static attribute can have its value change normally. However these changes are reflected in all instances of the class because in fact, there is only one variable shared by all objects.


####  Methods

Instance methods are declared using the `METHODS` command in the class definition block and implemented using the `METHOD … ENDMETHOD` block in the implementation block. Such methods can only be used with the reference to an object.

Instance methods can use self-reference (me) to read and change attributes of the object, as well as be used to call other methods belonging to the same object.

To declare static methods, the `CLASS-METHODS` command is used in the class definition block and the `METHOD… ENDMETHOD` block within the class implementation block. Static methods do not need instances created to be executed. However, instances can also make use of them.

#### Example of Instance Attributes and Methods:

```` ABAP

CLASS employee DEFINITION.
  PUBLIC SECTION.
    METHODS: get_name.
  PRIVATE SECTION.
    DATA: name TYPE string. " Instance attribute
ENDCLASS.

CLASS employee IMPLEMENTATION.
  METHOD get_name.
    WRITE name.
  ENDMETHOD.
ENDCLASS.

````
#### Example of Static Attributes and Methods:

```` ABAP

CLASS employee DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: get_total_employees.
  PRIVATE SECTION.
    CLASS-DATA: total_employees TYPE i. " Static attribute
ENDCLASS.

CLASS employee IMPLEMENTATION.
  METHOD get_total_employees.
    WRITE total_employees.
  ENDMETHOD.
ENDCLASS.

````

##### Comparison

| **Aspect**               | **Instance Attributes & Methods**                                         | **Static Attributes & Methods**                                       |
|--------------------------|---------------------------------------------------------------------------|----------------------------------------------------------------------|
| **Definition**            | Attributes and methods that belong to an instance of a class (i.e., an object). | Attributes and methods that belong to the class itself, not to individual objects. |
| **Access**                | Accessed through an object of the class (i.e., `obj->method`).             | Accessed directly through the class without creating an object (i.e., `class=>method`). |
| **Memory Allocation**     | Separate memory is allocated for each object's instance attributes.        | Only one shared memory allocation across all instances of the class. |
| **Scope**                 | Scoped to the specific object (instance). Each object has its own copy.    | Scoped globally to the class. Shared by all objects of the class.    |
| **Lifecycle**             | Exists as long as the object (instance) exists.                           | Exists as long as the program (or class) exists, regardless of object instantiation. |
| **Example (Attributes)**  | `obj->name` (where `name` is an instance attribute).                     | `class=>counter` (where `counter` is a static attribute).            |
| **Example (Methods)**     | `obj->calculate_salary( )` (instance method).                             | `class=>get_total_employees( )` (static method).                     |


### Advanced OOP Concepts: Final Class & Abstract Class

> [Object Oriented Programming in ABAP](#ABAP_OO) > [Content](#content) > [This section](#advance)

#### Abstraction

Abstract classes are simply classes that cannot be instantiated, that is, you cannot use the `CREATE OBJECT` or `NEW` statements to create objects of this class. Note that it is still possible to use a reference to an abstract class. However, when such a reference is filled in, it must point to an object of an inheriting class and of the abstract class.

An abstract method is simply a method that must be redefined in the child class. By definition, an abstract method must belong to an abstract class.

```` ABAP

CLASS lcl_saida_template DEFINITION ABSTRACT.
    PROTECTED SECTION.
    METHODS imprimir ABSTRACT.

ENDCLASS.

CLASS lcl_saida_alv DEFINITION
    INHERITING FROM lcl_saida_template.

    PROTECTED SECTION.
    METHODS imprimir REDEFINITION. "Obrigatorio

ENDCLASS.

CLASS lcl_saida_alv IMPLEMENTATION.
    METHOD imprimir.
        "exibeALV
    ENDMETHOD.

ENDCLASS.

````

#### Final

The concept of finalization is used when one wants to avoid the use of inheritance. Similarly, FINAL methods are those that cannot be redefined.

```` ABAP

CLASS lcl_saida_alv DEFINITION.
    PUBLIC SECTION.
    METHODSformat_columnsFINAL.

ENDCLASS.

CLASS lcl_saida_alv IMPLEMENTATION.
    METHOD format_columns.
        "IS NOT POSIBLE TO REDEFINE THIS METHOD. 
    ENDMETHOD.

ENDCLASS.

```` 


### Static Class vs Singleton Class

> [Object Oriented Programming in ABAP](#ABAP_OO) > [Content](#content) > [This section](#singleton)

A static class is a class that contains only static methods and attributes and cannot be instantiated. That's mean that all the functionality and data within the class are accessed directly through the class itself. Static classes are often used for utility functions, constants, or global methods where object state is irrelevant.

A Singleton class is a design pattern that ensures that a class can only have one instance throughout the lifetime of the application. It is useful when only one object is needed to coordinate actions or store a global state. The instance is lazily created the first time it is accessed, and subsequent calls return the same instance. Singleton classes are used when only one object is needed to manage global resources like logging, configuration settings, or a connection pool. This helps avoid unnecessary duplication of objects, is reused across the application, providing controlled access to global resources.

In a sumarize way we can say that is recommended to use se a Singleton when you need shared, stateful access across the application, particularly when managing resources like configuration settings, logging, or database connections. On the other hand
it may be more advantageous to use a Static Class for stateless utility functions and constants, where no instance-specific state or flexibility is required.

Una example of a singleton creation could be: 

```` ABAP

CLASS logger DEFINITION.
  PUBLIC SECTION.
    " Static method to get the single instance
    CLASS-METHODS: get_instance RETURNING VALUE(instance) TYPE REF TO logger.
    " Method to log messages
    METHODS: log_message IMPORTING message TYPE string.

  PRIVATE SECTION.
    " Private constructor to prevent direct instantiation
    METHODS: constructor.
    " Class attribute to hold the single instance
    CLASS-DATA: singleton_instance TYPE REF TO logger.
ENDCLASS.

CLASS logger IMPLEMENTATION.
  METHOD constructor.
    " Private constructor to control instance creation
  ENDMETHOD.

  METHOD get_instance.
    " Check if the instance already exists
    IF singleton_instance IS INITIAL.
      " If not, create it
      CREATE OBJECT singleton_instance.
    ENDIF.
    " Return the single instance
    instance = singleton_instance.
  ENDMETHOD.

  METHOD log_message.
    " A simple example of logging a message
    WRITE: / message.
  ENDMETHOD.
ENDCLASS.

````

To use the singleton class in ABAP, call get_instance to get the single instance, and then use it to call methods. The usage of the singleton will seens like:

```` ABAP

START-OF-SELECTION.
  DATA(logger_instance) = logger=>get_instance( ).
  logger_instance->log_message( 'First log message' ).

  " Retrieve the same instance again
  DATA(another_logger_instance) = logger=>get_instance( ).
  another_logger_instance->log_message( 'Second log message' ).

  " Both logger_instance and another_logger_instance refer to the same object

```` 










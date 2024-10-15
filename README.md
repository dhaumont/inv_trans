# ECTRANS - Field api interface 

The repository contains a prototype extending ECTRANS with a Field API interface.

The main idea of the interface is to build list of FIELDS_BASIC, and pass them to the INV_TRANS_FIELD_API.

Inside INV_TRANS_FIELD_API routine, a list of FIELD_*RB_VIEW objects is build, on top of the provided FIELDS_BASIC fields.
This allows to manage all the different type of fields (2D, 3D or 4D) in a uniform way, by treating as 1D arrays.

These FIELD_*RB_VIEW are then used as data source to fill the buffers for calling INV_TRANS.

In this preliminary version, field API and Ectrans are mocked-up.


Authors: Philippe Marguinaud and Denis Haumont
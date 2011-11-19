If you look for a quick and easy way to install PropelUtility ...
------------
You are at the right place \o/

If you want to contribute to PropelUtility  ...
------------
You should look at this other repo [https://github.com/mazenovi/PropelUtilityDev](https://github.com/mazenovi/PropelUtilityDev "https://github.com/mazenovi/PropelUtilityDev")

Please PR only on this PropelUtilityDev repo

What is PropelUtility ?
------------
* PropelUtility can create and export to xml format a Propel schema from an existant MySQLWorkbench shcema (*.mwb file) 
* PropelUtility can manage all Propel's schema elements unsupported by MySQLWorkbench
* PropelUtility is delivered as Python MySQLWorkbench plugin. in this it can be installed on Windows, Linux and Mac OS Platform
* PropelUtility has been tested on linux, windows and mac os systems

What PropelUtility is not ...
------------
* PropelUtility can't manage any schema's data which is manageable with MySQLWorkbench
* PropelUtility can't convert Propel schema to YAML format (use symfony propel:schema-to-yml instead)

PropelUtility
================

Installation
------------

via the command line

`git clone git://github.com/mazenovi/PropelUtility.git`

or unzip the downloaded archive downloaded " from PropelUtility's github page
  
* open MySQLWorkbench 
* go to `"Scripting" -> "Install Plugin / Module ..."` browse to the file `propel_utility_grt.py` you just gited or unziped, select it
* restart MySQLWorkbench

PropelUtility add two new entries to MySQLWorkbench in `"Plugins" -> "Catalog"` :

* `"Propel Utility"`
* `"Propel Erase All Data"`

usage
------------

Any changes on the Propel data with Propel Utility will be validated after a click on the "OK" Button.

If "CANCEL" is clicked, changes will be lost.

To definitely save the Propel data, you should remember to save the .mwb file
    
There are 3 type fields:

* the non-editable fields : they are for information only, MySQLworkbench can edit them but not PropelUtility
* the editable fields : on click on those fields make them editable and you can change their value with a keyboard input
* the editable fields with choices list : on click on those fields make them editable and you can change their value with a keyboard input, and a double click will show a list with all possible choices. Just select an item and click the "select this value" button
N.B. it seems that the choices are unavailable on linux.

tabs
------------
* Database can manage attributes for [`<database />`] (http://www.propelorm.org/reference/schema.html#database_element) xml tag  
* Tables can manage attributes for [`<table />`] (http://www.propelorm.org/reference/schema.html#table_element) xml tag 
* Columns can manage attributes for [`<column />`] (http://www.propelorm.org/reference/schema.html#column_element) xml tag 
* Foreign Keys can manage attributes for [`<foreign-key />`] (http://www.propelorm.org/reference/schema.html#foreignkey_element) xml tag 
* Indices show only attributes for  [`<index />`] (http://www.propelorm.org/reference/schema.html#index_element) et [`<unique/>`] (http://www.propelorm.org/reference/schema.html#unique_element) xml tags
* Behaviors can manage Behaviors for each table ([`<behavior />`] (http://www.propelorm.org/cookbook/writing-behavior.html) xml tag) and all associated parametrs ([`<parameter />`] ((http://www.propelorm.org/cookbook/writing-behavior.html)) xml tag)
* Export can export data managed with previous tab with this rule: required attributes are always exported, and optional attributes will be exported only if associated value is different from defaut value.

Propel Erase all data
================

This entry can erase all Propel data managed by PropelUtility and only this data (MySQLWorkbench data will not be affected)
# LambdaWerk backend developer test

## The task

There is a PostgreSQL table of persons (person), uniquely identified
by their first name (fname), last name (lname) and date of birth
(dob).  Every person has a telephone number (phone).

This table needs to be updated from an XML file containing elements of
the form

```
<member>
 <firstname>JOHN</firstname>
 <lastname>DOE</lastname>
 <date-of-birth>2002-02-01</date-of-birth>
 <phone>9548938821</phone>
</member>
```

If the phone number is already correct, nothing should be changed in
the database.  If a person record does not exist, it needs to be
created.

The person database table contains 10 million rows.

The update file contains 1.5 million entries.

## Objective

 - Write clean code that performs the operation correctly
 - Provide basic loading statistics at the end of the operation
 - Use proper mechanisms to process the input file
 - Find ways to minimize the overall run time of the merge process
 - Reason about performance and memory usage

The number of records in the sample database and the input file are
meant to reflect the number of records in a production system.  A
production system would have more individual fields per person,
consider that when choosing an implementation strategy.

## Files

The file person.sql.gz contains a database dump of the person table
which can be imported into PostgreSQL.

The file update-file.xml.gz contains the XML input file to be merged
into the database.

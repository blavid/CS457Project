CS457Project
============

A programming project in Haskell by Blake Wellington and Robert Konell

The aim of this project is to use the Haskell programming language to 
create a web-based application. It is intended as a learning exercise
with the following project goals:

* Learn functional programming with Haskell
* Learn how to use a REST API as a web service
* Learn how to use Haskell libraries to present the data
  in a meaningful way
  
The application will use data from Tri-Met's database. (Tri-Met is a 
the public transit system in Portland, Oregon). The database contains
route information for all of Tri-Met's trains and buses.

Since we are new to Haskell and have limited time to work on this project,
we have broken the project down into manageable, intermediate goals.

Initial Goal
------------
The initial goal of the project is to successfully pull a single, small
piece of information from the database, massage it in Haskell, and present
it on a web page. This may be something as basic as showing the next bus/train
to arrive at a given stop number.

Secondary Goal
--------------
Once the initial goal is in place, we will expand the user interface to contain
ever more complex query abilities.  For example, we might add the ability to 
select a certain route, list the stops of a route, estimate the travel time
between two stops, plan a trip (including multiple transfers), etc.

Final Goal
----------
Since this is a web-based application, it would be nice to have it hosted on
a public server. The final goal is to be able to pack all the code up and 
install it on a server in the cloud (such as Amazon EC2). This will involve
installing Haskell (or just shipping the compiled version) on a server 
equipped with Apache web server.

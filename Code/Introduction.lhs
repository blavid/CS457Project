CS457


Project Report
Authors: Robert Konell, Blake Wellington


Project: Trimet Web Application
Github: https://github.com/blakewellington/CS457Project

-----------------------------
Introduction:
-----------------------------

The aim of this project was to use the Haskell programming language to create
a web-based application. It was intended as a learning exercise with the
following project goals:

 * Learn functional programming with Haskell
 * Learn how to use a REST API as a web service
 * Learn how to use Haskell libraries to present the data in a meaningful way

The application uses data from Tri-Met's database. (Tri-Met is a the public
transit system in Portland, Oregon). The database contains route information
for all of Tri-Met's trains and buses.  

-----------------------------
Technology:
-----------------------------

The application is written in Haskell.  A number of 3rd party libraries 
are used, in particular:

  * Happstack - The web server
  * Aeson - JSON parsing
  * Conduit - HTTP calls

The cabal package system is used to download the required modules.  Once
everything is in place, the application is then combiled using ghc.

Other technology used is HTML and Javascript, for the client side
of the application.  

The source code is all in literate Haskell format, and the introduction
and conclusion together with all of the source is considered the project
report.  

The data for the application is taken from Trimet's API.  The API is 
accessed through URL strings, and there are 5 web services open to
applications.  These include: Arrivals, Detours, RouteConfig, Stop Location
Trip Planner, and Vehicles.  Each web service takes a unique number
of arguments, and returns back either JSON or XML.

-----------------------------
Project Life Time (better name?)
-----------------------------

Prior to any coding, a timeline was agreed upon by both students.  Project
goals had also been decided on.  Since this was only a 4 week project, a 
simple set of Trimet's API was chosen as particular goals.  If time
permitted, it was planned to tie the data from these services into
unique tools.

The first two weeks there wasn't a whole lot of progress.  Robert was
able to get Happstack running on his local environment and proofs 
of concept developed.  Blake was able to get the Aeson module loaded
and working for simple examples on his local environment.  When 
code from both was tried to tie together however, numerous issues
were encountered with packages being incompatible.  

It was decided that they would need to have a singular development
environment correctly configured for all of the different libraries.
A Digital Ocean server was used for this, and finally a good configuration
was settled on.  Over a week of time was lost by this time however.

Another huge ordeal came with constructing the data types for
parsing the JSON.  The complexity of the JSON returned from the 
web service produced a lot of issues, and it wouldn't be until 
another week that they were all finally wrinkled out. 

=================================
TO BE CONTINUED
==================================

-----------------------------
Design:
-----------------------------

The core of the application is WebServer.lhs, which is where the Main
module lives.  The main function calls the primary Happstack function
and listens to a port set in the configuration (default: 8000). When
a HTTP GET is received, the URL is parsed, and appropriate functions
handle the request.  The request will be for either:

  * A static page for using the application
  * The results of a query from one of the static pages
  * An unexpected URL

For the first case, a page is generated with proper UI elements to
use the web service.  For example, arrivals has a text box and a button
which are used to query a stop id.  

For the second case, the URL contains the stop id's requested, which
are passed to the HTTP service caller.  The JSON returns is then 
parsed into the Haskell data types developed to hold the data.  This
is then passed to the appropriate HTML builder functions which return
a Text object that the web server then packages up and returns back
to the user as a response.

In the third case, an error page will display.

The next part of the application is the TrimetFunctions.lhs.  It is
here that the appropriate URL strings are built for accessing the 
Trimet API.  Needed data is received from the web server, and the
URL is constructed from there.  The arguments in particular are
detailed with the TrimetFunctions module.

TrimetDataTypes.lhs contains two parimary types of definitions.
First, data types which match the JSON structure.  These data
types are multi-layered, with a top level "ResultSet", which may
contain a location, may location arrival data, and contain a query
time.  Both location and arrival have their own data type.  Arrival
in fact has a couple more layers of data types before that.  Full
details of these data types are covered in the TrimetDataTypes module.
The second type of definition included are instances of the FromJSON
type class.  Each custom data type needs an instance of FromJSON, 
which holds the definition for parsing into that data type.  Further
details for these definitions will also be included in the 
TrimetDataTypes module.
 

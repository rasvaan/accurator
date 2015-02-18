# Accurator Quick Start Guide #


Accurator is an annotation system running on Cliopatria powered by Prolog. Below the instructions for setting up Accurator on a mac or linux machine.

## Setup webserver ##


Install latest SWI-Prolog development release: http://www.swi-prolog.org/download/devel

Clone the Cliopatria webserver code in a suitable directory:
```bash
$ cd git
$ git clone git@github.com:ClioPatria/ClioPatria.git
```

Create a folder for the accurator server:
```bash
$ cd ..
$ mkdir accurator
$ cd accurator
```

Now configure this directory as a ClioPatria project by running:
```bash
$ ../git/ClioPatria/configure
```

You can now run the web server using the command below (typing 'halt.' will stop it):
```bash
$ ./run.pl
```
   
## Setup accurator package ##

Start the webserver and install the required cpacks:
```bash
$ ./run.pl
?- cpack_install(accurator).
```

Congratulations! If everything went well you now have a server with accurator functionallity running at: localhost:3020/intro.html

## Load data ##

Create a folder for the rdf data:
```bash
$ cd ..
$ ls
  ClioPatria  accurator
$ mkdir rdf
$ cd rdf
```

Clone a repository with RDF and a void file describing the data and load the data after starting the server. Here we download Naturalis data, were there are multiple subsets to choose from:
```bash
$ git clone https://github.com/rasvaan/naturalis.git
$ cd ..
$ ./run.pl
?- rdf_library:rdf_attach_library('../rdf/naturalis/').
?- rdf_library:rdf_list_library.
```

A list of datasets to load is shown. You can for example choose to load a vocabulary and print collection: 
```bash
?- rdf_library:rdf_load_library('IOC-birdlist-en-nl',[]).
?- rdf_library:rdf_load_library('Naturalis-prints',[]).
```
With the data loaded you should be able to annotate some artworks!

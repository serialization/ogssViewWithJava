# ogssView
A reflection-based viewer and editor for binary ogss files.

Usage
-----

from repository:

sbt run

build:

sbt assembly

java -jar target/scala-2.12/ogssView.jar [file.sg]?

The optional first parameter will be loaded on start-up.
This behaviour can be used to make ogssView the standard tool to read binary skill files.

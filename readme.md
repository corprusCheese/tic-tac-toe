<h1>Tic-tac-toe</h1>

запускать sbt run

запускать тесты sbt test

<h2>Routes</h2>

GET -> /board -> returns json like 
    
    { "turn": ???, "result": ???, "field": ??? }

POST -> /board -> you need to send 

    { "x": int value, "y": int value }
 
then it returns json with changed board 

    { "field": ??? }

for example:

    curl --request POST http://127.0.0.1:8080/board --data '{"x": 0, "y": 0}' 

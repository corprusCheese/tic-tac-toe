<h1>Tic-tak-toe</h1>

запускать sbt run

запускать тесты sbt test

<h2>Routes</h2>

GET -> /board -> returns json like 
    
    { "turn": ???, "result": ???, "field": ??? }

POST -> /board -> returns json with board (if you send parameters x and y in your post query, if you don't - just returns you a board like nothing happens)

    { "field": ??? }

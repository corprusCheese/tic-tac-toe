<h1>Tic-tac-toe</h1>

запускать sbt run, тесты sbt test

в этой ветке на текущий момент проект разделён на два приложения

- игра tic-tac-toe
- чат на вебсокетах

в heroku пока что крутится старая версия проекта, когда я не делил на несколько приложений, хочу еще сделать это все в докере, ибо там появился постгрес, который я использую для запоминания в бд игр с результатом

https://peaceful-depths-92861.herokuapp.com - старая мастер версия


плюс было бы норм немного уменьшить класс ws.api.services.ChatService

алсо для запуска в resources.config нужно создать файл reference.conf, пример такого файла:
    
    tictactoe {
        dbConnectUrl: "jdbc:postgresql://127.0.0.1:5432/tic-tac-toe"
        dbUser: "postgres"
        dbPassword: "password"
    }

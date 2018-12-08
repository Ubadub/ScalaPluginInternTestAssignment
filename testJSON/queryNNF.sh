curl -X POST -d @$1 http://localhost:8080/NNF --header "Content-Type: application/json" | python3 -m json.tool

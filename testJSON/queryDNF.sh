curl -X POST -d @$1 http://localhost:8080/DNF --header "Content-Type: application/json" | python3 -m json.tool

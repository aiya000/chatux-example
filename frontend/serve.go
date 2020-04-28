package main

import (
    "log"
    "net/http"
)

func main() {
    http.Handle("/", http.FileServer(http.Dir(".")))
    log.Println("Listening on :10252...")
    http.ListenAndServe(":10252", nil)
}

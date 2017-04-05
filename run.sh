#!/bin/bash

loc="$PWD"
cd "$loc"

gnome-terminal -e 'bash -c "service mongod start; exec bash"'
sleep 5

gnome-terminal -e 'bash -c "cd '$loc'/DirServer;echo ---- DirServer ----;stack exec DirServer-exe; exec bash"'

gnome-terminal -e 'bash -c "cd '$loc'/FileServer;echo ---- FileServer ----;stack exec DFS-exe; exec bash"'

gnome-terminal -e 'bash -c "cd '$loc'/AuthServer;echo ---- AuthServer ----;stack exec AuthServer-exe; exec bash"' 

gnome-terminal -e 'bash -c "cd '$loc'/Client;echo ---- Client ----;stack exec Client-exe; exec bash"'

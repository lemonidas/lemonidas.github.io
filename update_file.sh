#!/bin/bash

if [ $# -eq 1 ]
then
    scp -r $1 llamp@eniac.seas.upenn.edu:./html/$1
else 
    scp -r $1 llamp@eniac.seas.upenn.edu:./html/$2
fi

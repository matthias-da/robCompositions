
#! /bin/bash 
if LC_ALL=C grep -q '[^[:print:][:space:]]' socExp.csv; then
echo "STOOOOOPPPPPPPP socExp"
else
echo "file contains ascii characters only"
fi

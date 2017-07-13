
#! /bin/bash 

for file in vignettes/*.bib
do
if LC_ALL=C grep -q '[^[:print:][:space:]]' "$file"; then
echo "STOOOOOPPPPPPPP"
else
echo "file contains ascii characters only"
fi
done


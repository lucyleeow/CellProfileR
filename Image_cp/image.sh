#!/bin/bash -l


# imagedir=""
# copyto=""
# date="$1"
# well="$2"

# cd imagedir

# searchterm="MFGTMPcx7_${date}*_${well}*"

# echo "$searchterm"

# cp MFGTMPcx7_181122* copty

# searchterm="test*"

# echo $searchterm

while IFS=$'\n' read -r line_data; do
    myarray=(${line_data})
    echo ${myarray[0]}
    echo ${myarray[2]}
done < ./test2.tsv



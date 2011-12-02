#!/bin/bash

# Prepare different zip programs
(cd gzip && chmod 700 init_script.sh && ./init_script.sh)
(cd pigz && chmod 700 init_script.sh && ./init_script.sh)
(cd pgzip && make)

# Make random files of a size, and compress/decompress it with all zip formats
function testZips(){
    if [ ! -f "1MB.txt" ]; then
        dd if=/dev/random of="1MB.txt" bs=$(( 1024 * 1024 )) count=1
    fi
    

    if [ ! -f ${1}MB.txt ]; then
        touch ${1}MB.txt
        COUNTER=${1}
        until [ $COUNTER -lt 1 ]; do
            cat "1MB.txt" >> ${1}MB.txt
            let COUNTER-=1
        done
    fi
    
    cp ${1}MB.txt TempFile
    
	echo "pigz ${1}"
    echo "Times for compressing ${1} MB with pigz" >> tso.txt
	(/usr/bin/time  pigz/pigz -b 32 -f -k ${1}MB.txt)  2>> tso.txt
	
	rm ${1}MB.txt.gz
	cp TempFile ${1}MB.txt
	
	echo "pgzip ${1}"
    echo "Times for compressing ${1} MB with pgzip" >> tso.txt
	(/usr/bin/time  pgzip/pgzip ${1}MB.txt -b 128000 -f) 2>> tso.txt # Note 128 kb chunks is the default for pigz
	
	rm ${1}MB.txt.gz
	cp TempFile ${1}MB.txt
	
	echo "quickZip ${1}"
    echo "Times for compressing ${1} MB with quickZip" >> tso.txt
    let "size=${1}/8"
	(/usr/bin/time  python quickzip/quickzip.py ${1}MB.txt $size) 2>> tso.txt
	
	rm -rf temp   # Since quickzip makes a temp directory
	
	echo "gzip ${1}"
	echo "Times for compressing ${1} MB with old gzip" >> tso.txt
	(/usr/bin/time gzip/gzip -f ${1}MB.txt) 2>> tso.txt

    rm TempFile
    rm ${1}MB.txt.gz
    rm "1MB.txt"
}

if [ -f "tso.txt" ]; then
  rm tso.txt  
fi

testZips 20
testZips 100
testZips 250
#testZips 500
#testZips 750
#testZips 1000
#testZips 2000

echo "Output Written to tso.txt"

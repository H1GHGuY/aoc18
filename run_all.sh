#! /bin/bash

TARGETS="One OneTwo Two TwoTwo Three ThreeTwo Nine NineTwo"
P=_build/default/

for target in $TARGETS
do
  dune build $target.exe
done

echo "1"
./$P/One.exe < input_1.txt
echo -e "\n1-2"
./$P/OneTwo.exe < input_1.txt
echo -e "\n2"
./$P/Two.exe < input_2.txt
echo -e "\n2-2"
./$P/TwoTwo.exe < input_2.txt
echo -e "\n3"
./$P/Three.exe < input_3.txt
echo -e "\n3-2"
./$P/ThreeTwo.exe < input_3.txt
echo -e "\n9"
./$P/Nine.exe
echo -e "\n9-2"
./$P/NineTwo.exe

#!/bin/bash

rm -r target/deploy/routine-sequence-qc 2> /dev/null
mkdir -p target/deploy/routine-sequence-qc
mkdir -p target/deploy/routine-sequence-qc/js

cp -r resources/public/css target/deploy/routine-sequence-qc
cp -r resources/public/images target/deploy/routine-sequence-qc
cp resources/public/favicon.ico target/deploy/routine-sequence-qc/favicon.ico
cp resources/public/js/main.js target/deploy/routine-sequence-qc/js/main.js
cp resources/public/index.html target/deploy/routine-sequence-qc/index.html
pushd target/deploy > /dev/null
tar -czf routine-sequence-qc.tar.gz routine-sequence-qc
# rm -r routine-sequence-qc
popd > /dev/null

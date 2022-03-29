#!/bin/bash

folder=$(date +%Y-%m-%d)
mkdir -p releases/$folder/IQSlides

# Copy IQSlides package to release folder
cp -r R releases/$folder/IQSlides
cp -r inst releases/$folder/IQSlides
cp -r man releases/$folder/IQSlides
cp DESCRIPTION releases/$folder/IQSlides
cp NAMESPACE releases/$folder/IQSlides
cp LICENSE releases/$folder/IQSlides

# Build tar.gz
cd releases/$folder
R CMD build IQSlides
rm -r IQSlides
cd ../..

# Build test
mkdir releases/$folder/IQSlides
cp -r tests/testdata releases/$folder/IQSlides
cp -r tests/testthat releases/$folder/IQSlides

echo 'testLogFile <- "/IQDESKTOP/.validation/testlog_IQSlides.txt"' > releases/$folder/IQSlides/test_IQSlides.R
echo 'sink(file=testLogFile)' >> releases/$folder/IQSlides/test_IQSlides.R
echo 'testthat::test_dir("/tmp/05_validation/IQSlides/testthat")' >> releases/$folder/IQSlides/test_IQSlides.R
echo 'sink()' >> releases/$folder/IQSlides/test_IQSlides.R

cd releases/$folder
zip -r IQSlides.zip IQSlides
rm -r IQSlides
cd ../..


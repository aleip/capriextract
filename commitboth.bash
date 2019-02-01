message=$2
echo $message

# First commit all 
git commit -a -m "${message}"

cd ../logfiles
git commit -a -m "${message}"
cd ../capriextract



# Then pull to make sure there is no problem
. pullboth

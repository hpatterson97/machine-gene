#command to push new code
git add  .
git commit -m "wrtoe beginnings for function for 2D rejection sampling"
git push origin workingbranch

#git branch checks the branch your in

#note - change comit message each time
#check it got pushed on to git hub

# after you code has been updated in gihub- this pulls it back into R
git checkout master
git pull --rebase origin master
git checkout workingbranch
git rebase master


#terminal commands
ls list files
cd


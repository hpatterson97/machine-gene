#command to push new code
git add  .
git commit -m "added example for 2D rejection sampling, and oxygenised"
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


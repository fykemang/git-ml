# cs3110-a6
Midterm Project

------ INSTALL GUIDE ------

The guide is built to be run on the CS 3110 Ubunutu VM, or an Ubuntu distro. 
This might work on OS X, we give no assurances. Do not try this with windows. This is far more likely to work on your toaster
than on a DOS based OS

Instructions:

Unpack the zip into ~/cs3110-a6
run the command: make build
=> this will build the binaries into ~/cs3110-a6/_build
run the command: chmod +x vm-git-ml.sh
run the commmand: ./vm-git-ml.sh
=> this will install a bash script (git-ml) that will allow you to run git-ml from anywhere, as long as the complied binaries are in ~/cs3110-a6
=> this is the part that may required modification to run on OS X, the bash script is really not that complicated, if you really want to run this on OS X and not the VM, modification should be easy


Troubleshooting:
You may need to run: chmod +x main.byte 
in the ~/cs3110-a6 directory.
This shouldn't be nessesary, but if you are having issues, try it.

~~ Now git-ml is installed ~~

Head over to any directory and run the command: git-ml init 

Here are some useful features and commands to explore:

git-ml add [file path]
git-ml commit -m "[commit message]"
git-ml checkout -- [folder path]
git-ml checkout -b [branch name]
git-ml rm [path to a file]
git-ml status
git-ml diff
git-ml log
git-ml hash-object [file path]
git-ml cat-file [md5 hash pointer]

Here are some mini tutorials:





__Working with branches__

to switch to an existing branch, or create a new branch use

git-ml checkout -b new_branch

=> this will create new_branch and switch to it

This branch will have a seperate ref tree, make changes and commit the freely. Do 

git-ml checkout -b master

to go back to master. Any removed files, or any changes will be restored. That is a useful feature for the user. 





__status and diffs__

Go ahead and add some files with 

git-ml add [filename/file folder]

Then go ahead and commit your changes with

git-ml commit -m "testing status"

You can use the log command to see your commit

git-ml log

Now add some more files with,

git-ml add [some other file/file folder]

now use status to do

git-ml status

=> this will show the files that are yet to be commited
You cannot do status before you commit, because status does not have a commit 
to compare against
Go ahead and commit these more files with

git-ml commit -m "some more files"

Now edit one or more of these files that you have commited. Now you can do try to get the status again with

git-ml status 

=> this will show which files have been changed since the last commit, and files not yet commited
If you want to get a closer look at the differences in particular you can do

git-ml diff 

=> this will show additions, removals, and things that stay the same in every modified file. 





__branching and checkout__

Before trying checkout, try the following 

git checkout -- [some path to some folder]

=> this command will restore the contents of the folder to the latest commit but will not remove new files, it will overwrite changed files. This is a useful version control feature.
Try making some changes to some files you commited then try the previous command. Try deleting files, try modifing files. You will see the changes from the commit overwrite your changes.
Now we can explore branching. Try

git checkout -b newbranch

Make some changes, stage and commit those changes, do git-ml log to see your ref tree. 
Delete some files, rewrite some files, stage and commit those changes. Then try

git checkout -b master

Watch in wonder as your repo is restored to what it used to be. 












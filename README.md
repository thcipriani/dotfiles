DotFiles
=============
A Tyler Cipriani Joint.
-----------
**UPDATED FEAUTRE:** Awesomer than the combined powers of a ninja, Batman and Santa Claus!

**Prerequisites**
+ zsh
+ vim 7.3+
+ ssh
+ git-core
+ rvm

Install Instructions
-------------
1. Change to your home directory <code>cd ~</code>
2. Clone this repo into a directory named <code>dotfiles</code>:
<pre>
git clone http://github.com/thcipriani/dotfiles.git ~/dotfiles
</pre>
3. Make sure that <code>bootstrap.rb</code> in the <code>dotfiles</code> directory has the proper permissions and run that file:
<pre>
cd ~/dotfiles
chmod +x bootstrap.rb
./bootstrap.rb
</pre>
4. Grab all of my [pathogen](https://github.com/tpope/vim-pathogen "Pathogen: Really Awesome!") plugins by updating all of the submodules of this repo:
<pre>
cd ~/dotfiles
git submodule init
git submodule update
</pre>

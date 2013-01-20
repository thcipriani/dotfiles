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
+ tmux
+ Exuberant Ctags (>=5.5)
+ <a href="https://github.com/erkin/ponysay" target="_blank">ponysay</a>

Vim Features (just need to remind myself&#8230;)
-------------
+ Pathogen
+ <a href="https://github.com/scrooloose/syntastic" target="_blank">Syntastic</a>&#8212;syntax checking
+ <a href="http://www.vim.org/scripts/script.php?script_id=1929" target="_blank">DBGp client</a>&#8212;xDebug w/php
+ NerdTree w/NerdTree Tabs
+ Snipmate
+ EasyMotion
+ Surround
+ Powerline
+ Tagbar
+ CtrlP

Install Instructions
-------------
1. Change to your home directory <code>cd ~</code>
2. Clone this repo into a directory named <code>dotfiles</code>:
<pre>
git clone http://github.com/thcipriani/dotfiles.git ~/dotfiles
</pre>
3. Grab oh-my-zsh! and all of my [pathogen](https://github.com/tpope/vim-pathogen "Pathogen: Really Awesome!") plugins by updating all of the submodules of this repo:
<pre>
cd ~/dotfiles
git submodule init
git submodule update
</pre>
4. Make sure that <code>bootstrap.rb</code> in the <code>dotfiles</code> directory has the proper permissions and run that file:
<pre>
cd ~/dotfiles
chmod +x bootstrap.rb
./bootstrap.rb
</pre>

This is not the dotViM repo you're looking for. Move along.
=============
A Tyler Cipriani Joint.
-----------
**Features**
+ Powerline
+ Numbers (for ViM 7.3+)
+ Solarized
+ **UPDATED FEAUTRE:** Awesomer than the combined powers of a ninja, Batman and Santa Claus!

Install Instructions
-------------
1. Change to your home directory <code>cd ~</code>
2. Clone this repo into the <code>.vim</code> directory:
<pre>
git clone http://github.com/thcipriani/dotvim.git ~/.vim
</pre>
3. Create a link to the <code>vimrc</code> file in the <code>.vim</code> directory to the <code>.vimrc</code> dotfile in your home directory:
<pre>
ln -s ~/.vim/vimrc ~/.vimrc
</pre>
4. Grab all of my [pathogen](https://github.com/tpope/vim-pathogen "Pathogen: Really Awesome!") plugins by updating all of the submodules of this repo:
<pre>
cd ~/.vim
git submodule init
git submodule update
</pre>

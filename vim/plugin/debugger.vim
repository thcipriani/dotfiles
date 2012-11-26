" DBGp client: a remote debugger interface/client to the DBGp protocol
"
"=============================================================================
" Script Info and Documentation
"=============================================================================
" Name Of File: debugger.vim, debugger.py
"  Description: remote debugger client/interface to DBGp protocol
"   Maintainer: Hadi Zeftin <slack.dna <at> gmail.com>
"  Last Change: January 7, 2009
"          URL: http://www.vim.org/scripts/script.php?script_id=2508
"      Version: 1.0.2
"               Originally written by Seung Woo Shin <segv <at> sayclub.com>
"               at	http://www.vim.org/scripts/script.php?script_id=1152
"               Developed by Sam Ghods <sam <at> box.net>
"               at http://www.vim.org/scripts/script.php?script_id=1929
"        Usage: 
"               to set the socket waiting time before timeout(default 
"               5 second) use :
"               
"                 let g:debuggerTimeout = 10
"
"               above snippet use 10 second 
"
"               By default the script will create a dedicated tab for each 
"               debugging session, 
"
"                 let g:debuggerDedicatedTab = 1
"
"               if you dont want a dedicated tab for each debugging session
"               change it to 0, remember you would lost undo history on that
"               tab!
"
"               note : 
"               if you are using gVim/vim.gui and want to use a dedicated tab 
"               make sure to always show tab
"
"               if has("gui_running")
"                 if has("gui_gtk2")
"                   set showtabline=2
"                 endif
"               endif
"
"               put the snippet above on your ~/.vimrc file, this is
"               regarding the resize bug on vim with gtk
"
"
"
"               Wrote by Sam Ghods :
"
"               This file should reside in the plugins directory along
"               with debugger.py and be automatically sourced.
"
"               By default, the script expects the debugging engine to connect
"               on port 9000. You can change this with the g:debuggerPort
"               variable by putting the following line your vimrc:
"
"                 let g:debuggerPort = 10001
"
"               where 10001 is the new port number you want the server to
"               connect to.
"
"               There are three maximum limits you can set referring to the
"               properties (variables) returned by the debugging engine.
"
"               g:debuggerMaxChildren (default 32): The max number of array or
"               object children to initially retrieve per variable.
"               For example:
"
"                 let g:debuggerMaxChildren = 64
"
"               g:debuggerMaxData (default 1024 bytes): The max amount of
"               variable data to retrieve.
"               For example:
"
"                 let g:debuggerMaxData = 2048
"
"               g:debuggerMaxDepth (default 1): The maximum depth that the
"               debugger engine may return when sending arrays, hashs or
"               object structures to the IDE.
"               For example:
"
"                 let g:debuggerMaxDepth = 10
"
"               Finally, if you use the Mini Buffer Explorer vim plugin,
"               minibufexpl.vim, running the debugger may mess up your window
"               setup. As a result the script has support to close and open
"               the explorer when you enter and quit debugging sessions. To
"               enable this support, add the following line to your vimrc:
"
"                 let g:debuggerMiniBufExpl = 1
"
"      History: 1.0.1 o create a dedicated tab for debugging for every new
"                       debugging session, to retain undo history for other tab
"                       contributed by Michael Bahnmiller
"               1.0   o add tab-page support for Vim 7.x from Sam Ghods latest script
"                       at http://www.vim.org/scripts/script.php?script_id=1929
"      
" Known Issues: o if you are using gVim, makesure the showtabline options set to
"                 2 (always show tab) before start a new debugging session, 
"                 this is a known problem to the gtk/gnome build about
"                 gui window resizing.
"               o there are possibility after each debugging session ends, the
"                 folding, coloring, etc goes weird. this may occur if you're using 
"                 a custom syntax file, so you might wanna re-source your syntax
"                 file after each debugging session
"
"         Todo: so many to do, but first thing first, revamp the python script
"
"=============================================================================


"=============================================================================
" check that everything is OK
"=============================================================================
if !has("python")
    finish
endif


if filereadable($VIMRUNTIME."/plugin/debugger.py")
  pyfile $VIMRUNTIME/plugin/debugger.py
elseif filereadable($HOME."/.vim/plugin/debugger.py")
  pyfile $HOME/.vim/plugin/debugger.py
elseif filereadable($VIM."/vimfiles/plugin/debugger.py")
  pyfile $VIM/vimfiles/plugin/debugger.py
else
  call confirm('debugger.vim: Unable to find debugger.py. Place it in either your home vim directory or in the Vim runtime directory.', 'OK')
endif


"=============================================================================
" map debugging function keys
"=============================================================================
map <F1> :python debugger_resize()<cr>
map <F2> :python debugger_command('step_into')<cr>
map <F3> :python debugger_command('step_over')<cr>
map <F4> :python debugger_command('step_out')<cr>
map <F5> :call <SID>startDebugging()<cr>
map <F6> :call <SID>stopDebugging()<cr>
map <F11> :python debugger_context()<cr>
map <F12> :python debugger_property()<cr>
map <F11> :python debugger_watch_input("context_get")<cr>A<cr>
map <F12> :python debugger_watch_input("property_get", '<cword>')<cr>A<cr>
nnoremap ,e :python debugger_watch_input("eval")<cr>A


"=============================================================================
" Initialization
"=============================================================================
hi DbgCurrent term=reverse ctermfg=White ctermbg=Red gui=reverse
hi DbgBreakPt term=reverse ctermfg=White ctermbg=Green gui=reverse

command! -nargs=? Bp python debugger_mark('<args>')
command! -nargs=0 Up python debugger_up()
command! -nargs=0 Dn python debugger_down()
sign define current text=->  texthl=DbgCurrent linehl=DbgCurrent
sign define breakpt text=B>  texthl=DbgBreakPt linehl=DbgBreakPt

if !exists('g:debuggerPort')
  let g:debuggerPort = 9000
endif 
if !exists('g:debuggerMaxChildren')
  let g:debuggerMaxChildren = 32
endif
if !exists('g:debuggerMaxData')
  let g:debuggerMaxData = 1024
endif
if !exists('g:debuggerMaxDepth')
  let g:debuggerMaxDepth = 1
endif
if !exists('g:debuggerMiniBufExpl')
  let g:debuggerMiniBufExpl = 0
endif
if !exists('g:debuggerTimeout')
    let g:debuggerTimeout = 10
endif
if !exists('g:debuggerDedicatedTab')
    let g:debuggerDedicatedTab = 1
endif
if !exists('g:debuggerDebugMode')
    let g:debuggerDebugMode = 0
endif



"=============================================================================
" Debugging functions
"=============================================================================

function! s:startDebugging()
        python debugger_run()
endfunction

function! s:stopDebugging()
        python debugger_quit()
        " if your code goes weird re-source your syntax file, or any other
        " cleanups here
        "source ~/.vim/plugin/torte.vim 
endfunction

"=============================================================================
" Init Debugger python script
"=============================================================================

python debugger_init()

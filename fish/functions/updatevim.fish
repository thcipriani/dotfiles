function updatevim
    su (whoami) --shell /bin/bash -c 'SHELL=/bin/bash vim +BundleInstall! +BundleClean +qall'
end

function updatevim
    set SHELL (which bash)
    vim +BundleInstall! +BundleClean +qall
    set SHELL (which fish)
end

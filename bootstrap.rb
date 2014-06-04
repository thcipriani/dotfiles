#!/usr/bin/env ruby
# from http://errtheblog.com/posts/89-huba-huba

home = File.expand_path '~'
dotfiles = File.expand_path '~/.dotfiles'

dontlink = %w[bin srv etc bootstrap project plan]
otherbin = %w[sshit weather-ma-jig]

Dir['*'].each do |file|
  next if file =~ /#{dontlink.join('|')}/
  target = File.join(home, ".#{file}")
  `ln -sf "#{File.join dotfiles, file}" "#{target}"`
end

# link bin and srv
%w[bin srv etc].each do |dir|
  `ln -sf "#{File.join dotfiles, dir}" "#{File.join(home, dir)}"`
end

# Hardlinks to satisfy finger
%w[project plan].each do |file|
  target = File.join(home, ".#{file}")
  `ln "#{File.join dotfiles, file}" "#{target}"`
end

# link other repo executables so I don't have to deal with submodules
otherbin.each do |repo|
  `git clone git@github.com:thcipriani/#{repo}.git #{File.expand_path 'bin'}/src/#{repo}`
end

# Vundle setup
if not File.directory? File.expand_path '~/.vim/bundle/Vundle.vim'
  `git clone https://github.com/gmarik/Vundle.vim.git "$HOME/.vim/bundle/Vundle.vim"`
end

`vim +PluginInstall +qall`

# git push on commit
`echo 'git push' > .git/hooks/post-commit`
`chmod 755 .git/hooks/post-commit`

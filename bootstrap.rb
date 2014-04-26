#!/usr/bin/env ruby
# from http://errtheblog.com/posts/89-huba-huba

home = File.expand_path('~')
dontlink = %w[bin srv bootstrap]
otherbin = %w[sshit weather-ma-jig]

Dir['*'].each do |file|
  next if file =~ /#{dontlink.join('|')}/
  target = File.join(home, ".#{file}")
  `ln -sf #{File.expand_path file} #{target}`
end

# link bin to ~/bin
`ln -sf #{File.expand_path 'bin'} #{File.join(home, 'bin')}`
`ln -sf #{File.expand_path 'srv'} #{File.join(home, 'srv')}`

# link other repo executables so I don't have to deal with submodules
otherbin.each do |repo|
  `git clone git@github.com:thcipriani/#{repo}.git #{File.expand_path 'bin'}/src/#{repo}`
end

# git push on commit
`echo 'git push' > .git/hooks/post-commit`
`chmod 755 .git/hooks/post-commit`

#!/usr/bin/env ruby
# from http://errtheblog.com/posts/89-huba-huba

home = File.expand_path('~')
dontlink = %w[bin bootstrap]

Dir['*'].each do |file|
  next if file =~ /#{dontlink.join('|')}/
  target = File.join(home, ".#{file}")
  `ln -sf #{File.expand_path file} #{target}`
end

# link bin to ~/bin
`ln -sf #{File.expand_path 'bin'} #{File.join(home, 'bin')}`

# git push on commit
`echo 'git push' > .git/hooks/post-commit`
`chmod 755 .git/hooks/post-commit`

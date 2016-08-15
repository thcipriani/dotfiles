---
layout: post
title: Using .htaccess to remove Codeigniter&#8217;s index.php file
published: false
---

Quick post today. I was just finishing up a codeigniter project and saw an error in the docs
in the [CodeIgniter User Guide Ver. 2.1.2 &sect; CodeIgniter URLs](http://codeigniter.com/user_guide/general/urls.html "CI URLs")
under the section heading, &#8220;Removing the index.php file&#8221; it shows an .htaccess file example:

{% highlight bash linenos %}
RewriteEngine on
RewriteCond $1 !^(index\.php|images|robots\.txt)
RewriteRule ^(.*)$ /index.php/$1 [L]
{% endhighlight %}
The <code>RewriteRule</code> should not have the forward slash (<code>/</code>) before <code>index.php</code>. 

To function as expected the <code>RewriteRule</code> should read:
{% highlight bash linenos %}
RewriteEngine on
RewriteCond $1 !^(index\.php|images|js|css|robots\.txt)
RewriteRule ^(.*)$ index.php/$1 [L]
{% endhighlight %}

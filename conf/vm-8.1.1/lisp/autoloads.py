#!/usr/bin/python
# -*- python -*-

import sys

def identifier_start(string, startpos=0):
    #print string, startpos
    while (startpos < len(string) and
	   ("() \t\r\n.,".find(string[startpos]) != -1)):
	startpos = startpos + 1
    return startpos

def identifier_end(string, startpos=0):
    #print string, startpos
    while (startpos < len(string) and
	   ("() \t\r\n.,".find(string[startpos]) == -1)):
	startpos = startpos + 1
    return startpos

class Def:
    def __init__(self, filename, lineno, autoload, symbol):
	self.filename = filename
	self.lineno = lineno
	self.autoload = autoload
	self.symbol = symbol
    def __str__(self):
	return ("%s:%d %s %s" % (self.filename,
				 self.lineno,
				 self.symbol,
				 self.autoload))

def find_defs(filename, pattern="(defun", pos=0):
    """Find definitions of pattern in the given file.
    Returns defined symbols."""
    symbols = []

    fd = open(filename)
    lineno = 0
    autoload = False
    for l in fd:
	lineno = lineno + 1
	if l.startswith(";;;###autoload"):
	    autoload = True
	    continue
	s = l.find(pattern)
	if s == -1 or s != pos:
	    continue
	s = identifier_start(l, s + len(pattern))
	while "() \t\r\n.,".find(l[s]) != -1:
	    s = s + 1
	e = identifier_end(l, s) 
	if s == e:
	    raise "Could not find identifier end in " + repr(l)
	    continue
	#print s, e
	#print l[s : e]
	symbols.append(Def(filename, lineno, autoload, l[s : e]))
	autoload = False
    fd.close()
    return symbols

preloaded = ["vm-version.el", "vm-misc.el", "vm-macro.el", "vm-folder.el",
	     "vm-summary.el", "vm-minibuf.el", "vm-motion.el", "vm-page.el",
	     "vm-mouse.el", "vm-window.el", "vm-menu.el", "vm-message.el",
	     "vm-toolbar.el", "vm.el", "vm-undo.el", "vm-mime.el",
	     "vm-vars.el"]

def check_calls(filename, funs, missing):
    #print "-" * 50
    #print filename
    fd = open(filename)
    required = []
    for l in fd:
	s = l.find("(require")
	if s != -1:
	    s = identifier_start(l, s + len("(require '" ))
	    e = identifier_end(l, s)
	    #print l[s:e], "*" * 50
	    required.append(l[s:e] + ".el")
	    #print required
	    continue
				   
	# check for calls to external function without autoloads or require
	for c in l.split("("):
	    s = identifier_start(c, 0)
	    e = identifier_end(c, s)
		
	    #print repr(c)
	    s = identifier_start(c, 0)
	    e = identifier_end(c, s)
	    f = c[s:e]
	    if f not in funs:
		continue
	    d = funs[f]
	    if ((d.filename != filename) and (not d.autoload) and
		(d.filename not in preloaded) and
		(d.filename not in required)):
		#print preloaded
		#print "'%s' : '%s' => '%s' %s" % (filename, f, d.filename,
						  #d.filename in preloaded)
		#print preloaded
		if not missing.has_key(d.filename):
		    missing[d.filename] = []
		if f not in  missing[d.filename]:
		    missing[d.filename].append(f)
    fd.close()
    

# emit cross references with missing autoloads 
if __name__ == '__main__':
    funs = {}
    for filename in sys.argv[3:]:
	for d in find_defs(filename):
	    if funs.has_key(d.symbol):
		print "Duplicate %s <> %s" % (d, funs[d.symbol])
	    else:
		funs[d.symbol] = d
    missing = {}
    for filename in sys.argv[3:]:
	check_calls(filename, funs, missing)
    for f in missing.keys():
	print f
	for m in missing[f]:
	    print "\t", m
	
    

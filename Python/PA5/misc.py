#PA 4

import re

"Miscellaneous functions to practice Python"

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)

# Problem 1

# data type functions

# Function closest_to(l,v)
# Takes in a list l and returns the element that is closest to l.
# If tie then first element is returned.

def closest_to(l,v):
	"""Return the element of the list l closest in value to v.  In the case of
		a tie, the first such element is returned.  If l is empty, None is returned."""
	if not l:
		return None

	minDif = abs(l[0]-v)
	num = l[0]
	for x in l:
		diff = abs(x-v)
		if diff < minDif:
			minDif = diff
			num = x
	return num
		
# Function make_dict(keys,vals)
# This function takes in a list of keys with a corresponding list of 
# its values.
# It returns a dictionary pairing the keys with its values.

def make_dict(keys,values):
	"""Return a dictionary pairing corresponding keys to values."""
	l = zip(keys,values)
	return dict(l)
   
# file IO functions

# Function word_count(fn)
# Takes in a string that represents the file name and return a dictionary 
def word_count(fn):
	"""Open the file fn and return a dictionary mapping words to the number
	of times they occur in the file.  A word is defined as a sequence of
	alphanumeric characters and _.  All spaces and punctuation are ignored.
	Words are returned in lower case"""
	
	f = open(fn, 'r')
	wordList = []
	freq = []
	pattern = re.compile('[^a-zA-Z0-9_]')
	for line in f:
		words = pattern.split(line)
		for x in words:
			wordList.append(x.lower())

	f.close()		
	keys = [y for y in wordList if y != '']
	
	for z in keys:
		freq.append(wordList.count(z))

	return dict(zip(keys,freq))


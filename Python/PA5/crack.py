from misc import *
import crypt
import re

# load_words(filename,regexp) function
# This function loads words from the filename that
# matches all the regular expression as a list.
def load_words(filename,regexp):
	"""Load the words from the file filename that match the regular
	expression regexp.  Returns a list of matching words in the order
	they are in the file."""
	f = open(filename,'r')
	wordList = []
	pattern = re.compile(regexp)
	for line in f:
		words = line.split()
		for x in words:
			if pattern.match(x):
				wordList.append(x)
	f.close()
	return wordList
	
# transform_reverse(str)
# This function returns a list that contains
# the original string along with its reverse.
def transform_reverse(str):
	return [str,str[::-1]]


# transform_capitalize(str)
# This function returns a list that contains
# all the possible way to capitalize the string.
def transform_capitalize(s):
	return list(set(capitalize_helper(s)))

# A helper function that recursively returns
# a list of possible capitalized string
def capitalize_helper(s):
	if len(s) == 1:
		return [s.lower(),s.upper()]

	lower = s[0].lower()
	upper = s[0].upper()
	
	l1 = [lower+x for x in capitalize_helper(s[1:])]
	l2 = [upper+x for x in capitalize_helper(s[1:])]

	return l1 + l2

# transform_digits(str)
# This function transforms certain chracters into different
# character. It returns a list of all the possibilites.
def transform_digits(s):
	return list(set(digits_helper(s)))


# digits_helper(s)
# This function is a helper function to the 
# transform_digits function which it recursively takes
# one character at a time and creates a list of all possible transformations
def digits_helper(s):
	if len(s) == 0:
		return ['']
			
	wordList = []
	if s[0].lower() == 'o':
		wordList = ['0'+x for x in digits_helper(s[1:])]
	elif s[0].lower() == 'z':
		wordList = ['2'+x for x in digits_helper(s[1:])]
	elif s[0].lower() == 'a':
		wordList = ['4'+x for x in digits_helper(s[1:])]
	elif s[0].lower() == 'b':
		wordList = ['6'+x for x in digits_helper(s[1:])] + ['8'+x for x in digits_helper(s[1:])]
	elif s[0].lower() == 'i' or s[0].lower() == 'l':
		wordList = ['1'+x for x in digits_helper(s[1:])]
	elif s[0].lower() == 'e':
		wordList = ['3'+x for x in digits_helper(s[1:])]
	elif s[0].lower() == 's':
		wordList = ['5'+x for x in digits_helper(s[1:])]
	elif s[0].lower() == 't':
		wordList = ['7'+x for x in digits_helper(s[1:])]
	elif s[0].lower() == 'g' or s[0].lower() == 'q':
		wordList = ['9'+x for x in digits_helper(s[1:])]
	else:
		wordList = []

	return wordList + [s[0]+x for x in digits_helper(s[1:])]



# check_pass(plain,enc)
# This function takes in a string plaintext and an encrypted
# text enc and checks to see if the plain text encrypts to the
# encrypted text.
def check_pass(plain,enc):
	"""Check to see if the plaintext plain encrypts to the encrypted
	text enc"""

	if crypt.crypt(plain,enc[0:2]) == enc:
		return True
	else:
		return False


# load_passwd(filename)
# This function takes in a file and reads its contents to 
# return a dictionary of account,password,uid,gid,gecos,directory
# and shell.

def load_passwd(filename):
	"""Load the password file filename and returns a list of
	dictionaries with fields "account", "password", "UID", "GID",
	"GECOS", "directory", and "shell", each mapping to the
	corresponding field of the file."""

	f = open (filename,'r')
	finalList = []
	for line in f.readlines():
		words = re.split(':',line)
		words[2:4] = int(words[2]),int(words[3])
		tuples = zip(['account','password','UID','GID','GECOS','directory','shell'], words)
		finalList.append(dict(tuples))

	f.close()

	return finalList
	

# crack_pass_file(pass,words,out)
# This file takes in 3 files: 
#	pass - A file containing the passwords
#	words - A file containing the words
#	out - The output file for cracked passwords.
# And cracks as many passwords as possible using the words in words file 
# along with its transformations and stores all the cracked files within the output.
def crack_pass_file(pass_file,words_file,out_file):
	"""Crack as many passwords in file fn_pass as possible using words
	in the file words"""

	# open output file
	out = open(out_file,'w')
	# get words of character 6-8
	words = load_words(words_file, r'^.{6,8}$')
	# get passwords
	pwords = load_passwd(pass_file)
	
	# easy first
	for x in pwords:
		acc = x['account']
		psswd = x['password']
		for y in words:
			if check_pass(y,psswd):
				out.write(acc+'='+y+'\n')
				out.flush
				pwords.remove(x)
				break

	# try reverse (second easy)
	for x in pwords:
		acc = x['account']
		psswd = x['password']
		for y in words:
			z = transform_reverse(y)
			if check_pass(z[1],psswd):
				out.write(acc+'='+z[1]+'\n')
				out.flush()
				pwords.remove(x)
				break

	# try capitialize 
	for x in pwords:
		acc = x['account']
		psswd = x['password']
		for y in words:
			for z in transform_capitalize(y):
				if check_pass(z,psswd):
					out.write(acc+'='+z+'\n')
					out.flush()
					pwords.remove(x)
					break
					
	#try digits
	for x in pwords:
		acc = x['account']
		psswd = x['password']
		for y in words:
			for z in transform_digits(y):
				if check_pass(z,psswd):
					out.write(acc+'='+z+'\n')
					out.flush()
					pwords.remove(x)
					break
	out.close()

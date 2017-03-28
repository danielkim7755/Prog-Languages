
from misc import Failure

"""The Vector Class"""
class Vector(object):
	"""The constructor function for the Vector class which takes in an object and creates a vector of size indicated by the object"""
	def __init__(self,size):
		if isinstance(size,int) or isinstance(size,long):
			if (size < 0):
				raise ValueError("Vector length cannot be negative")
			self.vector = [0.0]*size
		else:
			try:
				self.vector = list(size)
			except TypeError:
				raise TypeError("Object type has to be long, int, or a sequence")

	"""This function returns a string representation of the current vector"""
	def __repr__(self):
		return "Vector(" + str(self.vector) +")"

	"""This function returns the length of the vector"""
	def __len__(self):
		return len(self.vector)

	"""This function returns an object that can iterate over the elements of Vector"""
	def __iter__(self):
		for x in self.vector:
			yield x

	"""This function adds the elements of a Vector and a sequence and returns a new Vector of the results"""
	def __add__(self,other):
		if (len(self.vector) != len(other)):
			raise ValueError("Both Objects need to be the same size")

		sumVec = [(x+y) for (x,y) in zip(self.vector,other)]
		return Vector(sumVec)

	"""This function adds a sequence to the elements of a Vector with a sequence and returns a new Vector of the results"""
	def __radd__(self,other):
		if(len(self) != len(other)):
			raise ValueError("Both objects need to be the same size")

		sumVec = [(x+y) for (x,y) in zip(self,other)]
		return Vector(sumVec)

	"""This function adds another objects elements into this Vector's elements. Implements the (+) operator for the Vector class"""
	def __iadd__(self,other):
		if(len(self.vector) != len(other)):
			raise ValueError("Both Vector and the Adding Object need to be the same size")

		sumVec = [(x+y) for (x,y) in zip(self.vector,other)]
		self.vector = sumVec
		return self

	"""This function returns the dot product of the current Vector instance and the input sequence or Vector argument"""
	def dot(self,other):
		if(len(self.vector) != len(other)):
			raise ValueError("The input Object must match the size of the Vector")

		try:
			dProd = 0
			for (x,y) in zip(self.vector,other):
				dProd = dProd + (x * y)
			return dProd
		except TypeError:
			raise TypeError("Object and Vector both have to be int or long")

	"""This function returns the element at the input index on the input Vector"""
	def __getitem__(self,index):
		if (type(index) == slice):
			return Vector(self.vector[index])
		else: 
			if (index + len(self.vector) < 0) or (index > len(self)):
				raise IndexError("Index is out of bounds")

			if(index >=0):
				return self.vector[index]
			else:
				return self.vector[index + len(self.vector)]

	"""This function sets the input value at the input index within the Vector"""
	def __setitem__(self,index,value):	
		if (type(index) == slice):
			slicedList = self.vector[index]
			if (len(slicedList) != len(value)):
				raise ValueError("Value size has to equal slice size")
			self.vector[index] = value
		else:
			if (index + len(self.vector) < 0) or (index > len(self)):
				raise IndexError("Index is out of bounds")

			if (index >= 0):
				self.vector[index] = value
			else:
				self.vector[index + len(self.vector)] = value

	"""This function compares two Vectors to see if they are equal. Implementes the (==) operator for the Vector class"""
	def __eq__(self,other):
		if not isinstance(other,Vector):
			return False

		for (x,y) in zip(self.vector,other.vector):
			if x != y:
				return False
		return True

	"""This function compares two Vectors to see if they are NOT equal. Implements the (!=) operator for the Vector class"""
	def __ne__(self,other):
		return not self.__eq__(other)

	"""This function compares two Vectors to see if the first Vector is greater than the second Vector. Implements (>)"""
	def __gt__(self,other):
		for (x,y) in zip(sorted(self.vector,reverse = True),sorted(other.vector,reverse = True)):
			if not (x > y):
 				return False
		return True

	"""This function compares two Vectors to see if the first Vector is greater than or equal to the second Vector. Implements (>=)"""
	def __ge__(self,other):
		vecA = Vector(sorted(self.vector,reverse = True))
		vecB = Vector(sorted(other.vector,reverse = True))
		return vecA.__eq__(vecB)

	"""This function compares two Vectors to see if the first Vector is less than the second Vector. Implements (<)"""
	def __lt__(self,other):
		return not self.__ge__(other)

	"""This function compares two Vectors to see if the first Vector is less than or equal to the second Vector. Implements (<=)"""
	def __le__(self,other):
		return not self.__gt__(other)

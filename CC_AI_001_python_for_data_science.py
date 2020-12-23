# %%
# =============================================================================
# MODULE 1 - PYTHON BASICS
# =============================================================================

print('helloooo!')
print('hello, {who}!'.format(who='people'))

import sys
sys.version

type(3.14)

sys.float_info

print('this sentence \nhas a line break')
print('this sentence \t has a tab')
print('this sentence has a bar \\')
print(r'this sentence has a bunch of special characters \n\t')

# %%
# =============================================================================
# MODULE 2 - PYTHON DATA STRUCTURES
# =============================================================================

A = (0,1,2,3)

B = ['a','b','c']
B[0] = B[0].upper()
B

L = ['asdf',123,True,(0,1)]
L.append('fdas')
L.pop()
L.extend(['fasdf',A,B])
L
del(L[3])
L
L.extend('asd,fsdad,fas'.split(','))
L
M = L
M.pop()
M
L
M = L.copy()
M.pop()
M
L
N = L[:]
N.extend(['this goes to N only'])
N
L

tuple1 = (0,1,3)
tuple2 = ('0','1','3')
tuple1+tuple2
sorted(tuple1)
sorted(tuple2)
tuple1.__add__(tuple2)
tuple1
tuple1.index(3)

set1 = {'rock', 'blues', 'jazz', 'soul'}
set1
set2 = set(['rock','blues','jazz','soul'])
set1 == set2
set2.remove('soul')
set2
set1 & set2
set1.union(set2)
set2 in set1
set1 in set2
'rock' in set1
[genre in set1 for genre in set2]
[genre in set2 for genre in set1]
set1.difference(set2)
set2.difference(set1)
set1.issuperset(set2)
set2.issuperset(set1)
set2.issubset(set1)

dict1 = {'key1':'value1', 'key2':'value2'}
dict1
dict1['key1']
'value1' in dict1
'key1' in dict1
dict2 = dict([('key1','value1'),('key2','value2')])
dict1 == dict2
del(dict2['key2'])
dict2

# %%
# =============================================================================
# MODULE 3 - PYTHON PROGRAMMING FUNDAMENTALS
# =============================================================================

def f(*x):
    print('this comes before return statement, this is printed')
    return sum(x)
    print('this comes after return statement, this is not printed')

f(2,3,4)

# %%
# =============================================================================
# MODULE 4 - WORKING WITH DATA IN PYTHON
# =============================================================================


# %%
# =============================================================================
# MODULE 5 - WORKING WITH NUMPY ARRAYS
# =============================================================================

%reset -f

import numpy as np

a = np.array([1,2,3,4,5])
np.array([1,2,3,4,5])**2

a**2
a[0]=5
a

np.linspace(0,10,7)
np.arange(0,10,1)

a.argmin()
a.argmax()

b = np.array([7,6,5])
np.hstack((a,b))

A = np.array([[1,2],[3,4]])
B = np.array([[2,4],[1,3]])
A*B
np.dot(A,B)
A[0]
A[0,0]
A[0][0]

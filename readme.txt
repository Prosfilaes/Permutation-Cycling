Take a certain number of colored strings running parallel, and switch adjacent strings one
at a time. Can you produce every permutation of the strings without duplicating them? This 
code is designed to answer this question.

Two and fewer strings is trivial.

For three strings, one solution is: 132, 312, 321, 231, 213, 123.

For four strings, one solution is: 1243, 2143, 2413, 2431, 2341, 3241, 3421, 4321, 
4231, 4213, 4123, 1423, 1432, 4132, 4312, 3412, 3142, 1342, 1324, 3124, 3214, 2314, 2134, 
1234.

Three was producable by hand; with more patience, a solution for four strings could have
been done by hand, which is not true for five, at least not without something smarter than
trial and error.

For five strings, one solution is: 13524, 31524, 35124, 53124, 51324, 15324, 15342, 15432, 
15423, 51423, 54123, 45123, 41523, 14523, 14532, 41532, 45132, 45312, 45321, 45231, 42531, 
42351, 43251, 34251, 32451, 32541, 35241, 53241, 52341, 25341, 23541, 23451, 24351, 24531, 
25431, 52431, 54231, 54321, 53421, 35421, 34521, 43521, 43512, 34512, 35412, 53412, 54312, 
54132, 51432, 51342, 53142, 35142, 31542, 13542, 13452, 31452, 34152, 43152, 41352, 14352, 
14325, 41325, 43125, 34125, 31425, 13425, 13245, 31245, 32145, 23145, 23415, 32415, 34215, 
43215, 42315, 24315, 24135, 42135, 41235, 14235, 12435, 21435, 21453, 12453, 14253, 41253, 
42153, 24153, 24513, 42513, 45213, 54213, 52413, 25413, 25143, 52143, 51243, 15243, 12543, 
21543, 21534, 12534, 15234, 51234, 52134, 25134, 25314, 52314, 53214, 35214, 32514, 23514, 
23154, 32154, 31254, 13254, 12354, 21354, 21345, 12345.

This is 119 switches (5! - 1). Six strings will take 719 switches. I've run this program
on 16 threads for a week, with no results. I suspect an algorithm with a better big O will
be needed to produce a result. (I suspect an O(n) algorithm may be possible.)


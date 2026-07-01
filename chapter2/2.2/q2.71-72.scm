; 2.71 -  most frequent will have 1 bit, and least frequent will be (n - 1)
; 2.72 - most frequent symbol will be on the left of the tree alone, and it will be a leaf node, so encoding it will cost o(1)
; but for least frequent, since we will visit every branch and use memq on it, it becomes expensive and order of growth is around o(n^2)
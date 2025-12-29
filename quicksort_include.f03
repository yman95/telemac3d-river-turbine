INTEGER :: first, last
INTEGER i, j
INTEGER,intent(in),optional :: f,l

first = 1
last = size(a)
      
if(PRESENT(f)) first=f
if(PRESENT(l)) last=l

x = a((first+last) / 2)
i = first
j = last

DO
  DO WHILE (compare(a(i),x))
      i=i+1
  END DO
  DO WHILE (compare(x,a(j)))
      j=j-1
  END DO

  IF (i >= j) exit
  t = a(i);a(i) = a(j);a(j) = t
  i=i+1
  j=j-1
END DO
IF (first < i - 1) CALL quicksort(a,first, i - 1)
IF (j + 1 < last)  CALL quicksort(a,j + 1 , last)
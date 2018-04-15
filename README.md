# Mathlogic ITMO 2018 course practice tasks

[Page at Yandex.Contest](https://contest.yandex.ru/contest/7696/enter/)

[Statement for tasks in russian](https://github.com/shd/logic2018/blob/master/hw-practice.pdf)

## Task 0: 

Parse logical expressions and print them in prefix form.

Examples: 

```haskell
!A&!B->!(A|B)
```

*should be*

```haskell 
(->,(&,(!A),(!B)),(!(|,A,B)))
```
---------------------------------
```haskell
P->!QQ->!R10&S|!T&U&V
```

*should be*

```haskell 
(->,P,(->,(!QQ),(|,(&,(!R10),S),(&,(&,(!T),U),V))))
```
## Task 1:

Check and annotate proof of classical propositional calculus.

Example:
```haskell
A,B|-A&B
A
B
A->B->A&B
B->A&B
A->A
A&B
```

*should be*

```haskell
(1) A (Предп. 1)
(2) B (Предп. 2)
(3) (A->(B->(A&B))) (Сх. акс. 3)
(4) (B->(A&B)) (M.P. 3, 1)
(5) (A->A) (Не доказано)
(6) (A&B) (M.P. 4, 2)
```

## Task 2:

Deduction theorem constrictive proof.

Example:
```haskell
A|-B->A
A->B->A
A
B->A
```
*should be*

```haskell
|-(A->(B->A))
((A->(B->A))->(A->(A->(B->A))))
(A->(B->A))
(A->(A->(B->A)))
(A->(A->A))
(A->((A->A)->A))
((A->(A->A))->((A->((A->A)->A))->(A->A)))
((A->((A->A)->A))->(A->A))
(A->A)
((A->A)->((A->(A->(B->A)))->(A->(B->A))))
((A->(A->(B->A)))->(A->(B->A)))
(A->(B->A))
```





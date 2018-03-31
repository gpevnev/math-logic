# Mathlogic ITMO 2018 course practice tasks

[Page at Yandex.Contest](https://contest.yandex.ru/contest/7696/enter/)

[Statement for tasks in russian](https://github.com/shd/logic2018/blob/master/hw-practice.pdf)

## Task 0: 

Parse logical expressions and print them in prefix form.

Examples: 
 * `!A&!B->!(A|B)` 
   should be 
   `(->,(&,(!A),(!B)),(!(|,A,B)))`
 * `P->!QQ->!R10&S|!T&U&V` 
   should be 
   `(->,P,(->,(!QQ),(|,(&,(!R10),S),(&,(&,(!T),U),V))))`

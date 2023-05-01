; You're already almost 1.5km (almost a mile) below the surface of the ocean, already so deep that you can't see any sunlight. What you can see, however, is a giant squid that has attached itself to the outside of your submarine.

; Maybe it wants to play bingo?

; Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. Numbers are chosen at random, and the chosen number is marked on all boards on which it appears. (Numbers may not appear on all boards.) If all numbers in any row or any column of a board are marked, that board wins. (Diagonals don't count.)

; The submarine has a bingo subsystem to help passengers (currently, you and the giant squid) pass the time. It automatically generates a random order in which to draw numbers and a random set of boards (your puzzle input). For example:

; 7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

; 22 13 17 11  0
;  8  2 23  4 24
; 21  9 14 16  7
;  6 10  3 18  5
;  1 12 20 15 19

;  3 15  0  2 22
;  9 18 13 17  5
; 19  8  7 25 23
; 20 11 10 24  4
; 14 21 16 12  6

; 14 21 17 24  4
; 10 16 15  9 19
; 18  8 23 26 20
; 22 11 13  6  5
;  2  0 12  3  7

; After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no winners, but the boards are marked as follows (shown here adjacent to each other to save space):

; 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
;  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
; 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
;  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
;  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

; After the next six numbers are drawn (17, 23, 2, 0, 14, and 21), there are still no winners:

; 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
;  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
; 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
;  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
;  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

; Finally, 24 is drawn:

; 22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
;  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
; 21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
;  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
;  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

; At this point, the third board wins because it has at least one complete row or column of marked numbers (in this case, the entire top row is marked: 14 21 17 24 4).

; The score of the winning board can now be calculated. Start by finding the sum of all unmarked numbers on that board; in this case, the sum is 188. Then, multiply that sum by the number that was just called when the board won, 24, to get the final score, 188 * 24 = 4512.

; To guarantee victory against the giant squid, figure out which board will win first. What will your final score be if you choose that board?

; First Part

(define mat '((1 2) (3 4)))

(define (transpose mat)
  (if (null? (car mat))
    '()
    (cons (map car mat)
          (transpose (map cdr mat)))))

(define (matrix-map f mat)
  (append (row-map f mat)
          (col-map f mat)))

(define (row-map f mat)
  (map f mat))

(define (col-map f mat)
  (map f (transpose mat)))

(define (member? x l)
  (cond ((null? l) #f)
        ((eq? x (car l)) #t)
        (else (member? x (cdr l)))))

(define (check-nums nums l)
  (cond ((null? l) #t)
        ((member? (car l) nums) (check-nums nums (cdr l)))
        (else #f)))

(define (check-board nums board)
    (any (lambda (x) (eq? x #t)) ; I know this is ugly, but I couldn't reduce with or, as it's apparently a macto
         (matrix-map (lambda (l) (check-nums nums l)) board)))

(define (check-all nums boards)
  (map (lambda (board) (check-board nums board)) boards))

(define (make-boards data)
  (fiveify (fiveify data)))

(define (find-true l)
  (let loop ((index 0)
             (rem l))
    (cond ((null? rem) -1)
          ((car rem) index)
          (else (loop (1+ index) (cdr rem))))))

(define (fiveify l)
  (if (null? l)
    '()
    (cons (list-head l 5)
            (fiveify (list-tail l 5)))))

(define (find-bingo numbers boards)
  (let loop ((nums 5))
    (if (not (eq? (find-true (check-all (list-head numbers nums) boards)) -1))
      (calculate-board (list-ref boards (find-true (check-all (list-head numbers nums) boards))) (list-head numbers nums))
      (loop (1+ nums)))))

(define (flatten matrix)
  (reduce append '() matrix))

(define (calculate-board board numbers)
  (* (car (reverse numbers))
     (reduce + 0 (filter (lambda (x) (not (member? x numbers))) (flatten board)))))

(find-bingo numbers-raw (make-boards tables-raw))

; Second Part

; On the other hand, it might be wise to try a different strategy: let the giant squid win.

; You aren't sure how many bingo boards a giant squid could play at once, so rather than waste time counting its arms, the safe thing to do is to figure out which board will win last and choose that one. That way, no matter which boards it picks, it will win for sure.

; In the above example, the second board is the last to win, which happens after 13 is eventually called and its middle column is completely marked. If you were to keep playing until this point, the second board would have a sum of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.

; Figure out which board will win last. Once it wins, what would its final score be?

; ------------------------

(define example-tables '(
22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7))


(define numbers-raw '(94 21 58 16 4 1 44 6 17 48 20 92 55 36 40 63 62 2 47 7 46 72 85 24 66 49 34 56 98 41 84 23 86 64 28 90 39 97 73 81 12 69 35 26 75 8 32 77 52 50 5 96 14 31 70 60 29 71 9 68 19 65 99 57 54 61 33 91 27 78 43 95 42 3 88 51 53 30 89 87 93 74 18 15 80 38 82 79 0 22 13 67 59 11 83 76 10 37 25 45))

(define tables-raw '(
49 74 83 34 40
87 16 57 75  3
68 94 77 78 89
56 38 29 26 60
41 42 45 19  1

42 35 10 20  9
49 39 40 41 73
 3 48 91 81 88
59 55 82 58 71
61 51 17 26 72

31 49 21 84 83
18 86 53 75 29
85  2 51 76 52
48 28 24 69 12
 5 87 67 95 82

54 21  0 63 13
84 29 27 12 82
55 86 33 90 95
72 96 24 88 37
38 51 35 46 50

24  1 23 62 97
53 72 99 59 81
54 26 93 63 20
79 41  2 86 98
84 13 87 33 96

36 85 51 32 84
41 70 65 86 73
94 28 80 81 59
96 82  7 10 83
 8 21 29 91 16

56 36 87 10 35
 8 90 37 22 96
24 82 30 50 86
70 52 55 51 57
26 41 61 46 65

 6 28 71 21 50
61 92 19 17 79
85 69 63 32 41
 8 36 37 38 83
13 45 88 77 78

55 61 34 72 65
 8 92 39 27 86
69 16 66 94 53
35 41 50  1 42
31 43  3 85 59

55 78 47 85 50
80 15 98 42 63
 2 68 37 24 45
 8 99 33 89 20
35 28 60  5 34

76  4 33 91  0
98 97 39 51  5
43 86 58 63 93
16 67 88 50 19
 2 68 17 26 89

20 57 93 41 35
76  7 14 58 54
85 51 24 40 38
47 13 82 29 10
 9 21  8 87 17

65 82 87 15 49
43 37 53  6 93
89 83 66 84 33
58 41 44  8 91
23  1 73  5 26

 2 27 51 80  5
88 17 32 75  0
10 38 78 56 25
48 11 63 73 50
57  9 67 86 31

35 47 63  9 13
12 14 82 37 32
49 74 79 90 10
22 50 41 46 15
39 56 19 42 21

 6 48  3  2 95
57 40 86  4 21
 1 23 65 76 90
47 63 29 58 49
77 36 71 55 83

31  1 98 47 99
85 56 81 29 76
14 46 12 62 83
86 45 74 73 32
17  9 59 26 21

89 72 83 48  3
81 34 27 42 41
90 22 95 85 36
44 45 31 73 57
19 60 50 29 75

93  2 26 35 39
91  7 85 69 62
55  4 27 57 10
92 44 30 73 22
 6 58 16 36  9

 6 42 55 24  8
19 43  2 21 90
99 89 48 60 58
72 87  1 66 63
53 16 71 20 28

98 82  4 29 95
40 63 71 64 96
41 76 93 58 66
30 36 28 59 74
92  1 91 39 65

96 78 45 44  3
53 29 75 51 64
19 84 41 30 60
86 47 99 71 42
95 23 40 43 22

70 26  4 34 15
66 51 12 16 36
28 11 77 61 87
27 75 38 65 31
 6 33 56 10 76

 9 74 75 61 55
63 49 29 48 44
65 12 45 17 31
43 71 88 96 57
20 42 34 99 21

15 11 32 26 51
23 20 19 14 82
75 60  0 18 59
30 66 40 57 47
77 44 37 80 61

 3 40 26 25 33
18 80 72 28 16
 9 46 50 91 93
88 13 52  1 65
70 27 78 43 39

35 77  7 49 72
59  8 87 60 15
38 81 71 24 20
50 54 94 31 75
68  2 11 27 64

45 39 55 51 30
56  0  2 28 10
43 32 46 80 98
15 82 17 92 89
73 62 93 33 40

21 94 54 29 24
40 35 73 43 77
80 14  2 76 31
17 11  8 42 45
46 78 59 99 55

92 24  9 39  4
55 20 17 65 99
67 86 72  6 38
53 51 27 63 93
48 95 83 66 85

26 68 60 15 41
32 55 33 71 63
92 22 70 20 78
85 89 29 27 84
98 91 36 23  6

77 71 11 12 24
98 85 36 29 35
80 51 88 25 81
23  9 33 61 48
 5 66 94 54 10

89 67 34 98 57
 4 20 80 83 28
63 77 66  5 47
 8 36 43 45 41
81 18 90 91 15

20 93 58 27 99
45 47 59  9 23
25 71 14 48 62
95  7 69 41 90
53  1 10 98 70

 1  4 67 24 48
53 88 77 70 86
99 30 23 61 27
82 95 73 37 78
47 92 13 94  0

94  8 19 74 24
10 60  2 65 18
31 22 16 25 32
75  4 86 55 26
93 47 98 43 44

58 39 34 69 79
88 78 85 84 23
89 63 29 28 40
37 83 56 74 32
24 73 61  7 35

46 35 27 49 81
92 41 33 64  5
13  6 96 66 85
76  3 19 17  2
82 30 88  0 39

46  2  1 82 72
 9 14 36 95 70
56 65 13 35 28
38 59 62 21 19
99 77 16 52  8

74 94 50 56  7
60 18 83 87 21
85 42 64 53 40
43 30 67 41 68
32 63 97 82  9

 6 16 58 70 86
42 28 51 38 54
88 46 90 83 36
65 24 95 63 52
94 25 84  5 71

10 84  5 18 34
76 46 82 49 98
74 99 29 11 41
42 92 20 64 39
91 85 79 32 52

67 68 72 43 14
86  5 24 40 70
57 12 92  0 98
60 58 15 13  2
17 51  6  3 74

50 27 68 12 80
79 26 17 59 86
57 29 82 70 71
93  6 78 39 24
72 53 23 49 98

69 27 19 18 54
 6 38 34 41 49
17 94 93 25 86
 1 45 60 44 62
31 72 59 83 36

45 33 42 91 39
 6 77 32 21 27
 9 92 30  2 43
89 79 86 11 83
23 94 76 65  1

78 16 22 80 75
42 61  8 35 93
62 59 66 79 13
44 77  7 87 68
74 14 52 65 19

60 24 88  7 29
95 33 36 81 71
67 39  2 49 37
78 25 28 35 93
20  3 12 99  6

49 99 87 89 85
86 63 42 38 68
46 19 94 60 65
18 51  8  0  3
64 77 23 35 16

95 22 33 57 64
12 37 75  6 74
76 16 86 15 48
70 99 24 19 52
47 65 46 32  8

82 71 28 64 46
88 90 85 69 84
33 86  7 27 10
45 48  0 31 99
37  4 77 29 49

69 33 73 50 26
24 41 15 91 78
95 47 70 23 19
80 14 60 56  3
55 58  8 43 16

64 76 70  0  4
58 26 95 88 53
 1 45 50 97 93
30 65 31  6 81
12 11 74 68 94

13 14 60 87 56
69  3 64 29 57
24 86 78 21 15
33 25 70 67 38
22 73 11 50 96

73 15 22 78 63
48 51  6 39 20
11 91 12  0 65
 2 90 19 64 43
42 89 71 10 31

48  5 78 25 89
82 57 32 80 60
13 21 88 76 65
38 72 53  4 51
 0 31 20 17 36

41 71 27 24 58
 3 54 43 36 75
 7  1 39 59 95
99 97 18 40 96
50 61 49 69 31

58 93 37 23 25
88 81 45 50 33
76 97  4 21 72
56  2 98 78 51
32 17 19 29  6

73 91 87 49 74
99 45 24 76 77
44 67  3 60 27
36 62 94 96 57
 0 16 48 54 92

90  7 66 65  1
27 42  3 11 26
63 95 69 53 29
43 12 52 37 96
84 13 41 36 35

10  5 54  4 16
51 17 22 49  3
86 65 40 58 47
71 69 20  7 98
75 57 97 74 35

31 53 17 88 12
74 39 59 82 68
46 23 13 28 76
 0 89 48 43 37
34 50 86 19 66

70 82 88 32  9
 7 15 38 56 62
91 25 49 78 77
40 42 44 79 18
 1 84 28 73 97

67 38 62 93 81
 1 63 17 86 90
55 66  6 39 13
72 80  5 70 11
30 71 96 14 73

73 29 93 64 48
 9 41 70 57 46
33 92 78 82 91
90  4 87 43 56
83 28 59 85  8

23 99 26  4 56
79 39 31 82 92
17 20 44 70 35
48 71 95 53  1
97 24 41 91 87

93 61 95 53 27
54 49 74 16 82
30 17 59 64 79
 4 28 36  9 38
58 80 44 85 45

92 28 76 97 45
93 34  3 75 81
15 14 67 64 80
 1 68 84 83 25
19 20 56 78 58

98 38 94 26 30
32 66 59 41 52
40 37 73 18 39
58  4 55 19 27
62 69 51 44 77

48  4  3 60 11
81 76 10  0 80
41 93 25 53 49
14 21 85 38 45
 5 89 12 98 74

10 18 21 71 94
14 64 44 83 47
78 11  5 29  6
56 36 85 73 26
62 30 35  7 39

32 31 71 52 12
62 35 44 95 68
67 29 15 85  9
27 72 58 21 93
11 54 40 41 37

 7 45 74 17  9
99 16 84 63 61
44 21 59 69 66
11 39 80 19 72
89 58 81 42 87

91 84 78  7 95
24  3 85 20 16
29 38 52 41 21
37 23 56 73 54
11 47  5 65 51

93 94 24 74 88
 9 72 82  6 73
92 12 97 34 71
35  3  0  2 19
55 38 67 26 50

56 22 52 96 72
21 67 65 37 64
99 76 51 39 24
90  4 57 75 47
40 32 44 83 34

90 58 56 43 94
99 61 95 18 72
86 20 47 89 53
97 38  0 39 93
85 98 62 21  5

68 20 23  1 63
37 97 47 30 70
66 35 46 36  2
29 87 82 59 10
12  5 39 73 99

 1 82 87 52 42
65 74 46 92 56
25 29 26 34 51
20 10 39 81 32
12 11 89 18 27

94 36 29 72 10
34 47 77 83 18
38 76  6 48 97
41 90 61 69 78
14 74 66 55 33

78 25  8 24 74
60 53 80 50 48
66 92 55 17  3
77 95 18 35 34
 5 11 70  1 82

58 91  5 63 35
99 37 76 90 73
20 50 67 61 17
13 62 54 32 21
47 30 94 68 49

16  1 38 84 56
53 79 14 17 47
23 97 61 24 65
83 89  4 29 90
94 57 50 22 31

 9 50 63 65 17
 8 22 66  4 19
89 71 24 70 68
47  1 56  6 40
62 46 75 43 37

 2 15 10 18 14
45 34 35 77 20
76 55 48 28 26
80 65 68 19 84
 1 94 40 85 11

80 47 69 78 37
88 10 95 86 96
28 14 98 20 35
16  6  8 50 38
51 26 53  9 54

93 75 53  1 16
30 66 76 19 51
69 22 28 31  7
15 36 96 17 97
92 23 45 26 48

96 13 63 81 65
39 47 48  5 31
64 20 95 61 60
 4 83 55 97 14
92 45 88 99 79

11 35 56 27 50
85 40 71 88 99
59 39 84 44 43
22 19 10 12 98
13  7 76 23 21

94 67 79 35 47
83 77  3 11 15
74 10 70 14 44
76  5 80 46 71
20 36 53 88 40

34 24 12 63 47
32 96 67 40 82
42  7 87 61 10
70 14 65 88 74
92  9 53 60 56

43 38 91 30 72
20  3 64 80 32
90 37 28  4 14
96 11  6 84 68
58 60 45 61 47

45 11 65 41 46
29 33 56 26  3
21 90 42 96 27
94 17 32 92 25
43 99 36 70 87

52 99 88 76 66
75 81 48  2 96
55 59 39 97 34
77 37  6 82 47
64 29 32 19 86

 4 20 57 61  8
28 33 42 30 86
34 65 14 40 46
43 49 37 22 39
71 18 11 85 92

68 51 39 93  1
 9 82 35 67 81
 3 12 29  6 83
58 73 97 26 20
 2 28 99 64 77

88 41 91 98 89
82  1 37 10 14
52 44 90 34 21
76 27 43 54 49
31 84 46 57 77

73 99 79 76 43
82 17 14 68 87
39 53 21  7 81
94 45 35 48 32
67 49 62 63 23

99  0 85 12 83
30 45 67 76 87
 7 39 57 66 98
13 82 46 28 24
25 15 90 68 51

69 30 73 61 99
12 13  7 53 20
 8 18 28 82 67
95 79 96 51 29
56 31 92 54 57
))

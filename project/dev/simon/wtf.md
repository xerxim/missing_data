# Funktion mice
data: 3 spalten, X3 hat missings, 10 Zeilen
m = 3
maxit = 5

relevanter aufruf:
q <- sampler(data, m, ignore, where, imp, blocks, method, visitSequence, predictorMatrix, formulas, calltype, blots, post, c(from, to), printFlag, ...)

# sampler
## aufruf
```list(data, m, ignore, where, imp, blocks, method, visitSequence, predictorMatrix, formulas, calltype, blots, post, c(from, to), printFlag)

data:

X1 X2 X3

1 7.204587 13.3567369 NA

2 11.933309 5.6517415 NA

3 3.959069 13.8673936 NA

4 13.997122 6.3358998 NA

5 7.065070 7.4391699 11.63349

6 11.885166 -0.7393102 13.26139

7 7.572462 3.4962347 10.00615

8 7.924379 3.8537045 NA

9 9.895927 6.1764879 14.84014

10 7.873527 9.5362111 14.20893

  

m:

[1] 3

  

ignore:

[1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

  

where:

X1 X2 X3

1 FALSE FALSE TRUE

2 FALSE FALSE TRUE

3 FALSE FALSE TRUE

4 FALSE FALSE TRUE

5 FALSE FALSE FALSE

6 FALSE FALSE FALSE

7 FALSE FALSE FALSE

8 FALSE FALSE TRUE

9 FALSE FALSE FALSE

10 FALSE FALSE FALSE

  

imp:

[[5]]$X1

[1] 1 2 3

<0 rows> (or 0-length row.names)

  

[[5]]$X2

[1] 1 2 3

<0 rows> (or 0-length row.names)

  

[[5]]$X3

1 2 3

1 10.00615 13.26139 14.84014

2 14.20893 14.84014 14.84014

3 10.00615 13.26139 13.26139

4 11.63349 14.84014 14.84014

8 14.20893 14.20893 14.20893

  
  

blocks:

[[6]]$X1

[1] "X1"

  

[[6]]$X2

[1] "X2"

  

[[6]]$X3

[1] "X3"

  
  

method:

X1 X2 X3

"" "" "cart"

  

visitSequence:

[1] "X1" "X2" "X3"

  

predictorMatrix:

X1 X2 X3

X1 0 1 1

X2 1 0 1

X3 1 1 0

  

formulas:

[[10]]$X1

X1 ~ X2 + X3

<environment: 0x557f2659ecc0>

  

[[10]]$X2

X2 ~ X1 + X3

<environment: 0x557f2659ecc0>

  

[[10]]$X3

X3 ~ X1 + X2

<environment: 0x557f2659ecc0>

  
  

calltype:

X1 X2 X3

"pred" "pred" "pred"

  

blots:

[[12]]$X1

list()

  

[[12]]$X2

list()

  

[[12]]$X3

list()

  
  

post:

X1 X2 X3

"" "" ""

  

c(from, to):

[1] 1 5

  

printFlag:

[1] TRUE
```
## loops
mehrere loops in der funktion:

### loop1: über maxit
k in from:to -> 1 - 5 
#### loop 1.1: über m
i in seq_len(m) -> 1 - 3

##### loop 1.1.1: über Spalten
h in visitSequence -> "X1" "X2" "X3"
hier loop über blocks:
!!! Blocks und spalten sind gleich !!!

Missings werden mit hot deck aufgefüllt

##### loop 1.1.2: über Spalten

hier drei unterscheidungen:

```
method

X1 X2 X3

"" "" "cart"
#####################

empt <- theMethod == ""

univ <- !empt && !is.passive(theMethod) && !handles.format(paste0("mice.impute.", theMethod))

mult <- !empt && !is.passive(theMethod) && handles.format(paste0("mice.impute.", theMethod))

pass <- !empt && is.passive(theMethod) && length(blocks[[h]]) == 1

```
Spalten ohne misssings werden übersprungen und X3 landet in univ:
wieder loop über block (=spalten, also pseudoloop in unserem fall)

Hier Aufruf 
`imp[[j]][, i] <- sampler.univ(data = data, r = r, where = where, pred = pred, formula = ff, method = theMethod, yname = j, k = k, ct = ct, user = user, ignore = ignore, ...)`

==Diese imputationen ersetzen das erste hot deck!==
# sampler.univ
## Aufruf
```
list(data = data, r = r, where = where, pred = pred, formula = ff, method = theMethod, yname = j, k = k, ct = ct, user = user, ignore = ignore, ...)

$data

X1 X2 X3

1 7.204587 13.3567369 10.00615

2 11.933309 5.6517415 14.20893

3 3.959069 13.8673936 10.00615

4 13.997122 6.3358998 11.63349

5 7.065070 7.4391699 11.63349

6 11.885166 -0.7393102 13.26139

7 7.572462 3.4962347 10.00615

8 7.924379 3.8537045 14.20893

9 9.895927 6.1764879 14.84014

10 7.873527 9.5362111 14.20893

  

$r

X1 X2 X3

[1,] TRUE TRUE FALSE

[2,] TRUE TRUE FALSE

[3,] TRUE TRUE FALSE

[4,] TRUE TRUE FALSE

[5,] TRUE TRUE TRUE

[6,] TRUE TRUE TRUE

[7,] TRUE TRUE TRUE

[8,] TRUE TRUE FALSE

[9,] TRUE TRUE TRUE

[10,] TRUE TRUE TRUE

  

$where

X1 X2 X3

1 FALSE FALSE TRUE

2 FALSE FALSE TRUE

3 FALSE FALSE TRUE

4 FALSE FALSE TRUE

5 FALSE FALSE FALSE

6 FALSE FALSE FALSE

7 FALSE FALSE FALSE

8 FALSE FALSE TRUE

9 FALSE FALSE FALSE

10 FALSE FALSE FALSE

  

$pred

X1 X2 X3

1 1 0

  

$formula

NULL

  

$method

X3

"cart"

  

$yname

[1] "X3"

  

$k

[1] 1

  

$ct

[1] "pred"

  

$user

list()

  

$ignore

[1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
```
## Funktion

vorbereitung parameter und formeln
hier aufruf:
```
args <- c(list(y = y, ry = ry, x = x, wy = wy, type = type), user, list(...))

imputes[cc] <- do.call(f, args = args)

imputes
```

# mice.impute.cart
## Aufruf
```
$y

[1] 10.00615 14.20893 10.00615 11.63349 11.63349 13.26139 10.00615 14.20893 14.84014 14.20893

  

$ry

[1] FALSE FALSE FALSE FALSE TRUE TRUE TRUE FALSE TRUE TRUE

  

$x

X1 X2

1 7.204587 13.3567369

2 11.933309 5.6517415

3 3.959069 13.8673936

4 13.997122 6.3358998

5 7.065070 7.4391699

6 11.885166 -0.7393102

7 7.572462 3.4962347

8 7.924379 3.8537045

9 9.895927 6.1764879

10 7.873527 9.5362111

  

$wy

1 2 3 4 5 6 7 8 9 10

TRUE TRUE TRUE TRUE FALSE FALSE FALSE TRUE FALSE FALSE

  

$type

X1 X2

1 1
```
## Funktion





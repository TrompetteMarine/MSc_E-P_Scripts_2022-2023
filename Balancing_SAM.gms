$title Social Accounting Matrix Balancing Problem (SAMBAL,SEQ=77)

$onText
A Social Accounting Matrix captures all the circular flows in an
economy and is called balanced if the row total equal the column
totals. A sample problem illustrates the use of Nonlinear Programming
to balance such matrices.


Zenios, S A, Drud, A S, and Mulvey, J, Balancing some large Social
Accounting Matrices with Nonlinear Programming. Tech. rep.,
Department of Civil Engineering, Princeton University, 1986.

Keywords: nonlinear programming, social accounting matrix, statistics
$offText

Set i 'accounts' / AZ0,DE0, C10, C20, C30, C40, C50, FZ0, GZ0, HZ0, IZ0, JZ0, KZ0, LZ0, MN0, OQ0, RU0, IDT, TRF, LAB, KAP, HOU, COR, GOV, S-I, RoW /;

Alias (i,j);

Table xb(i,j) 'original estimate'
    AZ0 DE0 C10 C20 C30 C40 C50 FZ0 GZ0 HZ0 IZ0 JZ0 KZ0 LZ0 MN0 OQ0 RU0 IDT TRF LAB  KAP HOU COR  GOV S-I RoW
AZ0 17  2   8   3   0   0   14  0   0   0   0   0   2   0   3   0   0                    36           5   15
DE0 0   70  0   1   4   0   11  2   1   1   0   1   2   0   10  1   0                    55           0   10
C10 39  4   35  1   1   0   11  0   1   3   0   1   3   1   15  1   1                    179          2   48
C20     22  0   2   1   0   3   0   0   1   0   0   0   0   2   0   0                    44           0   12
C30 0   1   0   0   18  1   21  1   1   1   0   1   1   0   7   1   0                    32           43  90
C40     1   0   0   20  43  30  0   1   1   0   1   1   0   9   1   1                    71           45  124
C50 3   21  4   8   16  1   127 1   4   6   1   2   4   2   29  2   1                    155          41  199
FZ0 0   4   1   2   15  0   58  48  1   3   0   2   5   1   30  1   0                    19           222 
GZ0 0   6   3   5   6   9   18  0   19  35  8   11  15  19  58  3   3                    14               7
HZ0     2   1   14  2   2   4   1   3   42  2   2   7   3   22  3   1                    41               34
IZ0 2   2   30  0   0   0   2   0   0   2   2   1   2   2   7   0   1                    86               
JZ0 0   4   1   1   6   0   12  1   2   5   2   30  4   4   21  2   1                    42           75  20
KZ0 0   1   0   1   0   0   3   2   0   3   1   18  78  8   28  1   1                    66               14
LZ0     2   0   0   1   0   3   5   0   1   1   1   18  11  13  0   0                    233          6   
MN0 0   6   4   3   8   1   15  3   3   10  6   18  16  16  148 4   3                    25           97  78
OQ0 0   11  9   3   4   4   25  8   1   9   4   8   8   5   37  11  1                    64               1
RU0 0   2   2   1   3   0   6   1   1   2   1   3   2   2   11  0   4                    46           2   5
LAB 10  21  26  1   19  15  79  71  140 61  35  63  53  16  197 347 46                                   
KAP 32  27  18  2   10  14  42  40  68  31  19  42  18  221 79  106 16                                   
IDT 2   3   2   0   1   1   6   4   10  6   2   4   10  29  11  13  2                                    
TRF 1   15  32  40  8   16  30  26  2   -4  8   10  20  3   33  2   8                                    
HOU                                                                             1200 786  36  31   484     6
COR                                                                             291  24            34      140
GOV                                                                     106 250 543  76   21  8            8
S-I                                                                                       115 31   -37     17
RoW                                                                                           -140     2       ; 

Parameter
   tb(i)   'original totals'   /  AZ0 106 ,DE0 227, C10 178, C20 88, C30 145, C40 111, C50 518, FZ0 213, GZ0 259, HZ0 218, IZ0 94, JZ0 219, KZ0 271, LZ0 342, MN0 770, OQ0 499, RU0 91, IDT 106, TRF 250, LAB 2034, KAP 886, HOU 1381, COR 70, GOV - 484, S-I 538, RoW 827/
   tw(i)   'wights for totals'
   xw(i,j) 'weights for cells';

tw(i)   = 1;
xw(i,j) = 1$xb(i,j);
tw(i)$(mapval(tb(i))     = mapval(na)) = 0;
xw(i,j)$(mapval(xb(i,j)) = mapval(na)) = 0;

display tw, xw;

Variable
   x(i,j)  'estimated cells'
   t(i)    'estimated totals'
   dev     'deviations';

Equation
   rbal(i) 'row balance'
   cbal(j) 'column balance'
   devsqr  'definition of square deviations';

rbal(i).. t(i) =e= sum(j$xb(i,j), x(i,j));

cbal(j).. t(j) =e= sum(i$xb(i,j), x(i,j));

devsqr..  dev  =e= sum((i,j)$xw(i,j), xw(i,j)*sqr(xb(i,j) - x(i,j))/xb(i,j))
                +  sum(i$tw(i), tw(i)*sqr(tb(i) - t(i))/tb(i));

Model bal / all /;

x.l(i,j) = xb(i,j)$xw(i,j);
t.l(i)   = tb(i)$tw(i);

solve bal using nlp minimizing dev;

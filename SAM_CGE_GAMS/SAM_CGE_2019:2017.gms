$title CGE model for aggregated Households for the year 2019 based on 2017 prices adjusted for inflation

$onText
No description.


Paris School of Economics, Master degree Dissertation
Bontemps Gabriel

Keywords: nonlinear programming, general equilibrium model, social accounting
          matrix, utility maximization problem
$offText

Set
   u    'SAM entry' / AZ0,DE0, C10, C20, C30, C40, C50, FZ0, GZ0, HZ0, IZ0, JZ0, KZ0, LZ0, MN0, OQ0, RU0, IDT, TRF, LAB, KAP, HOH, COR, GOV, S-I, RoW/
   i(u) 'goods'     / AZ0,DE0, C10, C20, C30, C40, C50, FZ0, GZ0, HZ0, IZ0, JZ0, KZ0, LZ0, MN0, OQ0, RU0 /
   h(u) 'factor'    / LAB,  KAP/;

Alias (u,v), (i,j), (h,k);

Table SAM(u,v) 'social accounting matrix'
     AZ0  DE0  C10           C20  C30      C40     C50     FZ0      GZ0    HZ0   IZ0   JZ0   KZ0    LZ0    MN0   OQ0   RU0   IDT  TRF    LAB      KAP          HOH       COR     GOV     S-I      RoW
AZ0  17    2   1546          163  0.1       0.1     1       14       0.1    0.1   0.1   0.1    0.1   0.1   11    3     1      2    1      23      2091          36               14      5         15
DE0  1     225 26            1    0.1       2       166     15       2      1     1     2      1     0.1   10    22    7      1    11     8       1289          55               2071    0.1       10
C10  98    4   109           1    1         0.1     20      2        9      3     277   2      3     1     37    130   1      7    1385   438     24685         179              0.1     2         48
C20  5     2   0.1           2    1         0.1     8       0.1      12     51    2     1      2     0.1   8     7     7      2    370    2       3             44               0.1     0.1       12
C30  0.1   28  1             2    1         430     200     31       24     7     0.1   23     1     2     121   30    11     3    75     361     10576         32               0.1     43        90
C40  0.1   1   0.1           0.1  2         283     3       0.1      177    13    0.1   1      0.1   9     4     53    2      11   653    193     30144         71               0.1     45        124
C50  38    58  64            3    190       612     1       450      129    13    2     32     6     5     80    247   1      12   106    2475    17397         155              0.1     41        199
FZ0  0.1   6   5             2    0.1       0.1     1       94       1      2     0.1   2      6     18    8     86    1      2    193    3930    39136         19               0.1     0.1       222 
GZ0  0.1   1   1             5    0.1       2       3       0.1      22     4     8     2      15    19    5     1     1      2    2      2841    2929          14               0.1     0.1       7
HZ0  0.1   1   4             0.1  0.1       2       3       1        73     64    0.1   2      4     40    40    30    1      3     8     110     337           41               0.1     0.1       34
IZ0  2     2   30            0.1  0.1       0.1     1       0.1      35     4     2     3      2     22    7     18    1      3    26     623     530           86               0.1     0.1       0.1
JZ0  0.1   1   1             1    0.1       2       0.1     1        18     3     1     27     130   193   21    42    3      3    8      325     5648          42               0.1     75        20
KZ0  0.1   2   5             1    0.1       2       8       14       5      4     1     3      181   85    28    16    0.1    19   94     108     3698          66               0.1     0.1       14
LZ0  0.1   2   0.1           0.1    1       0.1     0.1     0.1      24     4     0.1   3      30    170   13    12    0.1    0.1  3      61      7332          233              0.1     6         0.1
MN0  1     4   120           2    21        212     223     341      477    79    9     141    19    1116  148   55    29     3    641    5853    7968          25               0.1     97        78
OQ0  0.1   1   9             3    0.1       2       1       8        2      2     4     0.1    1     7     37    16    1      18   0.1    12407   16765         64               0.1     0.1       1
RU0  0.1   2   2             1    3         2       0.1     1        3      1     1     1      1     6     11    2     4      2    5      295     105           46               0.1     2         5
LAB  10    21  26            1    19        15      79      71       140    61    35    63     53    16    197   347   46     0.1  0.1    0.1     0.1                           705722   36475     735499                    
KAP  32    27  18            2    10        14      42      40       68     31    19    42     18    221   79    106   16     0.1  0.1    0.1     0.1                           136049   63        44529                   
IDT  2     3   2             0.1  1         1       6       4        10     6     2     4      10    29    11    13    2      0.1  0.1    0.1     0.1                            0.1     0.1       0.1                                    
TRF  1     15  32            40   8         16      30      26       2      4     8     10     20    3     33    2     8      0.1  0.1    0.1     0.1                            0.1     0.1       0.1               
HOH  455   833 185078        672  789       10105   48      102     43     638    59   395    3310  7706  291    3708  416    0.1  0.1    1200    786           92       31        103   847391    6
COR                                                                                                                           0.1  0.1    291     24            369      34         64   48        140
GOV 0.1    0.1 0.1           0.1  0.1       0.1    0.1     0.1       0.1    0.1   0.1   0.1    0.1   0.1   0.1   0.1   0.1    106  250    543     76            1618     463       0.1   797819    40279
S-I 14     0.1 10            0.1  2433      4154   1621    33974     0.1    0.1   0.1   0.1    5134  0.1   13    2270  0.1    3    0.1    0.1     0.1           115      31         37   0.1       285
RoW 13     5   1985          42   6657      13514  13756   0.1       0.1    234   0.1   108    11    0.1   868   1     3      0.1  0.1    0.1     0.1           10       4716       26   185       0.1   ;

* Loading the initial values   
Parameter
   Y0(j)   'composite factor'
   F0(h,j) 'the h-th factor input by the j-th firm'
   X0(i,j) 'intermediate input'
   Z0(j)   'output of the j-th good'
   Xp0(i)  'household consumption of the i-th good '
   Xg0(i)  'government consumption'
   Xv0(i)  'investment demand'
   E0(i)   'exports'
   M0(i)   'imports'
   Q0(i)   "Armington's composite good"
   D0(i)   'domestic good'
   Sp0     'private saving'
   Sg0     'government saving'
   Td0     'direct tax'
   Tz0(j)  'production tax'
   Tm0(j)  'import tariff'
   FF(h)   'factor endowment of the h-th factor'
   Sf      'foreign saving in EUR'
   pWe(i)  'export price in EUR'
   pWm(i)  'import price in EUR'
   tauz(i) 'production tax rate'
   taum(i) 'import tariff rate';

Td0     = SAM("GOV","HOH");
Tz0(j)  = SAM("IDT",j);
Tm0(j)  = SAM("RoW",j);
F0(h,j) = SAM(h,j);
Y0(j)   = sum(h, F0(h,j));
X0(i,j) = SAM(i,j);
Z0(j)   = Y0(j) +sum(i, X0(i,j));
M0(i)   = SAM("RoW",i);
tauz(j) = Tz0(j)/Z0(j);
taum(j) = Tm0(j)/M0(j);
Xp0(i)  = SAM(i,"HOH");
FF(h)   = SAM("HOH",h);
Xg0(i)  = SAM(i,"GOV");
Xv0(i)  = SAM(i,"S-I");
E0(i)   = SAM(i,"RoW");
Q0(i)   = Xp0(i)+Xg0(i)+Xv0(i)+sum(j, X0(i,j));
D0(i)   = (1+tauz(i))*Z0(i)-E0(i);
Sp0     = SAM("S-I","HOH");
Sg0     = SAM("S-I","GOV");
Sf      = SAM("S-I","RoW");
pWe(i)  = 1;
pWm(i)  = 1;

display Y0, F0, X0, Z0, Xp0, Xg0, Xv0, E0, M0, Q0, D0, Sp0, Sg0, Td0, Tz0, Tm0
        FF, Sf, tauz, taum;

* Calibration
Parameter
   sigma(i) 'elasticity of substitution'
   psi(i)   'elasticity of transformation'
   eta(i)   'substitution elasticity parameter'
   phi(i)   'transformation elasticity parameter'
   pii(i)   'inflation level on a 2017 basis';

sigma(i) =  2;
psi(i)   =  2;
pii(i)   = 0.15;
eta(i)   = (sigma(i) - 1)/sigma(i);
phi(i)   = (psi(i) + 1)/psi(i);

Parameter
   alpha(i)  'share parameter in utility func.'
   beta(h,j) 'share parameter in production func.'
   b(j)      'scale parameter in production func.'
   ax(i,j)   'intermediate input requirement coeff.'
   ay(j)     'composite fact. input req. coeff.'
   mu(i)     'government consumption share'
   lambda(i) 'investment demand share'
   deltam(i) 'share par. in Armington func.'
   deltad(i) 'share par. in Armington func.'
   gamma(i)  'scale par. in Armington func.'
   xid(i)    'share par. in transformation func.'
   xie(i)    'share par. in transformation func.'
   theta(i)  'scale par. in transformation func.'
   ssp       'average propensity for private saving'
   ssg       'average propensity for gov. saving'
   taud      'direct tax rate';

alpha(i)  =  Xp0(i)/sum(j, Xp0(j));
beta(h,j) =  F0(h,j)/sum(k, F0(k,j));
b(j)      =  Y0(j)/prod(h, F0(h,j)**beta(h,j));
ax(i,j)   =  X0(i,j)/Z0(j);
ay(j)     =  Y0(j)/Z0(j);
mu(i)     =  Xg0(i)/sum(j, Xg0(j));
lambda(i) =  Xv0(i)/(Sp0+Sg0+Sf);
deltam(i) = (1+taum(i))*M0(i)**(1-eta(i))/((1+taum(i))*M0(i)**(1-eta(i)) + D0(i)**(1-eta(i)));
deltad(i) =  D0(i)**(1-eta(i))/((1+taum(i))*M0(i)**(1-eta(i)) + D0(i)**(1-eta(i)));
gamma(i)  =  Q0(i)/(deltam(i)*M0(i)**eta(i)+deltad(i)*D0(i)**eta(i))**(1/eta(i));
xie(i)    =  E0(i)**(1-phi(i))/(E0(i)**(1-phi(i))+D0(i)**(1-phi(i)));
xid(i)    =  D0(i)**(1-phi(i))/(E0(i)**(1-phi(i))+D0(i)**(1-phi(i)));
theta(i)  =  Z0(i)/(xie(i)*E0(i)**phi(i)+xid(i)*D0(i)**phi(i))**(1/phi(i));
ssp       =  Sp0/sum(h, FF(h));
ssg       =  Sg0/(Td0+sum(j, Tz0(j))+sum(j, Tm0(j)));
taud      =  Td0/sum(h, FF(h));

display alpha, beta,  b,   ax,  ay, mu, lambda, deltam, deltad, gamma, xie
        xid,   theta, ssp, ssg, taud;

Variable
   Y(j)    'composite factor'
   F(h,j)  'the h-th factor input by the j-th firm'
   X(i,j)  'intermediate input'
   Z(j)    'output of the j-th good'
   Xp(i)   'household consumption of the i-th good'
   Xg(i)   'government consumption'
   Xv(i)   'investment demand'
   E(i)    'exports'
   M(i)    'imports'
   Q(i)    "Armington's composite good"
   D(i)    'domestic good'
   pf(h)   'the h-th factor price'
   py(j)   'composite factor price'
   pz(j)   'supply price of the i-th good'
   pq(i)   "Armington's composite good price"
   pe(i)   'export price in local currency'
   pm(i)   'import price in local currency'
   pd(i)   'the i-th domestic good price'
   epsilon 'exchange rate'
   Sp      'private saving'
   Sg      'government saving'
   Td      'direct tax'
   Tz(j)   'production tax'
   Tm(i)   'import tariff'
   UU      'utility [fictitious]';

Equation
   eqpy(j)   'composite factor agg. func.'
   eqF(h,j)  'factor demand function'
   eqX(i,j)  'intermediate demand function'
   eqY(j)    'composite factor demand function'
   eqpzs(j)  'unit cost function'
   eqTd      'direct tax revenue function'
   eqTz(j)   'production tax revenue function'
   eqTm(i)   'import tariff revenue function'
   eqXg(i)   'government demand function'
   eqXv(i)   'investment demand function'
   eqSp      'private saving function'
   eqSg      'government saving function'
   eqXp(i)   'household demand function'
   eqpe(i)   'world export price equation'
   eqpm(i)   'world import price equation'
   eqepsilon 'balance of payments'
   eqpqs(i)  'Armington function'
   eqM(i)    'import demand function'
   eqD(i)    'domestic good demand function'
   eqpzd(i)  'transformation function'
   eqDs(i)   'domestic good supply function'
   eqE(i)    'export supply function'
   eqpqd(i)  'market clearing cond. for comp. good'
   eqpf(h)   'factor market clearing condition'
   obj       'utility function [fictitious]';

* domestic production
eqpy(j)..   Y(j)   =e= b(j)*prod(h, F(h,j)**beta(h,j));

eqF(h,j)..  F(h,j) =e= beta(h,j)*py(j)*Y(j)/pf(h);

eqX(i,j)..  X(i,j) =e= ax(i,j)*Z(j);

eqY(j)..    Y(j)   =e= ay(j)*Z(j);

eqpzs(j)..  pz(j)  =e= ay(j)*(1+pii(j))*py(j) + sum(i, ax(i,j)*pq(i)*(1+pii(i)));

* government behavior
eqTd..      Td     =e= taud*sum(h, pf(h)*FF(h));

eqTz(j)..   Tz(j)  =e= tauz(j)*(1+pii(j))*pz(j)*Z(j);

eqTm(i)..   Tm(i)  =e= taum(i)*(1+pii(i))*pm(i)*M(i);

eqXg(i)..   Xg(i)  =e= mu(i)*(Td + sum(j, Tz(j)) + sum(j, Tm(j)) - Sg)/((1+pii(i))*pq(i));

* investment behavior
eqXv(i)..   Xv(i)  =e= lambda(i)*(Sp + Sg + epsilon*Sf)/((1+pii(i))*pq(i));

* savings
eqSp..      Sp     =e= ssp*sum(h, (pf(h)/(1.15))*FF(h));

eqSg..      Sg     =e= ssg*(Td + sum(j, Tz(j)) + sum(j, Tm(j)));

* household consumption
eqXp(i)..   Xp(i)  =e= alpha(i)*(sum(h, (1+pii(i))*pf(h)*FF(h)) - Sp - Td)/((1+pii(i))*pq(i));

* international trade
eqpe(i)..   pe(i)  =e= epsilon*pWe(i);

eqpm(i)..   pm(i)  =e= epsilon*pWm(i);

eqepsilon.. sum(i, (1+pii(i))*pWe(i)*E(i)) + Sf =e= sum(i, (1+pii(i))*pWm(i)*M(i));

* Armington function
eqpqs(i)..  Q(i)   =e=  gamma(i)*(deltam(i)*M(i)**eta(i) + deltad(i)*D(i)**eta(i))**(1/eta(i));

eqM(i)..    M(i)   =e= (gamma(i)**eta(i)*deltam(i)*pq(i)/((1+taum(i))*pm(i))*(1+pii(i)))**(1/(1-eta(i)))*Q(i);

eqD(i)..    D(i)   =e= (gamma(i)**eta(i)*deltad(i)*pq(i)*(1+pii(i))/pd(i))**(1/(1-eta(i)))*Q(i);

* transformation function
eqpzd(i)..  Z(i)   =e=  theta(i)*(xie(i)*E(i)**phi(i)+xid(i)*D(i)**phi(i))**(1/phi(i));

eqE(i)..    E(i)   =e= (theta(i)**phi(i)*xie(i)*(1+tauz(i))*pz(i)/pe(i))**(1/(1-phi(i)))*Z(i);

eqDs(i)..   D(i)   =e= (theta(i)**phi(i)*xid(i)*(1+tauz(i))*pz(i)/pd(i))**(1/(1-phi(i)))*Z(i);

* market clearing condition
eqpqd(i)..  Q(i)   =e= Xp(i) + Xg(i) + Xv(i) + sum(j, X(i,j));

eqpf(h)..   sum(j, F(h,j)) =e= FF(h);

* fictitious objective function
obj..       UU     =e= prod(i, Xp(i)**alpha(i));

* Initializing variables
Y.l(j)    = Y0(j);
F.l(h,j)  = F0(h,j);
X.l(i,j)  = X0(i,j);
Z.l(j)    = Z0(j);
Xp.l(i)   = Xp0(i);
Xg.l(i)   = Xg0(i);
Xv.l(i)   = Xv0(i);
E.l(i)    = E0(i);
M.l(i)    = M0(i);
Q.l(i)    = Q0(i);
D.l(i)    = D0(i);
pf.l(h)   = 1;
py.l(j)   = 1;
pz.l(j)   = 1;
pq.l(i)   = 1;
pe.l(i)   = 1;
pm.l(i)   = 1;
pd.l(i)   = 1;
epsilon.l = 1;
Sp.l      = Sp0;
Sg.l      = Sg0;
Td.l      = Td0;
Tz.l(j)   = Tz0(j);
Tm.l(i)   = Tm0(i);

* Setting lower bounds to avoid division by zero
Y.lo(j)    = 0.00001;
F.lo(h,j)  = 0.00001;
X.lo(i,j)  = 0.00001;
Z.lo(j)    = 0.00001;
Xp.lo(i)   = 0.00001;
Xg.lo(i)   = 0.00001;
Xv.lo(i)   = 0.00001;
E.lo(i)    = 0.00001;
M.lo(i)    = 0.00001;
Q.lo(i)    = 0.00001;
D.lo(i)    = 0.00001;
pf.lo(h)   = 0.00001;
py.lo(j)   = 0.00001;
pz.lo(j)   = 0.00001;
pq.lo(i)   = 0.00001;
pe.lo(i)   = 0.00001;
pm.lo(i)   = 0.00001;
pd.lo(i)   = 0.00001;
epsilon.lo = 0.00001;
Sp.lo      = 0.00001;
Sg.lo      = 0.00001;
Td.lo      = 0.00001;
Tz.lo(j)   = 0.00001;
Tm.lo(i)   = 0.00001;

* numeraire
pf.fx("LAB") = 1;

Model stdcge / all /;

solve stdcge maximizing UU using nlp;

* Simulation Runs: Abolition of Import Tariffs
taum(i) = 0;

option bRatio = 1;

solve stdcge maximizing UU using nlp;

* List8.1: Display of changes
Parameter
   dY(j), dF(h,j), dX(i,j), dZ(j),    dXp(i),     dXg(i), dXv(i)
   dE(i), dM(i),   dQ(i),   dD(i),    dpf(h),     dpy(j), dpz(i), dpq(i)
   dpe(i),dpm(i),  dpd(i),  depsilon, dTd,dTz(i), dTm(i), dSp,    dSg;

dY(j)    = (Y.l(j)  /Y0(j)  -1)*100;
dF(h,j)  = (F.l(h,j)/F0(h,j)-1)*100;
dX(i,j)  = (X.l(i,j)/X0(i,j)-1)*100;
dZ(j)    = (Z.l(j)  /Z0(j)  -1)*100;
dXp(i)   = (Xp.l(i) /Xp0(i) -1)*100;
dXg(i)   = (Xg.l(i) /Xg0(i) -1)*100;
dXv(i)   = (Xv.l(i) /Xv0(i) -1)*100;
dE(i)    = (E.l(i)  /E0(i)  -1)*100;
dM(i)    = (M.l(i)  /M0(i)  -1)*100;
dQ(i)    = (Q.l(i)  /Q0(i)  -1)*100;
dD(i)    = (D.l(i)  /D0(i)  -1)*100;
dpf(h)   = (pf.l(h) /1 -1)*100;
dpy(j)   = (py.l(j) /1 -1)*100;
dpz(j)   = (pz.l(j) /1 -1)*100;
dpq(i)   = (pq.l(i) /1 -1)*100;
dpe(i)   = (pe.l(i) /1 -1)*100;
dpm(i)   = (pm.l(i) /1 -1)*100;
dpd(i)   = (pd.l(i) /1 -1)*100;
depsilon = (epsilon.l/1 -1)*100;
dTd      = (Td.l    /Td0    -1)*100;
dTz(j)   = (Tz.l(j) /Tz0(j) -1)*100;
dTm(i)   = (Tm.l(i) /Tm0(i) -1)*100;
dSp      = (Sp.l    /Sp0    -1)*100;
dSg      = (Sg.l    /Sg0    -1)*100;

display dY,  dF,  dX,  dZ,  dXp,      dXg, dXv, dE,  dM,  dQ, dD, dpf, dpy, dpz
        dpq, dpe, dpm, dpd, depsilon, dTd, dTz, dTm, dSp, dSg;

* Welfare measure: Hicksian equivalent variations
Parameter
   UU0 'utility level in the base run eq.'
   ep0 'expenditure func. in the base run eq.'
   ep1 'expenditure func. in the C-f eq.'
   EV  'Hicksian equivalent variations';

UU0 = prod(i, Xp0(i)**alpha(i));
ep0 = UU0 /prod(i, (alpha(i)/1)**alpha(i));
ep1 = UU.l/prod(i, (alpha(i)/1)**alpha(i));
EV  = ep1 - ep0;

display EV;

* List A.1: an example of CSV file generation
File listA1out / listA1.csv /;
put  listA1out;

listA1out.pc=5;

* putting a note
put "This is an example of usage of the Put command." / /;

* putting dXp(i)
put "dXp(i)" /;
loop(i, put i.tl dXp(i) /;);
put / /;

* putting dTd
put "dTd" dTd /;
put / /;

* putting F(h,j)
put "F(h,j)" /;
put "";
loop(j, put j.tl);
put /;
loop(h,
   put h.tl;
   loop(j, put F.l(h,j););
   put /;
);
put / /;

* putting beta(h,j)
put "beta(h,j)" /;
put "";
loop(j, put j.tl);
put /;
loop(h,
   put h.tl;
   loop(j, put beta(h,j););
   put /;
);
put / /;
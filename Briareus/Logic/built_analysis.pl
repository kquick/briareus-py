:- [reporules,reportrules,analysis],
   setof(X, report(X), CFGS),
   findall(A, analysis(A), AS),
   findall(N, action(N), NS),
   findall(D, do(D), DS),
   append([CFGS,AS,NS,DS], PRINTS),
   print(PRINTS),
   halt.

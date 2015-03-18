 f(x):= sin(x);
                           f(x) := sin(x)
 t(x):=taylor(f(x), x, 0, 1);
                    t(x) := taylor(f(x), x, 0, 1)
t2(x):=taylor(f(x), x, 0, 3);
                   t2(x) := taylor(f(x), x, 0, 3)
t3(x):=taylor(f(x), x, 0, 5);
                   t3(x) := taylor(f(x), x, 0, 5)
(%i5) t4(x):=taylor(f(x), x, 0, )                   t4(x) := taylor(f(x), x, 0, 7)
(%i6) fortran(t(x));
      x
(%o6)                                done
(%i7) fortran(t2(x));
      x-x**3/6.0E+0
(%o7)                                done
(%i8) fortran(t3(x));
      x**5/1.2E+2-x**3/6.0E+0+x
(%o8)                                done
(%i9) fortran(t4(x));
      -x**7/5.04E+3+x**5/1.2E+2-x**3/6.0E+0+x
(%o9)                                done
(%i10) tex(t(x));
$$+x+\cdots $$
(%o10)                               false
(%i11) tex(t2(x));
$$x-{{x^3}\over{6}}+\cdots $$
(%o11)                               false
(%i12) tex(t3(x));
$$x-{{x^3}\over{6}}+{{x^5}\over{120}}+\cdots $$
(%o12)                               false
(%i13) tex(t4(x));
$$x-{{x^3}\over{6}}+{{x^5}\over{120}}-{{x^7}\over{5040}}+\cdots $$
(%o13)                               false
(%i14) plot2d ([f(x),t(x), t2(x), t3(x), t4(x)], [x, -%pi, %pi]);
(%o14) 

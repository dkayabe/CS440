  t0 = 1 + 1
  two = t0
  t3 = two - 0
  jneg l0 t3
  jump l1
l0:
  two2 = two
l2:
  t2 = two2 - 0
  jneg l3 t2
  jump l4
l3:
  t1 = two2 + 1
  two2 = t1
  jump l2
l4:
l1:
  n = arg
  r = 0
  t4 = n - 0
  jneg l5 t4
  jump l7
l7:
  t5 = n - 0
  jz l5 t5
  jump l6
l5:
  ret r
l6:
l8:
  t16 = 0 - n
  jneg l9 t16
  jump l14
l14:
  t17 = 0 - n
  jneg l9 t17
  jump l10
l9:
  one = 1
  t6 = one + two
  three = t6
  t7 = three + one
  four = t7
  t8 = n / two
  ndiv = t8
  t9 = n - one
  t10 = t9 / two
  nmdiv = t10
  newn = n
  t14 = ndiv - nmdiv
  jz l11 t14
  jump l12
l11:
  t12 = n - one
  t13 = t12 / two
  newn = t13
  jump l13
l12:
  t11 = n / two
  newn = t11
l13:
  n = newn
  t15 = r + one
  r = t15
  jump l8
l10:
  ret r

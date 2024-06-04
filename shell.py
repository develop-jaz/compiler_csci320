import basic


text = ['3+2-5/5 * 7', '(5-2)>(4/4)', 
    '(3>1) && (2<5)', '!(6>7)', '(4/2 >= 2) ||(2<1)']

for expr in text:
    result, error = basic.run(expr)

    if error: print(error.as_string())
    else: print(expr, ": ", result)
    
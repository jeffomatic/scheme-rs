use super::eval::{eval_sequence, Env, EnvPtr, Value};
use super::lex::scan;
use super::parse::parse;

fn define(env: EnvPtr) {
    let stdlib = "
      (define list (lambda (. rest) rest))
      (define null? (lambda (v) (= v ())))
      (define list? (lambda (v)
                      (if (empty? v)
                        #t
                        (if (not (pair? v))
                          #f
                          (list? (cdr v))))))
    ";
    eval_sequence(&parse(&scan(stdlib).unwrap()).unwrap(), env).unwrap();
}

#[test]
fn test() {
    let env = Env::root().into_ptr();
    define(env.clone());

    let cases = &[(
        "(list 1 2 3)",
        Value::make_list(&[
            Value::Number(1.0).into_ptr(),
            Value::Number(2.0).into_ptr(),
            Value::Number(3.0).into_ptr(),
        ]),
        ("(null? ())", Value::Boolean(true)),
        ("(null? #f", Value::Boolean(false)),
        ("(list? ()", Value::Boolean(true)),
        ("(list? (list 1)", Value::Boolean(true)),
        ("(list? 1234)", Value::Boolean(false)),
        ("(list? (cons 1 2))", Value::Boolean(false)),
    )];

    for c in cases.iter() {
        let got = eval_sequence(&parse(&scan(c.0).unwrap()).unwrap(), env.clone()).unwrap();
        assert_eq!(&*got.borrow(), &c.1, "src: {}", c.0);
    }
}

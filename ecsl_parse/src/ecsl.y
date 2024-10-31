%start Item
%%

Item -> Result<Item, ()>:
    'FN' 'IDENT' 'LBRACKET' FnArgs TrailingComma 'RBRACKET' ReturnTy {
        Ok(Item::new(
            $span,
            ItemKind::Fn(P::new(
                FnDef {
                    span: $span,
                    kind: FnKind::Fn,
                    ident: Ident($2.map_err(|_| ())?.span()),
                    params: $4?,
                    ret: $7?,
                    // block:
                }
            ))))
    }
    ;

FnArgs -> Result<Vec<Param>, ()>:
    Arg { Ok(vec![$1?]) }
    | FnArgs 'COMMA' Arg {
        flatten($1, $3)
    }
    ;

Arg -> Result<Param, ()>:
    'IDENT' 'COLON' Ty {
        Ok(Param {
            span: $span,
            ident: Ident($1.map_err(|_| ())?.span()),
            ty: P::new($3?)
            }
        )
    }
    ;

TrailingComma -> ():
    'COMMA' { () }
    | { () }
    ;

ReturnTy -> Result<RetTy, ()>:
    'ARROW' Ty {
        { Ok(RetTy::Ty(P::new($2?))) }
    }
    | {Ok(RetTy::None($span))}
    ;

Ty -> Result<Ty, ()>:
    'IDENT' {
        Ok(Ty::new($span, TyKind::Ident(Ident($span))))
    }
    | 'LSQUARE' Ty 'INT' 'RSQUARE' {
        Ok(Ty::new($span, TyKind::Array(
            P::new($2?), $3.map_err(|_| ())?.span()
        )))
    }
    ;


%%

use ecsl_ast::parse::*;

fn flatten<T>(lhs: Result<Vec<T>, ()>, rhs: Result<T, ()>)
           -> Result<Vec<T>, ()>
{
    let mut flt = lhs?;
    flt.push(rhs?);
    Ok(flt)
}
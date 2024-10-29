%start Item
%%

Item -> Result<Item, ()>:
    'FN' 'IDENT' 'LBRACKET' 'RBRACKET' ReturnTy {
        Ok(Item::new($span, ItemKind::Fn))
    }
    ;

// FnArgs -> Result<Vec<Param>, ()>:
//     Arg { Ok(vec![$1?]) }
//     ;
//
// Arg -> Result<Param, ()>:
//     'IDENT' 'COLON' Ty {
//         Ok(Param { span: $span, ident: Ident::new(0) } )
//     }
//     ;

ReturnTy -> Result<ReturnTy, ()>:
    'ARROW' Ty {
        { Ok(ReturnTy::Ty($2)) }
    }
    | {Ok(ReturnTy::None($span))}
    ;

Ty -> Result<Ty, ()>:
    'IDENT' { Ok(Ty) }
    ;


%%

use cfgrammar::Span;

use ecsl_ast::parse::*;
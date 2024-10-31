%start File
%%
File -> Result<ParsedFile, ()>:
    ItemList {
        Ok(ParsedFile { items: $1?})
    }
    ;

ItemList -> Result<Vec<Item>, ()>:
    Item { Ok(vec![$1?]) }
    | ItemList Item {
        flatten($1, $2)
    }
    ;

Item -> Result<Item, ()>:
    FnDef {
        Ok(Item::new(
            $span,
            ItemKind::Fn(P::new($1?)
        )))
    }
    ;

FnDef -> Result<FnDef, ()>:
    FnKind 'IDENT' 'LBRACKET' FnArgs 'RBRACKET' ReturnTy 'LCURLY' Block 'RCURLY' {
        Ok(FnDef {
            span: $span,
            kind: $1?,
            ident: Ident($2.map_err(|_| ())?.span()),
            params: $4?,
            ret: $6?,
            block: $8?,
        })
    }
    ;

FnKind -> Result<FnKind, ()>:
    'FN' { Ok(FnKind::Fn) }
    | 'SYS' { Ok(FnKind::Sys) }
    ;

FnArgs -> Result<Vec<Param>, ()>:
    ArgList TrailingComma { $1 }
    | { Ok(Vec::new()) }
    ;

ArgList -> Result<Vec<Param>, ()>:
    Arg { Ok(vec![$1?]) }
    | ArgList 'COMMA' Arg {
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
    'ARROW' Ty { Ok(RetTy::Ty(P::new($2?))) }
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

Block -> Result<Block, ()>:
    { Ok(Block { span: $span, stmts: Vec::new() }) }
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
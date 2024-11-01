%start File
%parse-param table: Rc<RefCell<PartialSymbolTable>>
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
            ident: table.new_ident($2.map_err(|_| ())?.span(), SymbolKind::Function),
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
            ident: table.new_ident($1.map_err(|_| ())?.span(), SymbolKind::Local),
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
        Ok(Ty::new($span, TyKind::Ident(
            table.new_ident($1.map_err(|_| ())?.span(), SymbolKind::Local)
        )))
    }
    | 'LSQUARE' Ty 'INT' 'RSQUARE' {
        Ok(Ty::new($span, TyKind::Array(
            P::new($2?), $3.map_err(|_| ())?.span()
        )))
    }
    ;

Block -> Result<Block, ()>:
    StmtList { Ok(Block { span: $span, stmts: $1? }) }
    | { Ok(Block { span: $span, stmts: Vec::new() }) }
    ;

StmtList -> Result<Vec<Stmt>, ()>:
    Stmt { Ok(vec![$1?]) }
    | StmtList Stmt {
        flatten($1, $2)
    }
    ;

Stmt -> Result<Stmt, ()>:
    'LET' Mutability 'IDENT' 'COLON' Ty 'ASSIGN' Expr 'SEMI' {
        Ok(Stmt::new($span, StmtKind::Let(
            $2?,
            table.new_ident($3.map_err(|_| ())?.span(), SymbolKind::Local),
            P::new($5?),
            P::new($7?),
        )))
    }
    | 'SEMI' { Ok(Stmt::new($span, StmtKind::Semi)) }
    ;

Mutability -> Result<Mutable, ()>:
    'MUT' { Ok(Mutable::Mut) }
    | 'IMM' { Ok(Mutable::Imm) }
    | { Ok(Mutable::Imm) }
    ;

Expr -> Result<Expr, ()>:
    Literal { Ok($1?) }
    | 'IDENT' { Ok(Expr::new(
        $span,
        ExprKind::Ident(
            table.new_ident($1.map_err(|_| ())?.span(), SymbolKind::Local)
        )
    ))}
    ;

Literal -> Result<Expr, ()>:
    'BOOLEAN' { Ok(Expr::new($span, ExprKind::Lit(Literal::Bool))) }
    | 'CHAR' { Ok(Expr::new($span, ExprKind::Lit(Literal::Char))) }
    | 'INT' { Ok(Expr::new($span, ExprKind::Lit(Literal::Int))) }
    | 'FLOAT' { Ok(Expr::new($span, ExprKind::Lit(Literal::Float))) }
    | 'STRING' { Ok(Expr::new($span, ExprKind::Lit(Literal::String))) }
    ;

%%

use ecsl_ast::parse::*;
use crate::*;

fn flatten<T>(lhs: Result<Vec<T>, ()>, rhs: Result<T, ()>)
           -> Result<Vec<T>, ()>
{
    let mut flt = lhs?;
    flt.push(rhs?);
    Ok(flt)
}
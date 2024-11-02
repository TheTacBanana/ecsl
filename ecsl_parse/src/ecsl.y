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
    FnKind 'IDENT' 'LBRACKET' FnArgs 'RBRACKET' ReturnTy 'LCURLY' Block 'RCURLY' {
        Ok(Item::new($span, ItemKind::Fn(P::new(FnDef {
            span: $span,
            kind: $1?,
            ident: table.new_ident($2.map_err(|_| ())?.span(), SymbolKind::Function),
            params: $4?,
            ret: $6?,
            block: $8?,
        }))))
    }
    | 'STRUCT' Component 'IDENT' Generics 'LCURLY'  'RCURLY' {
        Ok(Item::new($span, ItemKind::Struct(P::new(StructDef {
            span: $span,
            kind: $2?,
            ident: table.new_ident($3.map_err(|_| ())?.span(), SymbolKind::Struct($2?)),
            generics: $4?,
            fields: Vec::new(),
        }))))
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

ReturnTy -> Result<RetTy, ()>:
    'ARROW' Ty { Ok(RetTy::Ty(P::new($2?))) }
    | {Ok(RetTy::None($span))}
    ;

TrailingComma -> ():
    'COMMA' { () }
    | { () }
    ;

Component -> Result<DataKind, ()>:
    'COMP' { Ok(DataKind::Component) }
    | { Ok(DataKind::Normal) }
    ;

Generics -> Result<Option<P<Generics>>, ()>:
    'LT' GenericList TrailingComma 'GT' {
        Ok(Some(P::new(Generics {
            span: $span,
            params: $2?,
        })))
    }
    'LT' 'GT' { Ok(None) }
    | { Ok(None) }
    ;

GenericList -> Result<Vec<GenericParam>, ()>:
    'IDENT' { Ok(vec![
        GenericParam {
            span: $span,
            ident: table.new_ident($span, SymbolKind::Generic)
        }])
    }
    | GenericList 'COMMA' 'IDENT' {
        flatten(
            $1,
            Ok(GenericParam {
                span: $span,
                ident: table.new_ident($3.map_err(|_| ())?.span(), SymbolKind::Generic)
            })
        )
    }
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
    | 'FOR' 'LBRACKET' 'IDENT' 'COLON' Ty 'IN' Expr 'RBRACKET' 'LCURLY' Block 'RCURLY' {
        Ok(Stmt::new($span, StmtKind::For(
            table.new_ident($3.map_err(|_| ())?.span(), SymbolKind::Local),
            P::new($5?),
            P::new($7?),
            P::new($10?),
        )))
    }
    | 'WHILE' 'LBRACKET' Expr 'RBRACKET' 'LCURLY' Block 'RCURLY' {
        Ok(Stmt::new($span, StmtKind::While(
            P::new($3?),
            P::new($6?),
        )))
    }
    | IfStmt { Ok($1?)}
    | 'MATCH' 'LBRACKET' Expr 'RBRACKET' 'LCURLY' MatchArmList TrailingComma 'RCURLY' {
        Ok(Stmt::new($span, StmtKind::Match(
            P::new($3?),
            $6?
        )))
    }
    | Expr 'SEMI' {
        Ok(Stmt::new($span, StmtKind::Expr(
            P::new($1?)
        )))
    }
    | 'SEMI' { Ok(Stmt::new($span, StmtKind::Semi)) }
    ;

IfStmt -> Result<Stmt, ()>:
    'IF' 'LBRACKET' Expr 'RBRACKET' 'LCURLY' Block 'RCURLY' 'ELSE' IfStmt {
        Ok(Stmt::new($span, StmtKind::If(
            P::new($3?),
            P::new($6?),
            Some(P::new($9?)),
        )))
    }
    | 'IF' 'LBRACKET' Expr 'RBRACKET' 'LCURLY' Block 'RCURLY' {
        Ok(Stmt::new($span, StmtKind::If(
            P::new($3?),
            P::new($6?),
            None,
        )))
    }
    ;

MatchArmList -> Result<Vec<MatchArm>, ()>:
    MatchArm { Ok(vec![$1?]) }
    | MatchArmList 'COMMA' MatchArm {
        flatten($1, $3)
    }
    ;

MatchArm -> Result<MatchArm, ()>:
    'IDENT' 'LCURLY' FieldList 'RCURLY' 'ARROW' 'LCURLY' Block 'RCURLY' {
        Ok(MatchArm {
            span: $span,
            fields: $3?,
            block: P::new($7?)
        })
    }
    | 'IDENT' 'ARROW' 'LCURLY' Block 'RCURLY' {
        Ok(MatchArm {
            span: $span,
            fields: Vec::new(),
            block: P::new($4?)
        })
    }
    ;

FieldList -> Result<Vec<Field>, ()>:
    Field { Ok(vec![$1?]) }
    | FieldList 'COMMA' Field {
        flatten($1, $3)
    }
    ;

Field -> Result<Field, ()>:
    'IDENT' {
        Ok(Field {
            span: $span,
            ident: table.new_ident($1.map_err(|_| ())?.span(), SymbolKind::Local),
        })
    }
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

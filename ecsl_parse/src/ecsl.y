%start File
%parse-param table: Rc<RefCell<PartialSymbolTable>>

%left 'ASSIGN'
%left 'OR'
%left 'AND'
%left 'EQEQ' 'NOTEQ'
%left 'LT' 'LEQ' 'GT' 'GEQ'
%left 'PLUS' 'MINUS'
%left 'STAR' 'FSLASH'
%left 'NOT' 'AMPERSAND'
%left 'AS'
%left 'DOT'
%left 'RETURN'
%left 'SCHEDULE'

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
    'USE' UsePath 'SEMI' {
        Ok(Item::new($span, ItemKind::Use(P::new(UseDef {
            span: $span,
            path: P::new($2?),
        }))))
    }
    | FnDef { Ok(Item::new($span, ItemKind::Fn(P::new($1?)))) }
    | 'STRUCT' Component 'IDENT' Generics FieldDefs {
        Ok(Item::new($span, ItemKind::Struct(P::new(StructDef {
            span: $span,
            kind: $2?,
            ident: table.new_ident($3.map_err(|_| ())?.span(), SymbolKind::Struct($2?)),
            generics: $4?,
            fields: $5?,
        }))))
    }
    | 'ENUM' Component 'IDENT' Generics VariantDefs {
        Ok(Item::new($span, ItemKind::Enum(P::new(EnumDef {
            span: $span,
            kind: $2?,
            ident: table.new_ident($3.map_err(|_| ())?.span(), SymbolKind::Struct($2?)),
            generics: $4?,
            variants: $5?,
        }))))
    }
    | 'IMPL' Generics Ty ImplBlockContents {
        Ok(Item::new($span, ItemKind::Impl(P::new(ImplBlock {
            span: $span,
            generics: $2?,
            ty: P::new($3?),
            fn_defs: $4?
        }))))
    }
    ;

ImplBlockContents -> Result<Vec<FnDef>, ()>:
    'LCURLY' FnDefList 'RCURLY' { $2 }
    | 'LCURLY' 'RCURLY' { Ok(Vec::new()) }
    ;

FnDefList -> Result<Vec<FnDef>, ()>:
    FnDef { Ok(vec![$1?]) }
    | FnDefList FnDef {
        flatten($1, $2)
    }
    ;

FnDef -> Result<FnDef, ()>:
    FnKind 'IDENT' ConcreteGenerics 'LBRACKET' FnArgs 'RBRACKET' ReturnTy Block {
        Ok(FnDef {
            span: $span,
            kind: $1?,
            ident: table.new_ident($2.map_err(|_| ())?.span(), SymbolKind::Function),
            generics: $3?,
            params: $5?,
            ret: $7?,
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

ConcreteGenerics -> Result<Option<ConcreteGenerics>, ()>:
    'LT' TyList TrailingComma 'GT' {
        Ok(Some(ConcreteGenerics {
            span: $span,
            params: $2?,
        }))
    }
    | 'LT' 'GT' { Ok(None) }
    | { Ok(None) }
    ;

Generics -> Result<Option<P<Generics>>, ()>:
    'LT' GenericList TrailingComma 'GT' {
        Ok(Some(P::new(Generics {
            span: $span,
            params: $2?,
        })))
    }
    | 'LT' 'GT' { Ok(None) }
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

VariantDefs -> Result<Vec<VariantDef>, ()>:
    'LCURLY' VariantDefList TrailingComma 'RCURLY' { $2 }
    | 'LCURLY' 'RCURLY' { Ok(Vec::new()) }
    ;

VariantDefList -> Result<Vec<VariantDef>, ()>:
    VariantDef { Ok(vec![$1?]) }
    | VariantDefList 'COMMA' VariantDef {
        flatten($1, $3)
    }
    ;

VariantDef -> Result<VariantDef, ()>:
    'IDENT' VariantFieldDefs {
        Ok(VariantDef{
            span: $span,
            ident: table.new_ident($1.map_err(|_| ())?.span(), SymbolKind::VariantDef),
            fields: $2?,
        })
    }
    ;

VariantFieldDefs -> Result<Vec<FieldDef>, ()>:
    'LCURLY' FieldDefList TrailingComma 'RCURLY' { $2 }
    | 'LCURLY' 'RCURLY' { Ok(Vec::new()) }
    | { Ok(Vec::new()) }
    ;

FieldDefs -> Result<Vec<FieldDef>, ()>:
    'LCURLY' FieldDefList TrailingComma 'RCURLY' { $2 }
    | 'LCURLY' 'RCURLY' { Ok(Vec::new()) }
    | 'SEMI' { Ok(Vec::new()) }
    ;

FieldDefList -> Result<Vec<FieldDef>, ()>:
    FieldDef { Ok(vec![$1?]) }
    | FieldDefList 'COMMA' FieldDef {
        flatten($1, $3)
    }
    ;

FieldDef -> Result<FieldDef, ()>:
    'IDENT' 'COLON' Ty {
        Ok(FieldDef{
            span: $span,
            ident: table.new_ident($1.map_err(|_| ())?.span(), SymbolKind::FieldDef),
            ty: P::new($3?),
        })
    }
    ;

UsePathList -> Result<Vec<UsePath>, ()>:
    UsePath { Ok(vec![$1?]) }
    | UsePathList 'COMMA' UsePath {
        flatten($1, $3)
    }
    ;

UsePath -> Result<UsePath, ()>:
    'LCURLY' UsePathList TrailingComma 'RCURLY' {
        Ok(UsePath::Multiple(
            $span,
            $2?,
        ))
    }
    | 'IDENT' 'PATH' UsePath {
        Ok(UsePath::Single(
            $1.map_err(|_| ())?.span(),
            table.new_ident($1.map_err(|_| ())?.span(), SymbolKind::ImportPathSegment),
            P::new($3?),
        ))
    }
    | 'IDENT' {
        Ok(UsePath::Item(
            $span,
            table.new_ident($1.map_err(|_| ())?.span(), SymbolKind::ImportItem),
        ))
    }
    | 'SUPER' 'PATH' UsePath {
        Ok(UsePath::Super(
            $1.map_err(|_| ())?.span(),
            P::new($3?),
        ))
    }
    ;

TyList -> Result<Vec<Ty>, ()>:
    Ty { Ok(vec![$1?]) }
    | TyList 'COMMA' Ty {
        flatten($1, $3)
    }
    ;

Ty -> Result<Ty, ()>:
    'IDENT' ConcreteGenerics {
        Ok(Ty::new($span, TyKind::Ident(
            table.new_ident($1.map_err(|_| ())?.span(), SymbolKind::Local),
            $2?
        )))
    }
    | 'LSQUARE' Ty 'COLON' 'INT' 'RSQUARE' {
        Ok(Ty::new($span, TyKind::Array(
            P::new($2?), $4.map_err(|_| ())?.span()
        )))
    }
    | 'AMPERSAND' RefMutability 'LSQUARE' Ty 'RSQUARE' {
        Ok(Ty::new($span, TyKind::ArrayRef(
            $2?, P::new($4?)
        )))
    }
    | 'AMPERSAND' RefMutability Ty {
        Ok(Ty::new($span, TyKind::Ref(
            $2?, P::new($3?)
        )))
    }
    | 'STAR' PtrMutability Ty {
        Ok(Ty::new($span, TyKind::Ptr(
            $2?, P::new($3?)
        )))
    }
    | EntityTy {
        Ok(Ty::new($span, TyKind::Entity($1?)))
    }
    | 'QUERY' {
        Ok(Ty::new($span, TyKind::Query))
    }
    | 'SCHEDULE' {
        Ok(Ty::new($span, TyKind::Schedule))
    }
    | 'SYSTEM' {
        Ok(Ty::new($span, TyKind::System))
    }
    ;

EntityTy -> Result<EntityTy, ()>:
    'ENTITY' 'LT' EntityAttributeList TrailingComma 'GT' {
        Ok(EntityTy {
            bounds: $3?,
        })
    }
    |'ENTITY' {
        Ok(EntityTy {
            bounds: Vec::new(),
        })
    }
    ;

EntityAttributeList -> Result<Vec<EntityAttribute>, ()>:
    EntityAttribute { Ok(vec![$1?]) }
    | EntityAttributeList 'COMMA' EntityAttribute {
        flatten($1, $3)
    }
    ;

EntityAttribute -> Result<EntityAttribute, ()>:
    'IDENT' 'COLON' Ty {
        Ok(EntityAttribute {
            span: $span,
            ident: table.new_ident($1.map_err(|_| ())?.span(), SymbolKind::Local),
            ty: P::new($3?),
        })
    }
    ;

Block -> Result<Block, ()>:
    'LCURLY' StmtList 'RCURLY' { Ok(Block { span: $span, stmts: $2? }) }
    | 'LCURLY' 'RCURLY'{ Ok(Block { span: $span, stmts: Vec::new() }) }
    ;

StmtList -> Result<Vec<Stmt>, ()>:
    Stmt { Ok(vec![$1?]) }
    | StmtList Stmt {
        flatten($1, $2)
    }
    ;

Stmt -> Result<Stmt, ()>:
    'LET' RefMutability 'IDENT' 'COLON' Ty 'ASSIGN' Expr 'SEMI' {
        Ok(Stmt::new($span, StmtKind::Let(
            $2?,
            table.new_ident($3.map_err(|_| ())?.span(), SymbolKind::Local),
            P::new($5?),
            P::new($7?),
        )))
    }
    | 'FOR' 'LBRACKET' 'IDENT' 'COLON' Ty 'IN' Expr 'RBRACKET' Block {
        Ok(Stmt::new($span, StmtKind::For(
            table.new_ident($3.map_err(|_| ())?.span(), SymbolKind::Local),
            P::new($5?),
            P::new($7?),
            P::new($9?),
        )))
    }
    | 'WHILE' 'LBRACKET' Expr 'RBRACKET' Block {
        Ok(Stmt::new($span, StmtKind::While(
            P::new($3?),
            P::new($5?),
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
    'IF' 'LBRACKET' Expr 'RBRACKET' Block 'ELSE' IfStmt {
        Ok(Stmt::new($span, StmtKind::If(
            P::new($3?),
            P::new($5?),
            Some(P::new($7?)),
        )))
    }
    | 'IF' 'LBRACKET' Expr 'RBRACKET' Block {
        Ok(Stmt::new($span, StmtKind::If(
            P::new($3?),
            P::new($5?),
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
    'IDENT' 'LCURLY' FieldList 'RCURLY' 'ARROW' Block {
        Ok(MatchArm {
            span: $span,
            fields: $3?,
            block: P::new($6?)
        })
    }
    | 'IDENT' 'ARROW' Block {
        Ok(MatchArm {
            span: $span,
            fields: Vec::new(),
            block: P::new($3?)
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

RefMutability -> Result<Mutable, ()>:
    'MUT' { Ok(Mutable::Mut) }
    | { Ok(Mutable::Imm) }
    ;

PtrMutability -> Result<Mutable, ()>:
    'MUT' { Ok(Mutable::Mut) }
    | 'IMM' { Ok(Mutable::Imm) }
    ;

ExprList -> Result<Vec<Expr>, ()>:
    Expr { Ok(vec![$1?]) }
    | ExprList 'COMMA' Expr {
        flatten($1, $3)
    }
    ;

Expr -> Result<Expr, ()>:
    'IDENT' 'ASSIGN' Expr {
        Ok(Expr::new(
            $span,
            ExprKind::Assign(
                table.new_ident($1.map_err(|_| ())?.span(), SymbolKind::Local),
                P::new($3?),
            )
        ))
    }
    | 'LBRACKET' Expr 'RBRACKET' { $2 }
    | 'LSQUARE' ExprList TrailingComma 'RSQUARE' {
        Ok(Expr::new($span, ExprKind::Array(
            $2?
        )))
    }
    | Expr 'OR' Expr {
        Ok(Expr::new($span, ExprKind::BinOp(
            BinOpKind::Or,
            P::new($1?),
            P::new($3?),
        )))
    }
    | Expr 'AND' Expr {
        Ok(Expr::new($span, ExprKind::BinOp(
            BinOpKind::And,
            P::new($1?),
            P::new($3?),
        )))
    }
    | Expr 'EQEQ' Expr {
        Ok(Expr::new($span, ExprKind::BinOp(
            BinOpKind::Eq,
            P::new($1?),
            P::new($3?),
        )))
    }
    | Expr 'NOTEQ' Expr {
        Ok(Expr::new($span, ExprKind::BinOp(
            BinOpKind::Eq,
            P::new($1?),
            P::new($3?),
        )))
    }
    | Expr 'LT' Expr {
        Ok(Expr::new($span, ExprKind::BinOp(
            BinOpKind::Lt,
            P::new($1?),
            P::new($3?),
        )))
    }
    | Expr 'LEQ' Expr {
        Ok(Expr::new($span, ExprKind::BinOp(
            BinOpKind::Leq,
            P::new($1?),
            P::new($3?),
        )))
    }
    | Expr 'GT' Expr {
        Ok(Expr::new($span, ExprKind::BinOp(
            BinOpKind::Gt,
            P::new($1?),
            P::new($3?),
        )))
    }
    | Expr 'GEQ' Expr {
        Ok(Expr::new($span, ExprKind::BinOp(
            BinOpKind::Geq,
            P::new($1?),
            P::new($3?),
        )))
    }
    | Expr 'PLUS' Expr {
        Ok(Expr::new($span, ExprKind::BinOp(
            BinOpKind::Add,
            P::new($1?),
            P::new($3?),
        )))
    }
    | Expr 'MINUS' Expr {
        Ok(Expr::new($span, ExprKind::BinOp(
            BinOpKind::Sub,
            P::new($1?),
            P::new($3?),
        )))
    }
    | Expr 'STAR' Expr {
        Ok(Expr::new($span, ExprKind::BinOp(
            BinOpKind::Mul,
            P::new($1?),
            P::new($3?),
        )))
    }
    | Expr 'FSLASH' Expr {
        Ok(Expr::new($span, ExprKind::BinOp(
            BinOpKind::Div,
            P::new($1?),
            P::new($3?),
        )))
    }
    | 'NOT' Expr {
        Ok(Expr::new($span, ExprKind::UnOp(
            UnOpKind::Not,
            P::new($2?),
        )))
    }
    | 'MINUS' Expr {
        Ok(Expr::new($span, ExprKind::UnOp(
            UnOpKind::Neg,
            P::new($2?),
        )))
    }
    | 'STAR' Expr {
        Ok(Expr::new($span, ExprKind::UnOp(
            UnOpKind::Deref,
            P::new($2?),
        )))
    }
    | 'AMPERSAND' RefMutability Expr {
        Ok(Expr::new($span, ExprKind::Ref(
            $2?,
            P::new($3?),
        )))
    }
    | 'IDENT' FnArgExpr {
        Ok(Expr::new($span, ExprKind::Function(
            None,
            table.new_ident($1.map_err(|_| ())?.span(), SymbolKind::Function),
            $2?,
        )))
    }
    | Expr 'DOT' 'IDENT' FnArgExpr {
        Ok(Expr::new($span, ExprKind::Function(
            Some(P::new($1?)),
            table.new_ident($3.map_err(|_| ())?.span(), SymbolKind::Function),
            $4?,
        )))
    }
    | Expr 'DOT' 'IDENT' {
        Ok(Expr::new($span, ExprKind::Field(
            P::new($1?),
            table.new_ident($3.map_err(|_| ())?.span(), SymbolKind::Local)
        )))
    }
    //| Expr 'AS' Ty {
    //    Ok(Expr::new($span, ExprKind::Cast(
    //        P::new($1?),
    //        P::new($3?),
    //    )))
    //}

    | 'BREAK' {
        Ok(Expr::new($span, ExprKind::Break))
    }
    | 'CONTINUE' {
        Ok(Expr::new($span, ExprKind::Continue))
    }
    | 'RETURN' Expr {
        Ok(Expr::new($span, ExprKind::Return(
            Some(P::new($2?))
        )))
    }
    | 'RETURN' {
        Ok(Expr::new($span, ExprKind::Return(None)))
    }
    | 'ENTITY' {
        Ok(Expr::new($span, ExprKind::Entity))
    }
    | 'RESOURCE' {
        Ok(Expr::new($span, ExprKind::Entity))
    }
    | 'SCHEDULE' ScheduleExpr {
        Ok(Expr::new($span, ExprKind::Schedule(P::new($2?))))
    }

    | 'IDENT' {
        Ok(Expr::new(
            $span,
            ExprKind::Ident(
                table.new_ident($1.map_err(|_| ())?.span(), SymbolKind::Local)
            )
        ))
    }
    | Literal { Ok($1?) }
    ;

PrecedingSchedule -> ():
    'SCHEDULE' { () }
    | { () } // Nothing
    ;

ScheduleExpr -> Result<Schedule, ()>:
    PrecedingSchedule 'LSQUARE' ScheduleList TrailingComma 'RSQUARE' {
        Ok(Schedule::new($span,
            ScheduleKind::Ordered($3?)
        ))
    }
    | PrecedingSchedule 'LCURLY' ScheduleList TrailingComma 'RCURLY' {
        Ok(Schedule::new($span,
            ScheduleKind::Unordered($3?)
        ))
    }
    | 'IDENT' {
        Ok(Schedule::new($span,
            ScheduleKind::Expr(P::new(
                Expr::new($span, ExprKind::Ident(
                    table.new_ident($1.map_err(|_| ())?.span(), SymbolKind::System),
                ))
            ))
        ))
    }
    ;

ScheduleList -> Result<Vec<Schedule>, ()>:
    ScheduleExpr { Ok(vec![$1?]) }
    | ScheduleList 'COMMA' ScheduleExpr {
        flatten($1, $3)
    }
    ;

FnArgExpr -> Result<Vec<Expr>, ()>:
    'LBRACKET' ExprList TrailingComma 'RBRACKET' { $2 }
    | 'LBRACKET' 'RBRACKET' { Ok(Vec::new()) }
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

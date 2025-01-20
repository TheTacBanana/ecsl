%start File
%avoid_insert 'INT'
%avoid_insert 'FLOAT'
%avoid_insert 'BOOLEAN'
%avoid_insert 'STRING'
%avoid_insert 'CHAR'
%avoid_insert 'ENTITY'
%avoid_insert 'QUERY'
%avoid_insert 'RESOURCE'
%avoid_insert 'SELF'

%epp 'USE' 'use'
%epp 'PATH' '::'

%epp 'STRUCT' 'struct'
%epp 'ENUM' 'enum'
%epp 'COMP' 'comp'
%epp 'IMPL' 'impl'
%epp 'HASH' '#'

%epp 'FN' 'fn'
%epp 'SYS' 'sys'

%epp 'PLUS' '+'
%epp 'MINUS' '-'
%epp 'STAR' '*'
%epp 'FSLASH' '/'

%epp 'LEQ'  '<='
%epp 'LT' '<'
%epp 'GEQ' '>='
%epp 'GT' '>'
%epp 'EQEQ' '=='
%epp 'NOTEQ' '!='

%epp 'OR' '||'
%epp 'AND' '&&'
%epp 'NOT' '!'

%epp 'LET' 'let'
%epp 'MUT' 'mut'
%epp 'IMM' 'imm'
%epp 'ASSIGN' '='
%epp 'AS' 'as'

%epp 'LBRACKET' '('
%epp 'RBRACKET' ')'
%epp 'LCURLY' '{'
%epp 'RCURLY' '}'
%epp 'LSQUARE' '['
%epp 'RSQUARE' ']'

%epp 'SEMI' ';'
%epp 'COLON' ':'
%epp 'COMMA' ','
%epp 'DOTDOTEQ' '..='
%epp 'DOTDOT' '..'
%epp 'DOT' '.'

%epp 'AMPERSAND' '&'
%epp 'ARROW' '->'

%epp 'BOOLEAN' 'Boolean'
%epp 'IDENT' 'Ident'
%epp 'FLOAT' 'Float'
%epp 'INT' 'Int'
%epp 'STRING' 'String'
%epp 'CHAR' 'Char'

%epp 'ENTITY' 'Entity'
%epp 'QUERY' 'Query'
%epp 'RESOURCE' 'Resource'
%epp 'SCHEDULE' 'Schedule'
%epp 'SYSTEM' 'System'
%epp 'WITH' 'with'
%epp 'WITHOUT' 'without'
%epp 'ADDED' 'added'
%epp 'REMOVED' 'removed'

%epp 'IF' 'if'
%epp 'ELSE' 'else'
%epp 'FOR' 'for'
%epp 'IN' 'in'
%epp 'WHILE' 'while'
%epp 'MATCH' 'match'
%epp 'BREAK' 'break'
%epp 'CONTINUE' 'continue'
%epp 'RETURN' 'return'

%parse-param table: Rc<RefCell<PartialSymbolTable>>

%right 'ASSIGN'
%left 'DOTDOT' 'DOTDOTEQ'
%left 'OR'
%left 'AND'
%left 'EQEQ' 'NOTEQ'
%left 'LT' 'LEQ' 'GT' 'GEQ'
%left 'PLUS' 'MINUS'
%left 'STAR' 'FSLASH'
%right 'NOT' 'AMPERSAND'
%left 'DOT'
%right 'AS'

%%
File -> Result<SourceAST, ()>:
    ItemList {
        Ok(SourceAST {
            file: table.file_id(),
            items: $1?
        })
    }
    | {
        Ok(SourceAST {
            file: table.file_id(),
            items: Vec::new()
        })
    }
    ;

ItemList -> Result<Vec<Item>, ()>:
    Item { Ok(vec![$1?]) }
    | ItemList Item {
        flatten($1, $2)
    }
    ;

Item -> Result<Item, ()>:
    Attributes 'USE' UsePath 'SEMI' {
        Ok(Item::new($span, ItemKind::Use(P::new(UseDef {
            span: $span,
            attributes: $1?,
            path: P::new($3?),
        }))))
    }
    | FnDef { Ok(Item::new($span, ItemKind::Fn(P::new($1?)))) }
    | Attributes 'STRUCT' Component 'IDENT' Generics FieldDefs {
        Ok(Item::new($span, ItemKind::Struct(P::new(StructDef {
            span: $4.map_err(|_| ())?.span(),
            kind: $3?,
            ident: table.definition($4.map_err(|_| ())?.span(), SymbolKind::Struct($3?)),
            attributes: $1?,
            generics: $5?,
            fields: $6?,
        }))))
    }
    | Attributes 'ENUM' Component 'IDENT' Generics VariantDefs {
        Ok(Item::new($span, ItemKind::Enum(P::new(EnumDef {
            span: $4.map_err(|_| ())?.span(),
            kind: $3?,
            ident: table.definition($4.map_err(|_| ())?.span(), SymbolKind::Enum($3?)),
            attributes: $1?,
            generics: $5?,
            variants: $6?,
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
    FnKind 'IDENT' Generics 'LBRACKET' FnArgs 'RBRACKET' ReturnTy Block {
        Ok(FnDef {
            span: $2.map_err(|_| ())?.span(),
            kind: $1?,
            ident: table.definition($2.map_err(|_| ())?.span(), SymbolKind::Function($1?)),
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
    RefMutability 'SELF' {
        Ok(Param::new($span,ParamKind::SelfValue($1?)))
    }
    | 'AMPERSAND' RefMutability 'SELF' {
        Ok(Param::new($span,ParamKind::SelfReference($2?)))
    }
    | RefMutability 'IDENT' 'COLON' Ty {
        Ok(Param::new($span, ParamKind::Normal(
            $1?,
            table.definition($2.map_err(|_| ())?.span(), SymbolKind::FunctionArg),
            P::new($4?),
        )))
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

Attributes -> Result<Attributes, ()>:
    'HASH' 'LSQUARE' AttributeList TrailingComma 'RSQUARE' {
        Ok(Attributes::from_vec($3?))
     }
    | 'HASH' 'LSQUARE' 'RSQUARE' { Ok(Attributes::new())}
    | { Ok(Attributes::new()) }
    ;

AttributeList -> Result<Vec<Attribute>, ()>:
    Attribute { Ok(vec![$1?]) }
    | AttributeList 'COMMA' Attribute {
        flatten($1, $3)
    }
    ;

Attribute -> Result<Attribute, ()>:
    'IDENT' {
        Ok(Attribute::new(
            $span,
            AttributeKind::Marker(
                AttributeMarker::from_string(&table.string($span))
            )
        ))
    }
    | 'IDENT' 'LBRACKET' INT 'RBRACKET'{
        Ok(Attribute::new(
            $span,
            AttributeKind::Value(
                AttributeValue::from_string(
                    &table.string($1.map_err(|_| ())?.span())
                ),
                table.string($3.map_err(|_| ())?.span()).parse().map_err(|_| ())?
            )
        ))
    }
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

Generics -> Result<Option<Generics>, ()>:
    'LT' GenericList TrailingComma 'GT' {
        Ok(Some(Generics {
            span: $span,
            params: $2?,
        }))
    }
    | 'LT' 'GT' { Ok(None) }
    | { Ok(None) }
    ;

GenericList -> Result<Vec<GenericParam>, ()>:
    'IDENT' { Ok(vec![
        GenericParam {
            span: $span,
            ident: table.definition($span, SymbolKind::Generic)
        }])
    }
    | GenericList 'COMMA' 'IDENT' {
        flatten(
            $1,
            Ok(GenericParam {
                span: $span,
                ident: table.definition($3.map_err(|_| ())?.span(), SymbolKind::Generic)
            })
        )
    }
    ;

VariantDefs -> Result<Vec<VariantDef>, ()>:
    'LCURLY' VariantDefList TrailingComma 'RCURLY' { $2 }
    | 'LCURLY' 'RCURLY' { Ok(Vec::new()) }
    | 'SEMI' { Ok(Vec::new()) }
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
            ident: table.definition($1.map_err(|_| ())?.span(), SymbolKind::Variant),
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
            ident: table.definition($1.map_err(|_| ())?.span(), SymbolKind::Field),
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
    'LCURLY' 'RCURLY' {
        Ok(UsePath::Multiple(
            $span,
            Vec::new(),
        ))
    }
    | 'LCURLY' UsePathList TrailingComma 'RCURLY' {
        Ok(UsePath::Multiple(
            $span,
            $2?,
        ))
    }
    | 'IDENT' 'PATH' UsePath {
        Ok(UsePath::Single(
            $1.map_err(|_| ())?.span(),
            table.definition($1.map_err(|_| ())?.span(), SymbolKind::ImportPath),
            P::new($3?),
        ))
    }
    | 'IDENT' {
        Ok(UsePath::Item(
            $span,
            table.definition($1.map_err(|_| ())?.span(), SymbolKind::ImportItem),
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
            table.usage($1.map_err(|_| ())?.span(), SymbolKind::Ty),
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
    | 'ENTITY' 'LT' 'GT' {
        Ok(EntityTy {
            bounds: Vec::new(),
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
            ident: table.definition($1.map_err(|_| ())?.span(), SymbolKind::EntityAttribute),
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
            table.definition($3.map_err(|_| ())?.span(), SymbolKind::Local),
            $3.map_err(|_| ())?.span(),
            P::new($5?),
            P::new($7?),
        )))
    }
    | 'FOR' 'LBRACKET' 'IDENT' 'COLON' Ty 'IN' Expr 'RBRACKET' Block {
        Ok(Stmt::new($span, StmtKind::For(
            table.definition($3.map_err(|_| ())?.span(), SymbolKind::Local),
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
    | 'BREAK' 'SEMI'{
        Ok(Stmt::new($span, StmtKind::Break))
    }
    | 'CONTINUE' 'SEMI'{
        Ok(Stmt::new($span, StmtKind::Continue))
    }
    | 'RETURN' Expr 'SEMI'{
        Ok(Stmt::new($span, StmtKind::Return(
            Some(P::new($2?))
        )))
    }
    | 'RETURN' 'SEMI'{
        Ok(Stmt::new($span, StmtKind::Return(None)))
    }
    | Expr 'SEMI' {
        Ok(Stmt::new($span, StmtKind::Expr(
            P::new($1?)
        )))
    }
    | 'SEMI' { Ok(Stmt::new($span, StmtKind::Semi)) }
    ;

IfStmt -> Result<Stmt, ()>:
    'IF' 'LBRACKET' Expr 'RBRACKET' Block ElseStmt {
        Ok(Stmt::new($span, StmtKind::If(
            P::new($3?),
            P::new($5?),
            $6?,
        )))
    }
    ;

ElseStmt -> Result<Option<P<Stmt>>, ()>:
    'ELSE' 'IF' 'LBRACKET' Expr 'RBRACKET' Block ElseStmt {
        Ok(Some(P::new(Stmt::new($span, StmtKind::ElseIf(
            P::new($4?),
            P::new($6?),
            $7?,
        )))))
    }
    | 'ELSE' Block {
        Ok(Some(P::new(Stmt::new($span, StmtKind::Else(
            P::new($2?),
        )))))
    }
    | { Ok(None) }
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
            ident: table.usage($1.map_err(|_| ())?.span(), SymbolKind::VariantUsage),
            fields: $3?,
            block: P::new($6?)
        })
    }
    | 'IDENT' 'ARROW' Block {
        Ok(MatchArm {
            span: $span,
            ident: table.usage($1.map_err(|_| ())?.span(), SymbolKind::VariantUsage),
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
            ident: table.usage($1.map_err(|_| ())?.span(), SymbolKind::Field),
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
                table.usage($1.map_err(|_| ())?.span(), SymbolKind::Local),
                $1.map_err(|_| ())?.span(),
                P::new($3?),
            )
        ))
    }
    | 'LBRACKET' Expr 'RBRACKET' { $2 }
    | 'LSQUARE''RSQUARE' {
        Ok(Expr::new($span, ExprKind::Array(
            Vec::new()
        )))
    }
    | 'LSQUARE' ExprList TrailingComma 'RSQUARE' {
        Ok(Expr::new($span, ExprKind::Array(
            $2?
        )))
    }
    | Expr 'DOTDOT' Expr {
        Ok(Expr::new($span, ExprKind::Range(
            P::new($1?),
            P::new($3?),
            RangeType::Exclusive,
        )))
    }
    | Expr 'DOTDOTEQ' Expr {
        Ok(Expr::new($span, ExprKind::Range(
            P::new($1?),
            P::new($3?),
            RangeType::Inclusive,
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

    | 'IDENT' 'PATH' ConcreteGenerics 'PATH' FnArgExpr {
        Ok(Expr::new($span, ExprKind::Function(
            None,
            None,
            table.usage($1.map_err(|_| ())?.span(), SymbolKind::FunctionUsage),
            $5?,
        )))
    }
    | 'IDENT' FnArgExpr {
        Ok(Expr::new($span, ExprKind::Function(
            None,
            None,
            table.usage($1.map_err(|_| ())?.span(), SymbolKind::FunctionUsage),
            $2?,
        )))
    }
    | Expr 'DOT' 'IDENT' 'PATH' ConcreteGenerics 'PATH' FnArgExpr {
        Ok(Expr::new($span, ExprKind::Function(
            Some(P::new($1?)),
            $5?,
            table.usage($3.map_err(|_| ())?.span(), SymbolKind::FunctionUsage),
            $7?,
        )))
    }
    | Expr 'DOT' 'IDENT' FnArgExpr {
        Ok(Expr::new($span, ExprKind::Function(
            Some(P::new($1?)),
            None,
            table.usage($3.map_err(|_| ())?.span(), SymbolKind::FunctionUsage),
            $4?,
        )))
    }
    | Expr 'DOT' 'IDENT' {
        Ok(Expr::new($span, ExprKind::Field(
            P::new($1?),
            table.usage($3.map_err(|_| ())?.span(), SymbolKind::FieldUsage)
        )))
    }
    | Expr 'AS' 'IDENT' { //TODO: Doesnt work for ptr casting
        Ok(Expr::new($span, ExprKind::Cast(
            P::new($1?),
            P::new(
                Ty::new(
                    $3.map_err(|_| ())?.span(),
                    TyKind::Ident(
                        table.usage($3.map_err(|_| ())?.span(), SymbolKind::Ty),
                        None,
                    )
                )
            ),
        )))
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
    | 'QUERY' 'LBRACKET' 'RBRACKET' {
        Ok(Expr::new($span, ExprKind::Query(P::new(
            QueryExpr {
                filters: Vec::new(),
            }
        ))))
    }
    | 'QUERY' 'DOT' QueryFilterList 'LBRACKET' 'RBRACKET'{
        Ok(Expr::new($span, ExprKind::Query(P::new(
            QueryExpr {
                filters: $3?,
            }
        ))))
    }

    | 'IDENT' 'PATH' ConcreteGenerics 'PATH' 'IDENT' FieldAssignments {
        Ok(Expr::new($span, ExprKind::Enum(
            P::new(Ty::new($span, TyKind::Ident(
                table.usage($1.map_err(|_| ())?.span(), SymbolKind::Ty),
                $3?
            ))),
            table.usage($5.map_err(|_| ())?.span(), SymbolKind::VariantUsage),
            $6?,
        )))
    }
    | 'IDENT' 'PATH' 'IDENT' FieldAssignments {
        Ok(Expr::new($span, ExprKind::Enum(
            P::new(Ty::new($span, TyKind::Ident(
                table.usage($1.map_err(|_| ())?.span(), SymbolKind::Ty),
                None,
            ))),
            table.usage($3.map_err(|_| ())?.span(), SymbolKind::VariantUsage),
            $4?,
        )))
    }
    | 'IDENT' 'PATH' ConcreteGenerics FieldAssignments {
        Ok(Expr::new($span, ExprKind::Struct(
            P::new(Ty::new($span, TyKind::Ident(
                table.usage($1.map_err(|_| ())?.span(), SymbolKind::Ty),
                $3?
            ))),
            $4?,
        )))
    }
    | 'IDENT' FieldAssignments {
        Ok(Expr::new($span, ExprKind::Struct(
            P::new(Ty::new($span, TyKind::Ident(
                table.usage($1.map_err(|_| ())?.span(), SymbolKind::Ty),
                None,
            ))),
            $2?,
        )))
    }

    | 'IDENT' {
        Ok(Expr::new(
            $span,
            ExprKind::Ident(
                table.usage($1.map_err(|_| ())?.span(), SymbolKind::Local)
            )
        ))
    }
    | 'SELF' {
        Ok(Expr::new(
            $span,
            ExprKind::MethodSelf
        ))
    }
    | Literal { Ok($1?) }
    ;

FieldAssignments -> Result<Vec<FieldExpr>, ()>:
    'LCURLY' FieldExprList TrailingComma 'RCURLY' { $2 }
    | 'LCURLY' 'RCURLY' { Ok(Vec::new()) }
    //| { Ok(Vec::new()) }
    ;

FieldExprList -> Result<Vec<FieldExpr>, ()>:
    FieldExpr { Ok(vec![$1?]) }
    | FieldExprList 'COMMA' FieldExpr {
        flatten($1, $3)
    }
    ;

FieldExpr -> Result<FieldExpr, ()>:
    'IDENT' 'COLON' Expr {
        Ok(FieldExpr {
            span: $span,
            ident: table.usage($1.map_err(|_| ())?.span(), SymbolKind::FieldUsage),
            expr: P::new($3?),
        })
    }
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
    | PrecedingSchedule 'LSQUARE' 'RSQUARE' {
        Ok(Schedule::new($span,
            ScheduleKind::Ordered(Vec::new())
        ))
    }
    | PrecedingSchedule 'LCURLY' ScheduleList TrailingComma 'RCURLY' {
        Ok(Schedule::new($span,
            ScheduleKind::Unordered($3?)
        ))
    }
    | PrecedingSchedule 'LCURLY' 'RCURLY' {
        Ok(Schedule::new($span,
            ScheduleKind::Unordered(Vec::new())
        ))
    }
    | 'IDENT' {
        Ok(Schedule::new($span,
            ScheduleKind::Expr(P::new(
                Expr::new($span, ExprKind::Ident(
                    table.usage($1.map_err(|_| ())?.span(), SymbolKind::ScheduleItem),
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

QueryFilterList -> Result<Vec<QueryFilter>, ()>:
    QueryFilter { Ok(vec![$1?]) }
    | QueryFilterList 'DOT' QueryFilter {
        flatten($1, $3)
    }
    ;

QueryFilter -> Result<QueryFilter, ()>:
    'WITH' 'LT' TyList TrailingComma 'GT' {
        Ok(QueryFilter::new($span, FilterKind::With($3?)))
    }
    | 'WITH' 'LT' 'GT' {
        Ok(QueryFilter::new($span, FilterKind::With(Vec::new())))
    }
    | 'WITHOUT' 'LT' TyList  TrailingComma 'GT' {
        Ok(QueryFilter::new($span, FilterKind::Without($3?)))
    }
    | 'WITHOUT' 'LT' 'GT' {
        Ok(QueryFilter::new($span, FilterKind::Without(Vec::new())))
    }
    | 'ADDED' 'LT' TyList TrailingComma 'GT' {
        Ok(QueryFilter::new($span, FilterKind::Added($3?)))
    }
    | 'ADDED' 'LT' 'GT' {
        Ok(QueryFilter::new($span, FilterKind::Added(Vec::new())))
    }
    | 'REMOVED' 'LT' TyList TrailingComma 'GT'{
        Ok(QueryFilter::new($span, FilterKind::Removed($3?)))
    }
    | 'REMOVED' 'LT' 'GT' {
        Ok(QueryFilter::new($span, FilterKind::Added(Vec::new())))
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

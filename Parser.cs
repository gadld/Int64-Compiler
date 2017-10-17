/*
  Buttercup compiler - This class performs the syntactic analysis,
  (a.k.a. parsing).
  Copyright (C) 2013 Ariel Ortiz, ITESM CEM
  
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

using System;
using System.Collections.Generic;

namespace Int64 {

    class Parser {      

        static readonly ISet<TokenCategory> firstOfProgram =
            new HashSet<TokenCategory>() {
                TokenCategory.IDENTIFIER,
                TokenCategory.VAR
            };


        static readonly ISet<TokenCategory> firstOfStatement =
            new HashSet<TokenCategory>() {
                TokenCategory.IDENTIFIER,
                TokenCategory.IF,
                TokenCategory.SWITCH,
                TokenCategory.WHILE,
                TokenCategory.DO,
                TokenCategory.FOR,
                TokenCategory.BREAK,
                TokenCategory.CONTINUE,
                TokenCategory.RETURN,
                TokenCategory.SEMICOLON
            };

        static readonly ISet<TokenCategory> firstOfUnary =
            new HashSet<TokenCategory>() {
                TokenCategory.ADDITION,
                TokenCategory.SUBTRACTION,
                TokenCategory.BIT_NOT,
                TokenCategory.NOT
            };
        static readonly ISet<TokenCategory> firstOfLit =
            new HashSet<TokenCategory>() {
                TokenCategory.BOOL,
                TokenCategory.INT,
                TokenCategory.CHARACTER
            };
        static readonly ISet<TokenCategory> firstOfLitAlt =
            new HashSet<TokenCategory>() {
                TokenCategory.BOOL,
                TokenCategory.INT,
                TokenCategory.CHARACTER,
                TokenCategory.STRING,
                TokenCategory.CURLY_BRACE_LEFT
            };
        static readonly ISet<TokenCategory> firstOfExprPrimary =
            new HashSet<TokenCategory>() {
                TokenCategory.IDENTIFIER,
                TokenCategory.BOOL,
                TokenCategory.INT,
                TokenCategory.CHARACTER,
                TokenCategory.STRING,
                TokenCategory.CURLY_BRACE_LEFT,
                TokenCategory.PARENTHESIS_LEFT
            };
                
        IEnumerator<Token> tokenStream;

        public Parser(IEnumerator<Token> tokenStream) {
            this.tokenStream = tokenStream;
            this.tokenStream.MoveNext();
        }

        public TokenCategory CurrentToken {
            get { return tokenStream.Current.Category; }
        }

        public Token Expect(TokenCategory category) {
            if (CurrentToken == category) {
                Token current = tokenStream.Current;
                tokenStream.MoveNext();
                return current;
            } else {
                throw new SyntaxError(category, tokenStream.Current);                
            }
        }
        public void CProgram(){
            Program();
            Expect(TokenCategory.EOF);
        }
        public void Program() {            
            while(firstOfProgram.Contains(CurrentToken)){
                if(CurrentToken==TokenCategory.VAR){
                    VarDef();
                }
                else if(CurrentToken==TokenCategory.IDENTIFIER){
                    FunDef();
                }
                else{
                    throw new SyntaxError(firstOfProgram, tokenStream.Current);
                }
            }
        }
        public void VarDef() {
            Expect(TokenCategory.VAR);
            Expect(TokenCategory.IDENTIFIER);
            while(CurrentToken==TokenCategory.COMMA){

                Expect(TokenCategory.COMMA);
                Expect(TokenCategory.IDENTIFIER);
            }
            Expect(TokenCategory.SEMICOLON);
        }
        public void FunDef() {
            Expect(TokenCategory.IDENTIFIER);
            Expect(TokenCategory.PARENTHESIS_LEFT);

            if(CurrentToken==TokenCategory.IDENTIFIER){
                Expect(TokenCategory.IDENTIFIER);
                 while(CurrentToken==TokenCategory.COMMA){
                    Expect(TokenCategory.COMMA);
                    Expect(TokenCategory.IDENTIFIER);
                }
            }
            Expect(TokenCategory.PARENTHESIS_RIGHT);

            Expect(TokenCategory.CURLY_BRACE_LEFT);

            VarDefList();

            StmtList();
            Expect(TokenCategory.CURLY_BRACE_RIGHT);
        }
        public void VarDefList(){
            Expect(TokenCategory.VAR);
            Expect(TokenCategory.IDENTIFIER);
            while(CurrentToken==TokenCategory.COMMA){
                Expect(TokenCategory.COMMA);
                Expect(TokenCategory.IDENTIFIER);
            }
            Expect(TokenCategory.SEMICOLON);

        }
        public void StmtList(){
            while(firstOfStatement.Contains(CurrentToken)){
                Stmt();
            }
        }
        public void Stmt(){
            switch(CurrentToken){
                case TokenCategory.IDENTIFIER:
                    Expect(TokenCategory.IDENTIFIER);
                    if(CurrentToken==TokenCategory.ASSIGN){
                        Assign();
                    }
                    else if(CurrentToken==TokenCategory.PARENTHESIS_LEFT){
                        FunCall();
                    }
                    else{
                        throw new SyntaxError(new HashSet<TokenCategory>() {
                            TokenCategory.ASSIGN,
                            TokenCategory.PARENTHESIS_LEFT
                        }, tokenStream.Current);
                    }
                    break;
                case TokenCategory.IF:
                    Expect(TokenCategory.IF);
                    IfStmt();
                    break;
                case TokenCategory.SWITCH:
                    SwitchStmt();
                    break;
                case TokenCategory.WHILE:
                    WhileStmt();
                    break;
                case TokenCategory.DO:
                    DoWhileStmt();
                    break;
                case TokenCategory.FOR:
                    ForStmt();
                    break;
                case TokenCategory.BREAK:
                    Expect(TokenCategory.BREAK);
                    Expect(TokenCategory.SEMICOLON);
                    break;
                case TokenCategory.CONTINUE:
                    Expect(TokenCategory.CONTINUE);
                    Expect(TokenCategory.SEMICOLON);
                    break;
                case TokenCategory.RETURN:
                    Expect(TokenCategory.RETURN);
                    Expr();
                    Expect(TokenCategory.SEMICOLON);
                    break;
                case TokenCategory.SEMICOLON:
                    Expect(TokenCategory.SEMICOLON);
                    break;
                default:
                    throw new SyntaxError(firstOfStatement, tokenStream.Current);
            }
        }
        public void Assign(){
            Expect(TokenCategory.ASSIGN);
            Expr();
            Expect(TokenCategory.SEMICOLON);
        }
        public void FunCall(){
            Expect(TokenCategory.PARENTHESIS_LEFT);
            if(firstOfExprPrimary.Contains(CurrentToken)){

                Expr();
                while(CurrentToken==TokenCategory.COMMA){
                    Expect(TokenCategory.COMMA);
                    Expr();
                }
            }
            Expect(TokenCategory.PARENTHESIS_RIGHT);
            Expect(TokenCategory.SEMICOLON);
        }
        public void IfStmt(){
            Expect(TokenCategory.PARENTHESIS_LEFT);
            Expr();
            Expect(TokenCategory.PARENTHESIS_RIGHT);
            Expect(TokenCategory.CURLY_BRACE_LEFT);
            while(firstOfStatement.Contains(CurrentToken)){
                Stmt();
            }
            Expect(TokenCategory.CURLY_BRACE_RIGHT);
            while(CurrentToken==TokenCategory.ELSE){
                Expect(TokenCategory.ELSE);
                if(CurrentToken==TokenCategory.IF){
                    Expect(TokenCategory.IF);
                    Expect(TokenCategory.PARENTHESIS_LEFT);
                    Expr();
                    Expect(TokenCategory.PARENTHESIS_RIGHT);
                    Expect(TokenCategory.CURLY_BRACE_LEFT);
                    while(firstOfStatement.Contains(CurrentToken)){
                        Stmt();
                    }
                    Expect(TokenCategory.CURLY_BRACE_RIGHT);
                }
                else if(CurrentToken==TokenCategory.CURLY_BRACE_LEFT){
                    Expect(TokenCategory.CURLY_BRACE_LEFT);
                    while(firstOfStatement.Contains(CurrentToken)){
                        Stmt();
                    }
                    Expect(TokenCategory.CURLY_BRACE_RIGHT);
                    break;
                }
                
            }
            
        }
        public void SwitchStmt(){
            Expect(TokenCategory.PARENTHESIS_LEFT);
            Expr();
            Expect(TokenCategory.PARENTHESIS_RIGHT);
            Expect(TokenCategory.CURLY_BRACE_LEFT);
            while(CurrentToken==TokenCategory.CASE){
                Case();
            }
            if(CurrentToken==TokenCategory.DEFAULT){
                Default();
            }
            Expect(TokenCategory.CURLY_BRACE_RIGHT);
        }
        public void Case(){
            Expect(TokenCategory.CASE);
            Lit();
            while(CurrentToken==TokenCategory.COMMA){
                Expect(TokenCategory.COMMA);
                Lit();
            }
            Expect(TokenCategory.COLON);
            while(firstOfStatement.Contains(CurrentToken)){
                Stmt();
            }
        }
        public void Lit(){
            switch(CurrentToken){
                case TokenCategory.BOOL:
                    Expect(TokenCategory.BOOL);
                    break;
                case TokenCategory.INT:
                    Expect(TokenCategory.INT);
                    break;
                case TokenCategory.CHARACTER:
                    Expect(TokenCategory.CHARACTER);
                    break;
                default:
                    throw new SyntaxError(new HashSet<TokenCategory>() {
                        TokenCategory.BOOL,
                        TokenCategory.INT,
                        TokenCategory.CHARACTER
                    }, tokenStream.Current);
            }
            
        }
        public void Default(){
            Expect(TokenCategory.DEFAULT);
            Expect(TokenCategory.COLON);
            while(firstOfStatement.Contains(CurrentToken)){
                Stmt();
            }
        }
        public void WhileStmt(){
            Expect(TokenCategory.WHILE);

            Expect(TokenCategory.PARENTHESIS_LEFT);
            Expr();
            Expect(TokenCategory.PARENTHESIS_RIGHT);
            Expect(TokenCategory.CURLY_BRACE_LEFT);
            while(firstOfStatement.Contains(CurrentToken)){
                Stmt();
            }
            Expect(TokenCategory.CURLY_BRACE_RIGHT);
        }
        public void DoWhileStmt(){
            Expect(TokenCategory.DO);

            Expect(TokenCategory.CURLY_BRACE_LEFT);
            
            while(firstOfStatement.Contains(CurrentToken)){
                Stmt();
            }
            Expect(TokenCategory.CURLY_BRACE_RIGHT);
            Expect(TokenCategory.PARENTHESIS_LEFT);
            Expr();
            Expect(TokenCategory.PARENTHESIS_RIGHT);
            Expect(TokenCategory.COLON);
        }
        public void ForStmt(){
            Expect(TokenCategory.FOR);

            Expect(TokenCategory.PARENTHESIS_LEFT);
            Expect(TokenCategory.IDENTIFIER);
            Expect(TokenCategory.IN);
            Expr();
            Expect(TokenCategory.PARENTHESIS_RIGHT);
            Expect(TokenCategory.CURLY_BRACE_LEFT);
            while(firstOfStatement.Contains(CurrentToken)){
                Stmt();
            }
            Expect(TokenCategory.CURLY_BRACE_RIGHT);
        }
        public void Expr(){
            ExprOr();

            if(CurrentToken==TokenCategory.QUESTION_MARK){
                Expect(TokenCategory.QUESTION_MARK);
                Expr();
                Expect(TokenCategory.COLON);
                Expr();
            }
        }
        public void ExprOr(){
            ExprAnd();

            while(CurrentToken==TokenCategory.OR){
                Expect(TokenCategory.OR);
                ExprAnd();
            }
        }
        public void ExprAnd(){
            ExprComp();

            while(CurrentToken==TokenCategory.AND){
                Expect(TokenCategory.AND);
                ExprComp();
            }
        }
        public void ExprComp(){
            ExprRel();

            while(CurrentToken==TokenCategory.NOT_EQUAL||CurrentToken==TokenCategory.EQUAL){
                switch(CurrentToken){
                    case TokenCategory.NOT_EQUAL:
                        Expect(TokenCategory.NOT_EQUAL);
                        ExprRel();
                        break;
                    case TokenCategory.EQUAL:
                        Expect(TokenCategory.EQUAL);
                        ExprRel();
                        break;
                    default:
                        throw new SyntaxError(new HashSet<TokenCategory>() {
                            TokenCategory.NOT_EQUAL,
                            TokenCategory.EQUAL
                        }, tokenStream.Current);
                    }
            }
        }
        public void ExprRel(){
            ExprBitOr();

            while(CurrentToken==TokenCategory.LESS_THAN||CurrentToken==TokenCategory.LESS_OR_EQUAL_THAN||CurrentToken==TokenCategory.GREATER_THAN||CurrentToken==TokenCategory.GREATER_OR_EQUAL_THAN){
                switch(CurrentToken){
                    case TokenCategory.LESS_THAN:
                        Expect(TokenCategory.LESS_THAN);
                        ExprBitOr();
                        break;
                    case TokenCategory.LESS_OR_EQUAL_THAN:
                        Expect(TokenCategory.LESS_OR_EQUAL_THAN);
                        ExprBitOr();
                        break;
                    case TokenCategory.GREATER_THAN:
                        Expect(TokenCategory.GREATER_THAN);
                        ExprBitOr();
                        break;
                    case TokenCategory.GREATER_OR_EQUAL_THAN:
                        Expect(TokenCategory.GREATER_OR_EQUAL_THAN);
                        ExprBitOr();
                        break;
                    default:
                        throw new SyntaxError(new HashSet<TokenCategory>() {
                            TokenCategory.LESS_THAN,
                            TokenCategory.LESS_OR_EQUAL_THAN,
                            TokenCategory.GREATER_THAN,
                            TokenCategory.GREATER_OR_EQUAL_THAN
                        }, tokenStream.Current);
                }
            }
        }
        public void ExprBitOr(){
            ExprBitAnd();

            while(CurrentToken==TokenCategory.BIT_OR||CurrentToken==TokenCategory.XOR){
                switch(CurrentToken){
                    case TokenCategory.BIT_OR:
                        Expect(TokenCategory.BIT_OR);
                        ExprBitAnd();
                        break;
                    case TokenCategory.XOR:
                        Expect(TokenCategory.XOR);
                        ExprBitAnd();
                        break;
                    
                    default:
                        throw new SyntaxError(new HashSet<TokenCategory>() {
                            TokenCategory.BIT_OR,
                            TokenCategory.XOR
                        }, tokenStream.Current);
                }
            }
        }
        public void ExprBitAnd(){
            ExprBitShift();

            while(CurrentToken==TokenCategory.BIT_AND){
                switch(CurrentToken){
                    case TokenCategory.BIT_AND:
                        Expect(TokenCategory.BIT_AND);
                        ExprBitShift();
                        break;
                    default:
                        throw new SyntaxError(new HashSet<TokenCategory>() {
                            TokenCategory.BIT_AND,
                        }, tokenStream.Current);
                }
            }
        }
        public void ExprBitShift(){
            ExprAdd();
            while(CurrentToken==TokenCategory.SHIFT_LEFT||CurrentToken==TokenCategory.SHIFT_RIGHT||CurrentToken==TokenCategory.SHIFT_RIGHT_ALT){
                switch(CurrentToken){
                    case TokenCategory.SHIFT_LEFT:
                        Expect(TokenCategory.SHIFT_LEFT);
                        ExprAdd();
                        break;
                    case TokenCategory.SHIFT_RIGHT:
                        Expect(TokenCategory.SHIFT_RIGHT);
                        ExprAdd();
                        break;
                    case TokenCategory.SHIFT_RIGHT_ALT:
                        Expect(TokenCategory.SHIFT_RIGHT_ALT);
                        ExprAdd();
                        break;
                    
                    default:
                        throw new SyntaxError(new HashSet<TokenCategory>() {
                            TokenCategory.SHIFT_LEFT,
                            TokenCategory.SHIFT_RIGHT,
                            TokenCategory.SHIFT_RIGHT_ALT
                        }, tokenStream.Current);
                }
            }
        }
        public void ExprAdd(){
            ExprMul();
            while(CurrentToken==TokenCategory.SUBTRACTION||CurrentToken==TokenCategory.ADDITION){
                switch(CurrentToken){
                    case TokenCategory.SUBTRACTION:
                        Expect(TokenCategory.SUBTRACTION);
                        ExprMul();
                        break;
                    case TokenCategory.ADDITION:
                        Expect(TokenCategory.ADDITION);
                        ExprMul();
                        break;
                    
                    default:
                        throw new SyntaxError(new HashSet<TokenCategory>() {
                            TokenCategory.SUBTRACTION,
                            TokenCategory.ADDITION
                        }, tokenStream.Current);
                }
            }
        }
        public void ExprMul(){
            ExprPow();
            while(CurrentToken==TokenCategory.MULTIPLICATION||CurrentToken==TokenCategory.DIVISION||CurrentToken==TokenCategory.MODULUS){
                switch(CurrentToken){
                    case TokenCategory.MULTIPLICATION:
                        Expect(TokenCategory.MULTIPLICATION);
                        ExprPow();
                        break;
                    case TokenCategory.DIVISION:
                        Expect(TokenCategory.DIVISION);
                        ExprPow();
                        break;
                    case TokenCategory.MODULUS:
                        Expect(TokenCategory.MODULUS);
                        ExprPow();
                        break;
                    
                    default:
                        throw new SyntaxError(new HashSet<TokenCategory>() {
                            TokenCategory.MULTIPLICATION,
                            TokenCategory.DIVISION,
                            TokenCategory.MODULUS,
                        }, tokenStream.Current);
                }
            }
        }
        public void ExprPow(){
            ExprUnary();
            while(CurrentToken==TokenCategory.POWER){
                Expect(TokenCategory.POWER);
                ExprPow();
            }
        }
        public void ExprUnary(){
            if(firstOfUnary.Contains(CurrentToken)){
                switch(CurrentToken){
                    case TokenCategory.ADDITION:
                        Expect(TokenCategory.ADDITION);
                        ExprUnary();
                        break;
                    case TokenCategory.SUBTRACTION:
                        Expect(TokenCategory.ADDITION);
                        ExprUnary();
                        break;
                    case TokenCategory.NOT:
                        Expect(TokenCategory.NOT);
                        ExprUnary();
                        break;
                    case TokenCategory.BIT_NOT:
                        Expect(TokenCategory.BIT_NOT);
                        ExprUnary();
                        break;
                    default:
                        throw new SyntaxError(new HashSet<TokenCategory>() {
                            TokenCategory.ADDITION,
                            TokenCategory.SUBTRACTION,
                            TokenCategory.NOT,
                            TokenCategory.BIT_NOT
                        }, tokenStream.Current);
                }
            }
            else if(firstOfExprPrimary.Contains(CurrentToken)){
                ExprPrimary();
            }
        }
        public void ExprPrimary(){
            if(CurrentToken==TokenCategory.IDENTIFIER){
                Expect(TokenCategory.IDENTIFIER);
                if(CurrentToken==TokenCategory.PARENTHESIS_LEFT){
                    FunCall();
                }
            }
            else if(firstOfLitAlt.Contains(CurrentToken)){
                LitAlt();
            }
            else if(CurrentToken==TokenCategory.PARENTHESIS_LEFT){
                Expect(TokenCategory.PARENTHESIS_LEFT);
                Expr();
                Expect(TokenCategory.PARENTHESIS_RIGHT);

            }
        }
        public void LitAlt(){
            if(firstOfLit.Contains(CurrentToken)){
                Lit();
            }
            else if(CurrentToken==TokenCategory.STRING){
                Expect(TokenCategory.STRING);
            }
            else if(CurrentToken==TokenCategory.CURLY_BRACE_LEFT){
                ArrayList();
            }
        }
        public void ArrayList(){
            Expect(TokenCategory.CURLY_BRACE_LEFT);
            if(firstOfLit.Contains(CurrentToken)){
                Lit();
                while(CurrentToken==TokenCategory.COMMA){
                    Expect(TokenCategory.COMMA);
                    Lit();
                }
            }
            
            Expect(TokenCategory.CURLY_BRACE_RIGHT);

        }

    }
}

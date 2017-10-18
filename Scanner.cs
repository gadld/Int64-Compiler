/*
Authors:
 - Gad Levy A01017986
 - Jonathan Ginsburg A01021617
 - Pablo de la Mora A01020365
*/

using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;

namespace Int64 {

    class Scanner {

        readonly string input;

        static readonly Regex newLineRegex = new Regex(@"\n", RegexOptions.Compiled);
        static readonly Regex regex = new Regex(
            @"
                (?<Assign>     [=]       )
              | (?<MultilineComment>    [/][*][^*]*[*]+([^/*][^*]*[*]+)*[/]       )
              | (?<Comment>    [/][/].*       )
              | (?<QuestionMark>    [?]       )
              | (?<NewLine>     \n       )
              | (?<Equal>    [==]       )
              | (?<NotEqual>    [!=]       )
              | (?<GreaterThan>      [>]       )
              | (?<GreatOrEqualThan>    [>=]       )
              | (?<LessThan>       [<]       )
              | (?<LessOrEqualThan>     [<=]       )
              | (?<Or>     [||]       )
              | (?<BitOr>     [|]       )
              | (?<Xor>     [\^]       )
              | (?<And>     [&&]       )
              | (?<BitAnd>     [&]       )
              | (?<ShiftLeft>     [<<]       )
              | (?<ShiftRight>     [>>]       )
              | (?<ShiftRightAlt>     [>>>]       )
              | (?<Power>      [**]       )
              | (?<Multiplication>        [*]       )
              | (?<Subtraction>        [-]       )
              | (?<Addition>       [+]       )
              | (?<Division>   [/]       )
              | (?<Modulus>    [%]       )
              | (?<Not>    [!]       )
              | (?<BitNot>    [~]       )
              | (?<Base2>      0(b|B)(0|1)+       )
              | (?<Base8>      0(o|O)[0-7]+       )
              | (?<Base16>     0(x|X)[0-9a-fA-F]+       )
              | (?<Base10> \d+       )
              | (?<ParenthesisLeft>    [(]       )
              | (?<ParenthesisRight>   [)]       )
              | (?<CurlyBraceLeft>   [{]       )
              | (?<CurlyBraceRight>   [}]       )
              | (?<Colon>       [:]       )
              | (?<Semicolon>       [;]       )
              | (?<Comma>      [,]       )
              | (?<String>    [""](\\.|[^""])*[""]      )
              | (?<Character>    ['](\\.|[^'])*[']      )
              | (?<Identifier> [a-zA-Z_]+ )
              | (?<WhiteSpace> \s        )     # Must go anywhere after Newline.
              | (?<Unknown>      .         )     # Must be last: match any other character.
            ",
            RegexOptions.IgnorePatternWhitespace
                | RegexOptions.Compiled
                | RegexOptions.Multiline
            );

        static readonly IDictionary<string, TokenCategory> keywords =
            new Dictionary<string, TokenCategory>() {
                {"break", TokenCategory.BREAK},
                {"else", TokenCategory.ELSE},
                {"return", TokenCategory.RETURN},
                {"case", TokenCategory.CASE},
                {"false", TokenCategory.TRUE},
                {"switch", TokenCategory.SWITCH},
                {"continue", TokenCategory.CONTINUE},
                {"for", TokenCategory.FOR},
                {"true", TokenCategory.FALSE},
                {"default", TokenCategory.DEFAULT},
                {"if", TokenCategory.IF},
                {"do", TokenCategory.DO},
                {"in", TokenCategory.IN},
                {"var", TokenCategory.VAR},
                {"while", TokenCategory.WHILE}
            };

        static readonly IDictionary<string, TokenCategory> nonKeywords =
            new Dictionary<string, TokenCategory>() {
                {"Assign", TokenCategory.ASSIGN},
                //{"Comment", TokenCategory.COMMENT},
                {"QuestionMark", TokenCategory.QUESTION_MARK},
                {"Equal", TokenCategory.EQUAL},
                {"NotEqual", TokenCategory.NOT_EQUAL},
                {"GreaterThan", TokenCategory.GREATER_THAN},
                {"GreaterOrEqualThan", TokenCategory.GREATER_OR_EQUAL_THAN},
                {"LessThan", TokenCategory.LESS_THAN},
                {"LessOrEqualThan", TokenCategory.LESS_OR_EQUAL_THAN},
                {"Or", TokenCategory.OR},
                {"BitOr", TokenCategory.BIT_OR},
                {"Xor", TokenCategory.XOR},
                {"And", TokenCategory.AND},
                {"BitAnd", TokenCategory.BIT_AND},
                {"ShiftLeft", TokenCategory.SHIFT_LEFT},
                {"ShiftRight", TokenCategory.SHIFT_RIGHT},
                {"ShiftRightAlt", TokenCategory.SHIFT_RIGHT_ALT},
                {"Power", TokenCategory.POWER},
                {"Multiplication", TokenCategory.MULTIPLICATION},
                {"Subtraction", TokenCategory.SUBTRACTION},
                {"Addition", TokenCategory.ADDITION},
                {"Division", TokenCategory.DIVISION},
                {"Modulus", TokenCategory.MODULUS},
                {"Not", TokenCategory.NOT},
                {"BitNot", TokenCategory.BIT_NOT},
                {"Base2", TokenCategory.BASE_2},
                {"Base8", TokenCategory.BASE_8},
                {"Base16", TokenCategory.BASE_16},
                {"Base10", TokenCategory.BASE_10},
                {"ParenthesisLeft", TokenCategory.PARENTHESIS_LEFT},
                {"ParenthesisRight", TokenCategory.PARENTHESIS_RIGHT},
                {"CurlyBraceLeft", TokenCategory.CURLY_BRACE_LEFT},
                {"CurlyBraceRight", TokenCategory.CURLY_BRACE_RIGHT},
                {"Colon", TokenCategory.COLON},
                {"Semicolon", TokenCategory.SEMICOLON},
                {"Comma", TokenCategory.COMMA},
                {"String", TokenCategory.STRING},
                {"Character", TokenCategory.CHARACTER},
                {"WhiteSpace", TokenCategory.WHITE_SPACE},
                {"Unknown", TokenCategory.UNKNOWN},
                {"Identifier", TokenCategory.IDENTIFIER},
            };

        public Scanner(string input) {
            this.input = input;
        }

        public IEnumerable<Token> Start() {

            var row = 1;
            var columnStart = 0;

            Func<Match, TokenCategory, Token> newTok = (m, tc) =>
                new Token(m.Value, tc, row, m.Index - columnStart + 1);

            foreach (Match m in regex.Matches(input)) {

                if (m.Groups["NewLine"].Success) {

                    // Found a new line.
                    row++;
                    columnStart = m.Index + m.Length;

                } else if (m.Groups["WhiteSpace"].Success
                    || m.Groups["Comment"].Success) {
                    // Skip white space and comments.

                } else if (m.Groups["MultilineComment"].Success) {
                  foreach (Match nl in newLineRegex.Matches(m.Value)) {
                    row++;
                  }
                } else if (m.Groups["Identifier"].Success) {

                    if (keywords.ContainsKey(m.Value)) {

                        // Matched string is a Buttercup keyword.
                        yield return newTok(m, keywords[m.Value]);

                    } else {
                        // Otherwise it's just a plain identifier.
                        yield return newTok(m, TokenCategory.IDENTIFIER);
                    }
                } else if (m.Groups["String"].Success) {

                   yield return newTok(m, TokenCategory.STRING);

                } else if (m.Groups["Unknown"].Success) {

                    // Found an illegal character.
                    yield return newTok(m, TokenCategory.UNKNOWN);

                } else {

                    // Match must be one of the non keywords.
                    foreach (var name in nonKeywords.Keys) {
                        if (m.Groups[name].Success) {
                            yield return newTok(m, nonKeywords[name]);
                            break;
                        }
                    }
                }
            }

            yield return new Token(null,
                                   TokenCategory.EOF,
                                   row,
                                   input.Length - columnStart + 1);
        }
    }
}
#include <iostream>
#include <sstream>
#include <vector>
#include <cctype>
#include <string>
#include <map>
#include <unordered_map>
#include <stdexcept>
#include <memory>  // Add this for unique_ptr
using namespace std;

// -------------------- TOKEN TYPES --------------------
enum TokenType {
    // Data types
    INT, FLOAT, DOUBLE, CHAR, STRING, BOOL, LONG, SHORT, VOID, AUTO,
    
    // Values
    IDENTIFIER, NUMBER, FLOAT_NUMBER, STRING_LITERAL, CHAR_LITERAL, TRUE, FALSE,
    
    // Keywords
    IF, ELSE, WHILE, FOR, RETURN, BREAK, CONTINUE, SWITCH, CASE, DEFAULT,
    DO, STRUCT, CLASS, PUBLIC, PRIVATE, PROTECTED, CONST, STATIC,
    
    // Operators
    PLUS, MINUS, MUL, DIV, MOD,
    ASSIGN, EQ, NEQ, LT, GT, LE, GE,
    AND, OR, NOT, BIT_AND, BIT_OR, BIT_XOR, BIT_NOT,
    PLUS_ASSIGN, MINUS_ASSIGN, MUL_ASSIGN, DIV_ASSIGN, MOD_ASSIGN,
    INC, DEC,
    
    // Delimiters
    LPAREN, RPAREN, LBRACE, RBRACE, LBRACKET, RBRACKET,
    SEMI, COMMA, DOT, COLON, QUESTION, ARROW,
    
    // Special
    END_OF_FILE, ERROR
};

struct Token {
    TokenType type;
    string value;
    int line;
    int column;
    
    Token(TokenType t = END_OF_FILE, const string& v = "", int l = 0, int c = 0) 
        : type(t), value(v), line(l), column(c) {}
};

// -------------------- LEXER --------------------
class Lexer {
    string input;
    int pos = 0;
    int line = 1;
    int column = 1;
    
    char peek(int offset = 0) {
        if (pos + offset < input.size()) 
            return input[pos + offset];
        return '\0';
    }
    
    void advance() {
        if (pos < input.size()) {
            if (input[pos] == '\n') {
                line++;
                column = 1;
            } else {
                column++;
            }
            pos++;
        }
    }
    
public:
    Lexer(const string& src) : input(src) {}

    // Skip whitespace and comments
    void skipWhitespaceAndComments() {
        while (pos < input.size()) {
            if (isspace(input[pos])) {
                advance();
            } 
            // Single line comments
            else if (input[pos] == '/' && peek(1) == '/') {
                while (pos < input.size() && input[pos] != '\n') {
                    advance();
                }
            }
            // Multi-line comments
            else if (input[pos] == '/' && peek(1) == '*') {
                advance(); advance(); // skip /*
                while (pos < input.size() && !(input[pos] == '*' && peek(1) == '/')) {
                    advance();
                }
                if (pos < input.size()) {
                    advance(); advance(); // skip */
                }
            }
            else {
                break;
            }
        }
    }

    // Get next token
    Token getNextToken() {
        skipWhitespaceAndComments();
        if (pos >= input.size()) return Token(END_OF_FILE, "", line, column);

        char current = input[pos];
        int startLine = line;
        int startCol = column;

        // Identifiers or keywords
        if (isalpha(current) || current == '_') {
            string value;
            while (pos < input.size() && (isalnum(input[pos]) || input[pos]=='_')) {
                value += input[pos];
                advance();
            }
            
            // Keywords
            static const unordered_map<string, TokenType> keywords = {
                {"int", INT}, {"float", FLOAT}, {"double", DOUBLE}, {"char", CHAR},
                {"string", STRING}, {"bool", BOOL}, {"long", LONG}, {"short", SHORT},
                {"void", VOID}, {"auto", AUTO},
                {"if", IF}, {"else", ELSE}, {"while", WHILE}, {"for", FOR},
                {"return", RETURN}, {"break", BREAK}, {"continue", CONTINUE},
                {"switch", SWITCH}, {"case", CASE}, {"default", DEFAULT},
                {"do", DO}, {"struct", STRUCT}, {"class", CLASS},
                {"public", PUBLIC}, {"private", PRIVATE}, {"protected", PROTECTED},
                {"const", CONST}, {"static", STATIC},
                {"true", TRUE}, {"false", FALSE}
            };
            
            auto it = keywords.find(value);
            if (it != keywords.end()) {
                return Token(it->second, value, startLine, startCol);
            }
            return Token(IDENTIFIER, value, startLine, startCol);
        }

        // Numbers (integer and floating point)
        if (isdigit(current)) {
            string num;
            bool hasDot = false;
            bool hasExponent = false;
            
            while (pos < input.size() && 
                  (isdigit(input[pos]) || input[pos]=='.' || 
                   input[pos]=='e' || input[pos]=='E' ||
                   input[pos]=='+' || input[pos]=='-')) {
                
                if (input[pos] == '.') {
                    if (hasDot) break; // multiple dots not allowed
                    hasDot = true;
                }
                if (input[pos] == 'e' || input[pos] == 'E') {
                    hasExponent = true;
                }
                
                num += input[pos];
                advance();
            }
            
            // Check for float suffixes
            if (pos < input.size() && (input[pos] == 'f' || input[pos] == 'F')) {
                num += input[pos];
                advance();
                hasDot = true; // Treat as float
            }
            
            return Token((hasDot || hasExponent) ? FLOAT_NUMBER : NUMBER, num, startLine, startCol);
        }

        // Character literals
        if (current == '\'') {
            advance();
            string ch;
            
            // Handle escape sequences
            if (pos < input.size() && input[pos] == '\\') {
                advance();
                if (pos < input.size()) {
                    switch (input[pos]) {
                        case 'n': ch = "\n"; break;
                        case 't': ch = "\t"; break;
                        case 'r': ch = "\r"; break;
                        case '\\': ch = "\\"; break;
                        case '\'': ch = "'"; break;
                        case '"': ch = "\""; break;
                        default: ch = string(1, input[pos]); break;
                    }
                    advance();
                }
            } else if (pos < input.size()) {
                ch = input[pos];
                advance();
            }
            
            if (pos >= input.size() || input[pos] != '\'') {
                return Token(ERROR, "Unterminated character literal", startLine, startCol);
            }
            advance();
            return Token(CHAR_LITERAL, ch, startLine, startCol);
        }

        // String literals
        if (current == '"') {
            advance();
            string str;
            bool escaped = false;
            
            while (pos < input.size() && (escaped || input[pos] != '"')) {
                if (escaped) {
                    switch (input[pos]) {
                        case 'n': str += '\n'; break;
                        case 't': str += '\t'; break;
                        case 'r': str += '\r'; break;
                        case '\\': str += '\\'; break;
                        case '"': str += '"'; break;
                        default: str += input[pos]; break;
                    }
                    escaped = false;
                } else {
                    if (input[pos] == '\\') {
                        escaped = true;
                    } else {
                        str += input[pos];
                    }
                }
                advance();
            }
            
            if (pos >= input.size()) {
                return Token(ERROR, "Unterminated string literal", startLine, startCol);
            }
            advance(); // skip closing "
            return Token(STRING_LITERAL, str, startLine, startCol);
        }

        // Multi-character operators
        if (current == '=' && peek(1) == '=') {
            advance(); advance();
            return Token(EQ, "==", startLine, startCol);
        }
        if (current == '!' && peek(1) == '=') {
            advance(); advance();
            return Token(NEQ, "!=", startLine, startCol);
        }
        if (current == '<' && peek(1) == '=') {
            advance(); advance();
            return Token(LE, "<=", startLine, startCol);
        }
        if (current == '>' && peek(1) == '=') {
            advance(); advance();
            return Token(GE, ">=", startLine, startCol);
        }
        if (current == '&' && peek(1) == '&') {
            advance(); advance();
            return Token(AND, "&&", startLine, startCol);
        }
        if (current == '|' && peek(1) == '|') {
            advance(); advance();
            return Token(OR, "||", startLine, startCol);
        }
        if (current == '+' && peek(1) == '+') {
            advance(); advance();
            return Token(INC, "++", startLine, startCol);
        }
        if (current == '-' && peek(1) == '-') {
            advance(); advance();
            return Token(DEC, "--", startLine, startCol);
        }
        if (current == '+' && peek(1) == '=') {
            advance(); advance();
            return Token(PLUS_ASSIGN, "+=", startLine, startCol);
        }
        if (current == '-' && peek(1) == '=') {
            advance(); advance();
            return Token(MINUS_ASSIGN, "-=", startLine, startCol);
        }
        if (current == '*' && peek(1) == '=') {
            advance(); advance();
            return Token(MUL_ASSIGN, "*=", startLine, startCol);
        }
        if (current == '/' && peek(1) == '=') {
            advance(); advance();
            return Token(DIV_ASSIGN, "/=", startLine, startCol);
        }
        if (current == '-' && peek(1) == '>') {
            advance(); advance();
            return Token(ARROW, "->", startLine, startCol);
        }

        // Single-char tokens
        switch (current) {
            case '+': advance(); return Token(PLUS, "+", startLine, startCol);
            case '-': advance(); return Token(MINUS, "-", startLine, startCol);
            case '*': advance(); return Token(MUL, "*", startLine, startCol);
            case '/': advance(); return Token(DIV, "/", startLine, startCol);
            case '%': advance(); return Token(MOD, "%", startLine, startCol);
            case '=': advance(); return Token(ASSIGN, "=", startLine, startCol);
            case '<': advance(); return Token(LT, "<", startLine, startCol);
            case '>': advance(); return Token(GT, ">", startLine, startCol);
            case '!': advance(); return Token(NOT, "!", startLine, startCol);
            case '&': advance(); return Token(BIT_AND, "&", startLine, startCol);
            case '|': advance(); return Token(BIT_OR, "|", startLine, startCol);
            case '^': advance(); return Token(BIT_XOR, "^", startLine, startCol);
            case '~': advance(); return Token(BIT_NOT, "~", startLine, startCol);
            case ';': advance(); return Token(SEMI, ";", startLine, startCol);
            case '(': advance(); return Token(LPAREN, "(", startLine, startCol);
            case ')': advance(); return Token(RPAREN, ")", startLine, startCol);
            case '{': advance(); return Token(LBRACE, "{", startLine, startCol);
            case '}': advance(); return Token(RBRACE, "}", startLine, startCol);
            case '[': advance(); return Token(LBRACKET, "[", startLine, startCol);
            case ']': advance(); return Token(RBRACKET, "]", startLine, startCol);
            case ',': advance(); return Token(COMMA, ",", startLine, startCol);
            case '.': advance(); return Token(DOT, ".", startLine, startCol);
            case ':': advance(); return Token(COLON, ":", startLine, startCol);
            case '?': advance(); return Token(QUESTION, "?", startLine, startCol);
        }

        // Unknown character
        string unknown(1, current);
        advance();
        return Token(ERROR, "Unknown character: " + unknown, startLine, startCol);
    }
};

// -------------------- AST NODES --------------------
struct ASTNode {
    virtual ~ASTNode() = default;
    virtual void print(int indent = 0) const = 0;
};

struct ProgramNode : public ASTNode {
    vector<unique_ptr<ASTNode>> statements;
    
    void print(int indent = 0) const override {
        cout << string(indent, ' ') << "Program:" << endl;
        for (const auto& stmt : statements) {
            stmt->print(indent + 2);
        }
    }
};

struct VarDeclarationNode : public ASTNode {
    TokenType type;
    string name;
    unique_ptr<ASTNode> initializer;
    
    void print(int indent = 0) const override {
        cout << string(indent, ' ') << "VarDecl: " << name << endl;
        if (initializer) {
            initializer->print(indent + 2);
        }
    }
};

struct ExpressionNode : public ASTNode {
    virtual ~ExpressionNode() = default;
};

struct BinaryOpNode : public ExpressionNode {
    TokenType op;
    unique_ptr<ExpressionNode> left;
    unique_ptr<ExpressionNode> right;
    
    void print(int indent = 0) const override {
        cout << string(indent, ' ') << "BinaryOp: " << op << endl;
        if (left) left->print(indent + 2);
        if (right) right->print(indent + 2);
    }
};

struct LiteralNode : public ExpressionNode {
    TokenType type;
    string value;
    
    LiteralNode(TokenType t = IDENTIFIER, const string& v = "") : type(t), value(v) {}
    
    void print(int indent = 0) const override {
        cout << string(indent, ' ') << "Literal: " << value << " (" << type << ")" << endl;
    }
};

struct IdentifierNode : public ExpressionNode {
    string name;
    
    IdentifierNode(const string& n = "") : name(n) {}
    
    void print(int indent = 0) const override {
        cout << string(indent, ' ') << "Identifier: " << name << endl;
    }
};

// -------------------- PARSER --------------------
class Parser {
    Lexer lexer;
    Token current;

    void advance() { current = lexer.getNextToken(); }

    void expect(TokenType type, const string& msg = "") {
        if (current.type != type) {
            cerr << "Syntax Error at line " << current.line << ":" << current.column 
                 << " - Expected " << type << " got " << current.value;
            if (!msg.empty()) cerr << " - " << msg;
            cerr << endl;
            exit(1);
        }
        advance();
    }

    bool match(TokenType type) {
        if (current.type == type) {
            advance();
            return true;
        }
        return false;
    }

public:
    Parser(Lexer l) : lexer(l) { advance(); }

    // Program = {Declaration | Statement}
    unique_ptr<ProgramNode> parseProgram() {
        auto program = make_unique<ProgramNode>();
        
        while (current.type != END_OF_FILE) {
            if (isDataType(current.type)) {
                program->statements.push_back(parseDeclaration());
            } else {
                program->statements.push_back(parseStatement());
            }
        }
        return program;
    }

    // Declaration: Type IDENTIFIER (= Expression)? ;
    unique_ptr<VarDeclarationNode> parseDeclaration() {
        auto decl = make_unique<VarDeclarationNode>();
        decl->type = current.type;
        advance(); // skip type
        
        if (current.type != IDENTIFIER) {
            cerr << "Syntax Error: Expected identifier after type" << endl;
            exit(1);
        }
        decl->name = current.value;
        advance(); // skip identifier
        
        if (match(ASSIGN)) {
            decl->initializer = parseExpression();
        }
        expect(SEMI, "Expected ';' after declaration");
        return decl;
    }

    // Statement: if | while | for | do-while | switch | return | break | continue | block | expression
    unique_ptr<ASTNode> parseStatement() {
        switch (current.type) {
            case IF: return parseIfStatement();
            case WHILE: return parseWhileStatement();
            case FOR: return parseForStatement();
            case DO: return parseDoWhileStatement();
            case SWITCH: return parseSwitchStatement();
            case RETURN: return parseReturnStatement();
            case BREAK: return parseBreakStatement();
            case CONTINUE: return parseContinueStatement();
            case LBRACE: return parseBlockStatement();
            default: return parseExpressionStatement();
        }
    }

    unique_ptr<ASTNode> parseIfStatement() {
        expect(IF);
        expect(LPAREN);
        auto condition = parseExpression();
        expect(RPAREN);
        
        auto thenBranch = parseStatement();
        unique_ptr<ASTNode> elseBranch = nullptr;
        
        if (match(ELSE)) {
            elseBranch = parseStatement();
        }
        
        // For simplicity, return the condition
        return move(condition);
    }

    unique_ptr<ASTNode> parseWhileStatement() {
        expect(WHILE);
        expect(LPAREN);
        auto condition = parseExpression();
        expect(RPAREN);
        auto body = parseStatement();
        return move(condition);
    }

    unique_ptr<ASTNode> parseForStatement() {
        expect(FOR);
        expect(LPAREN);
        
        // Initialization
        unique_ptr<ASTNode> init = nullptr;
        if (!match(SEMI)) {
            if (isDataType(current.type)) {
                init = parseDeclaration();
            } else {
                init = parseExpression();
                expect(SEMI);
            }
        }
        
        // Condition
        unique_ptr<ASTNode> condition = nullptr;
        if (!match(SEMI)) {
            condition = parseExpression();
            expect(SEMI);
        }
        
        // Increment
        unique_ptr<ASTNode> increment = nullptr;
        if (!match(RPAREN)) {
            increment = parseExpression();
            expect(RPAREN);
        }
        
        auto body = parseStatement();
        return condition ? move(condition) : make_unique<LiteralNode>(TRUE, "true");
    }

    unique_ptr<ASTNode> parseDoWhileStatement() {
        expect(DO);
        auto body = parseStatement();
        expect(WHILE);
        expect(LPAREN);
        auto condition = parseExpression();
        expect(RPAREN);
        expect(SEMI);
        return move(condition);
    }

    unique_ptr<ASTNode> parseSwitchStatement() {
        expect(SWITCH);
        expect(LPAREN);
        auto expr = parseExpression();
        expect(RPAREN);
        expect(LBRACE);
        
        while (current.type != RBRACE && current.type != END_OF_FILE) {
            if (match(CASE)) {
                parseExpression();
                expect(COLON);
            } else if (match(DEFAULT)) {
                expect(COLON);
            } else {
                parseStatement();
            }
        }
        expect(RBRACE);
        return move(expr);
    }

    unique_ptr<ASTNode> parseReturnStatement() {
        expect(RETURN);
        unique_ptr<ExpressionNode> expr = nullptr;
        if (!match(SEMI)) {
            expr = parseExpression();
            expect(SEMI);
        }
        return expr ? move(expr) : make_unique<LiteralNode>(VOID, "void");
    }

    unique_ptr<ASTNode> parseBreakStatement() {
        expect(BREAK);
        expect(SEMI);
        return make_unique<LiteralNode>(BREAK, "break");
    }

    unique_ptr<ASTNode> parseContinueStatement() {
        expect(CONTINUE);
        expect(SEMI);
        return make_unique<LiteralNode>(CONTINUE, "continue");
    }

    unique_ptr<ASTNode> parseBlockStatement() {
        expect(LBRACE);
        auto block = make_unique<ProgramNode>();
        
        while (current.type != RBRACE && current.type != END_OF_FILE) {
            if (isDataType(current.type)) {
                block->statements.push_back(parseDeclaration());
            } else {
                block->statements.push_back(parseStatement());
            }
        }
        expect(RBRACE);
        return move(block);
    }

    unique_ptr<ASTNode> parseExpressionStatement() {
        auto expr = parseExpression();
        expect(SEMI, "Expected ';' after expression");
        return move(expr);
    }

    // Expression parsing with operator precedence
    unique_ptr<ExpressionNode> parseExpression() {
        return parseAssignment();
    }

    unique_ptr<ExpressionNode> parseAssignment() {
        auto left = parseLogicalOr();
        
        if (current.type == ASSIGN || current.type == PLUS_ASSIGN || 
            current.type == MINUS_ASSIGN || current.type == MUL_ASSIGN || 
            current.type == DIV_ASSIGN || current.type == MOD_ASSIGN) {
            auto op = current.type;
            advance();
            auto right = parseAssignment();
            
            auto binOp = make_unique<BinaryOpNode>();
            binOp->op = op;
            binOp->left = move(left);
            binOp->right = move(right);
            return move(binOp);
        }
        
        return left;
    }

    unique_ptr<ExpressionNode> parseLogicalOr() {
        auto left = parseLogicalAnd();
        
        while (match(OR)) {
            auto op = OR;
            auto right = parseLogicalAnd();
            
            auto binOp = make_unique<BinaryOpNode>();
            binOp->op = op;
            binOp->left = move(left);
            binOp->right = move(right);
            left = move(binOp);
        }
        
        return left;
    }

    unique_ptr<ExpressionNode> parseLogicalAnd() {
        auto left = parseEquality();
        
        while (match(AND)) {
            auto op = AND;
            auto right = parseEquality();
            
            auto binOp = make_unique<BinaryOpNode>();
            binOp->op = op;
            binOp->left = move(left);
            binOp->right = move(right);
            left = move(binOp);
        }
        
        return left;
    }

    unique_ptr<ExpressionNode> parseEquality() {
        auto left = parseRelational();
        
        while (current.type == EQ || current.type == NEQ) {
            auto op = current.type;
            advance();
            auto right = parseRelational();
            
            auto binOp = make_unique<BinaryOpNode>();
            binOp->op = op;
            binOp->left = move(left);
            binOp->right = move(right);
            left = move(binOp);
        }
        
        return left;
    }

    unique_ptr<ExpressionNode> parseRelational() {
        auto left = parseAdditive();
        
        while (current.type == LT || current.type == GT || current.type == LE || current.type == GE) {
            auto op = current.type;
            advance();
            auto right = parseAdditive();
            
            auto binOp = make_unique<BinaryOpNode>();
            binOp->op = op;
            binOp->left = move(left);
            binOp->right = move(right);
            left = move(binOp);
        }
        
        return left;
    }

    unique_ptr<ExpressionNode> parseAdditive() {
        auto left = parseMultiplicative();
        
        while (current.type == PLUS || current.type == MINUS) {
            auto op = current.type;
            advance();
            auto right = parseMultiplicative();
            
            auto binOp = make_unique<BinaryOpNode>();
            binOp->op = op;
            binOp->left = move(left);
            binOp->right = move(right);
            left = move(binOp);
        }
        
        return left;
    }

    unique_ptr<ExpressionNode> parseMultiplicative() {
        auto left = parseUnary();
        
        while (current.type == MUL || current.type == DIV || current.type == MOD) {
            auto op = current.type;
            advance();
            auto right = parseUnary();
            
            auto binOp = make_unique<BinaryOpNode>();
            binOp->op = op;
            binOp->left = move(left);
            binOp->right = move(right);
            left = move(binOp);
        }
        
        return left;
    }

    unique_ptr<ExpressionNode> parseUnary() {
        if (current.type == PLUS || current.type == MINUS || 
            current.type == NOT || current.type == BIT_NOT || 
            current.type == INC || current.type == DEC) {
            auto op = current.type;
            advance();
            auto operand = parseUnary();
            
            // For simplicity, treat unary as binary with null left
            auto unaryOp = make_unique<BinaryOpNode>();
            unaryOp->op = op;
            unaryOp->right = move(operand);
            return move(unaryOp);
        }
        
        return parsePrimary();
    }

    unique_ptr<ExpressionNode> parsePrimary() {
        if (current.type == NUMBER || current.type == FLOAT_NUMBER || 
            current.type == STRING_LITERAL || current.type == CHAR_LITERAL ||
            current.type == TRUE || current.type == FALSE) {
            auto literal = make_unique<LiteralNode>();
            literal->type = current.type;
            literal->value = current.value;
            advance();
            return move(literal);
        }
        
        if (current.type == IDENTIFIER) {
            auto ident = make_unique<IdentifierNode>();
            ident->name = current.value;
            advance();
            return move(ident);
        }
        
        if (match(LPAREN)) {
            auto expr = parseExpression();
            expect(RPAREN);
            return expr;
        }
        
        cerr << "Syntax Error: Unexpected token " << current.value << endl;
        exit(1);
    }

private:
    bool isDataType(TokenType type) {
        return type == INT || type == FLOAT || type == DOUBLE || type == CHAR ||
               type == STRING || type == BOOL || type == LONG || type == SHORT ||
               type == VOID || type == AUTO;
    }
};

//---------------- MAIN ----------------
int main() {
    string input = R"(
        // Sample program with various constructs
        int x = 10;
        float y = 3.14f;
        double z = 2.71828;
        string s = "hello world";
        char c = '\n';
        bool flag = true;
        
        // Arithmetic operations
        x = x + 5 * (y - 2);
        
        // Control structures
        if (x > 10 && y < 5.0) {
            z = z / 2.0;
        } else {
            z = z * 2.0;
        }
        
        // Loops
        while (x < 100) {
            x = x + 1;
            if (x % 10 == 0) {
                continue;
            }
            if (x > 50) {
                break;
            }
        }
        
        for (int i = 0; i < 10; i = i + 1) {
            x = x + i;
        }
        
        // Function-like (simplified)
        return x + y;
    )";

    Lexer lexer(input);
    Parser parser(lexer);
    
    try {
        auto ast = parser.parseProgram();
        cout << "Program parsed successfully!" << endl;
        cout << "\nAbstract Syntax Tree:" << endl;
        ast->print();
    } catch (const exception& e) {
        cerr << "Error: " << e.what() << endl;
        return 1;
    }
    
    return 0;
}
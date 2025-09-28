#include <iostream>
#include <sstream>
#include <vector>
#include <cctype>
#include <string>
#include <string_view>
#include <map>
#include <unordered_map>
#include <stdexcept>
#include <memory>
#include <utility>
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
    
    // Optimized constructors with move semantics
    Token(TokenType t = END_OF_FILE, string&& v = "", int l = 0, int c = 0) 
        : type(t), value(std::move(v)), line(l), column(c) {}
    
    // Move constructor
    Token(Token&& other) noexcept 
        : type(other.type), value(std::move(other.value)), line(other.line), column(other.column) {}
    
    Token& operator=(Token&& other) noexcept {
        type = other.type;
        value = std::move(other.value);
        line = other.line;
        column = other.column;
        return *this;
    }
    
    // Disable copy (use move instead)
    Token(const Token&) = delete;
    Token& operator=(const Token&) = delete;
};

// -------------------- OPTIMIZED LEXER --------------------
class Lexer {
    string_view input;
    int pos = 0;
    int line = 1;
    int column = 1;
    
    // Static keyword map for efficiency
    static const unordered_map<string_view, TokenType> keywords;
    
    char peek(int offset = 0) const {
        size_t index = pos + offset;
        return index < input.size() ? input[index] : '\0';
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
    
    string_view substr(size_t start, size_t length) const {
        return input.substr(start, length);
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

        // Identifiers or keywords - optimized with string_view
        if (isalpha(current) || current == '_') {
            int start = pos;
            while (pos < input.size() && (isalnum(input[pos]) || input[pos]=='_')) {
                advance();
            }
            string_view value_view = substr(start, pos - start);
            
            // Convert to string only for map lookup
            auto it = keywords.find(value_view);
            if (it != keywords.end()) {
                return Token(it->second, string(value_view), startLine, startCol);
            }
            return Token(IDENTIFIER, string(value_view), startLine, startCol);
        }

        // Numbers - pre-allocate string
        if (isdigit(current)) {
            string num;
            num.reserve(16);  // Pre-allocate for most numbers
            
            bool hasDot = false;
            bool hasExponent = false;
            
            while (pos < input.size() && 
                  (isdigit(input[pos]) || input[pos]=='.' || 
                   input[pos]=='e' || input[pos]=='E' ||
                   input[pos]=='+' || input[pos]=='-')) {
                
                if (input[pos] == '.') {
                    if (hasDot) break;
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
                hasDot = true;
            }
            
            return Token((hasDot || hasExponent) ? FLOAT_NUMBER : NUMBER, std::move(num), startLine, startCol);
        }

        // Character literals
        if (current == '\'') {
            advance();
            string ch;
            ch.reserve(2);
            
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
            return Token(CHAR_LITERAL, std::move(ch), startLine, startCol);
        }

        // String literals
        if (current == '"') {
            advance();
            string str;
            str.reserve(32);  // Pre-allocate common string size
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
            advance();
            return Token(STRING_LITERAL, std::move(str), startLine, startCol);
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

// Define static keyword map
const unordered_map<string_view, TokenType> Lexer::keywords = {
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

// -------------------- OPTIMIZED AST NODES --------------------
struct ASTNode {
    virtual ~ASTNode() = default;
    virtual void print(ostream& os, int indent = 0) const = 0;
    
    // Helper for efficient indentation
    void printIndent(ostream& os, int indent) const {
        for (int i = 0; i < indent; ++i) os << ' ';
    }
};

struct ProgramNode : public ASTNode {
    vector<unique_ptr<ASTNode>> statements;
    
    void reserve(size_t capacity) {
        statements.reserve(capacity);
    }
    
    void print(ostream& os, int indent = 0) const override {
        printIndent(os, indent);
        os << "Program:\n";
        for (const auto& stmt : statements) {
            stmt->print(os, indent + 2);
        }
    }
};

struct VarDeclarationNode : public ASTNode {
    TokenType type;
    string name;
    unique_ptr<ASTNode> initializer;
    
    void print(ostream& os, int indent = 0) const override {
        printIndent(os, indent);
        os << "VarDecl: " << name << '\n';
        if (initializer) {
            initializer->print(os, indent + 2);
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
    
    void print(ostream& os, int indent = 0) const override {
        printIndent(os, indent);
        os << "BinaryOp: " << op << '\n';
        if (left) left->print(os, indent + 2);
        if (right) right->print(os, indent + 2);
    }
};

struct LiteralNode : public ExpressionNode {
    TokenType type;
    string value;
    
    LiteralNode(TokenType t = IDENTIFIER, string&& v = "") : type(t), value(std::move(v)) {}
    
    void print(ostream& os, int indent = 0) const override {
        printIndent(os, indent);
        os << "Literal: " << value << " (" << type << ")\n";
    }
};

struct IdentifierNode : public ExpressionNode {
    string name;
    
    IdentifierNode(string&& n = "") : name(std::move(n)) {}
    
    void print(ostream& os, int indent = 0) const override {
        printIndent(os, indent);
        os << "Identifier: " << name << '\n';
    }
};

// -------------------- OPTIMIZED PARSER --------------------
class Parser {
    Lexer lexer;
    Token current;
    
    // Cache for common patterns (simplified)
    unordered_map<size_t, unique_ptr<ExpressionNode>> expressionCache;

    void advance() { current = lexer.getNextToken(); }

    void expect(TokenType type, const string& msg = "") {
        if (current.type != type) {
            cerr << "Syntax Error at line " << current.line << ":" << current.column 
                 << " - Expected " << type << " got " << current.value;
            if (!msg.empty()) cerr << " - " << msg;
            cerr << '\n';
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
    
    // Fast type checking
    bool isDataType(TokenType type) const {
        return type == INT || type == FLOAT || type == DOUBLE || type == CHAR ||
               type == STRING || type == BOOL || type == LONG || type == SHORT ||
               type == VOID || type == AUTO;
    }

public:
    Parser(Lexer l) : lexer(l) { advance(); }

    // Program = {Declaration | Statement}
    unique_ptr<ProgramNode> parseProgram() {
        auto program = make_unique<ProgramNode>();
        program->reserve(64);  // Pre-allocate reasonable capacity
        
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
            cerr << "Syntax Error: Expected identifier after type\n";
            exit(1);
        }
        decl->name = std::move(current.value);  // Move the name
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
        block->reserve(16);
        
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

    // Expression parsing with operator precedence - optimized with switch
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
            
            auto unaryOp = make_unique<BinaryOpNode>();
            unaryOp->op = op;
            unaryOp->right = move(operand);
            return move(unaryOp);
        }
        
        return parsePrimary();
    }

    unique_ptr<ExpressionNode> parsePrimary() {
        // Use switch for faster dispatch
        switch (current.type) {
            case NUMBER: 
            case FLOAT_NUMBER: 
            case STRING_LITERAL: 
            case CHAR_LITERAL:
            case TRUE: 
            case FALSE: {
                auto literal = make_unique<LiteralNode>();
                literal->type = current.type;
                literal->value = std::move(current.value);  // Move the value
                advance();
                return move(literal);
            }
                
            case IDENTIFIER: {
                auto ident = make_unique<IdentifierNode>();
                ident->name = std::move(current.value);  // Move the name
                advance();
                return move(ident);
            }
                
            case LPAREN: {
                advance();
                auto expr = parseExpression();
                expect(RPAREN);
                return expr;
            }
                
            default:
                cerr << "Syntax Error: Unexpected token " << current.value << '\n';
                exit(1);
        }
    }
};

// -------------------- OPTIMIZED MAIN --------------------
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
        cout << "Program parsed successfully!\n";
        cout << "\nAbstract Syntax Tree:\n";
        ast->print(cout);  // Pass ostream for efficiency
        
    } catch (const exception& e) {
        cerr << "Error: " << e.what() << '\n';
        return 1;
    }
    
    return 0;
}

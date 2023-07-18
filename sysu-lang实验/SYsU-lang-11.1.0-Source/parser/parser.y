%code requires
{
class Tree;
}
%{
#include "parser.hh"
#include <vector>
#include <memory>
#include <llvm/Support/JSON.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/ADT/APFloat.h>
#include <queue>
#include <map>

#define yyerror(x)                                                             \
  do {                                                                         \
    llvm::errs() << (x);                                                       \
  } while (0)

namespace {
auto llvmin = llvm::MemoryBuffer::getFileOrSTDIN("-");
auto input = llvmin.get() -> getBuffer();
auto end = input.end(), it = input.begin();
auto wk_getline(char endline = "\n"[0]) {
  auto beg = it;
  while (it != end && *it != endline)
    ++it;
  auto len = it - beg;
  if (it != end && *it == endline)
    ++it;
  return llvm::StringRef(beg, len);
}
Tree* root;
} // namespace

// 以下树结构仅供参考，你可以随意修改或定义自己喜欢的形式
class Tree{
public:
  std::string kind;
  std::string name;
  std::string value;
  std::vector<std::unique_ptr<Tree>> sons;
  // ------add--------------------
  //计算结果
  double result;
  //标记是不是固定的字符串
  int const_str_tag;
  //若为函数，则维护一个参数列表
  std::vector<std::string> function_params;
  //若为数组，维护维度
  std::vector<int> arry_dims;
  //变量的类型
  std::string type;
  //函数才维护的返回值
  std::string rtype;
  //数组的类型
  std::string atype;
  Tree(std::string kind="", std::string name="", std::string value=""): kind(kind), name(name), value(value){
    result=0;
    const_str_tag=0;
  }
  void addSon(Tree* son){ sons.emplace_back(std::unique_ptr<Tree>(son)); }
  void addSon(std::unique_ptr<Tree>&& son){ sons.emplace_back(std::move(son)); }
  llvm::json::Value toJson() const {
    llvm::json::Object tmp{
      {"kind", kind},
      {"name", name},
      {"value", value},
      {"inner", llvm::json::Array{}}
    };
    for(auto&& it: sons) tmp.get("inner")->getAsArray()->push_back(it->toJson());
    return tmp;
  }
  void print(int depth=0) const {
    yyerror("|");
    for(int i=0;i<depth;++i) yyerror(" ");
    yyerror("-"+kind+" "+name+" "+value+" "+type);
    for(auto&& it: sons)
    {
      yyerror("\n");
      it->print(depth+1);
    }
    if(!depth) yyerror("\n\n");
  }
};

//变量到节点的映射
std::map<std::string,Tree*> dictionary;

auto yylex() {
  auto tk = wk_getline();
  auto b = tk.find("'") + 1, e = tk.rfind("'");
  auto s = tk.substr(b, e - b).str(), t = tk.substr(0, tk.find(" ")).str();
  
  if (t == "numeric_constant") {
    yylval = new Tree("IntegerLiteral", "", s);
    // 检测是否是浮点数，p计数法浮点数，e计数法浮点数，如果是，转换成double
    if (s.find('.') != std::string::npos || s.find('p') != std::string::npos || s.find('e') != std::string::npos) {
        yylval->kind = "FloatingLiteral";
        yylval->value = s;
        yylval->type="double";
        llvm::StringRef str(yylval->value);
        llvm::APFloat apf(0.0);
        apf.convertFromString(str, llvm::APFloat::rmNearestTiesToEven);
        llvm::SmallString<16> Buffer;
        apf.toString(Buffer);
        yylval->value = Buffer.c_str();
        yylval->result=std::stof(yylval->value);
        return T_NUMERIC_CONSTANT;
    }
    else{
        auto v = std::stoll(s,0,0);
        auto tmp = std::to_string(v);
        auto float_v=std::stold(s);
        auto f_tmp=std::to_string(float_v);
        bool is_float=false;
        if(double(int(float_v))!=float_v) is_float=true;
        //long类型
        if (v > 2147483647) {
          yylval->kind="IntegerLiteral";
          yylval->value=tmp;
          yylval->type="long";
        }
        //int类型
        else {
          yylval->kind="IntegerLiteral";
          yylval->value=tmp;
          yylval->type="int";
        }
        yylval->result=std::stof(yylval->value);
        return T_NUMERIC_CONSTANT;    
    }
  }
  if (t == "identifier") {
    //在字典中查找看是否出现过
    if(dictionary.find(s)!=dictionary.end()){
      yylval=dictionary[s];
    }
    else{
      yylval=new Tree("id",s);
      yylval->result=0.0;
      dictionary[s]=yylval;
    }
    return T_IDENTIFIER;
  }
  if (t == "int")
    return T_INT;
  if (t == "return")
    return T_RETURN;
  if (t == "semi")
    return T_SEMI;
  if (t == "l_paren")
    return T_L_PAREN;
  if (t == "r_paren")
    return T_R_PAREN;
  if (t == "l_brace")
    return T_L_BRACE;
  if (t == "r_brace")
    return T_R_BRACE;
  // TO-DO：你需要在这里补充更多的TOKEN
  if (t == "plus")
    return T_PLUS;
  if (t == "star")
    return T_STAR;
  if (t == "minus")
    return T_MINUS;
  if (t == "equal")
    return T_EQUAL;
  if (t == "equalequal")
    return T_EQUALEQUAL;
  if (t == "slash")
    return T_SLASH;
  if (t == "less")
    return T_LESS;
  if (t == "greater")
    return T_GREATER;
  if (t == "lessequal")
    return T_LESSEQUAL;
  if (t == "greaterequal")
    return T_GREATEREQUAL;
  if (t == "exclaimequal")
    return T_EXCLAIMEQUAL;
  if (t == "percent")
    return T_PERCENT;
  if (t == "l_square")
    return T_L_SQUARE;
  if (t == "r_square")
    return T_R_SQUARE;
  if (t == "ampamp")
    return T_AMPAMP;
  if (t == "pipepipe")
    return T_PIPEPIPE;
  if (t == "comma")
    return T_COMMA;
  if (t == "const")
    return T_CONST;
  if (t == "if")
    return T_IF;
  if (t == "else")
    return T_ELSE;
  if (t == "do")
    return T_DO;
  if (t == "while")
    return T_WHILE;
  if (t == "break")
    return T_BREAK;
  if (t=="ellipsis")
    return T_ELLIPSIS;
  if (t == "continue")
    return T_CONTINUE;
  if (t == "void")
    return T_VOID;
  if (t == "char")
    return T_CHAR;
  if (t == "exclaim")
    return T_EXCLAIM;
  if (t == "float")
    return T_FLOAT;
  if (t == "long")
    return T_LONG;
  if (t == "double")
    return T_DOUBLE;
  if (t == "long long")
    return T_LONGLONG;
  if (t== "string_literal"){
    yylval=new Tree("StringLiteral","",s);
    yylval->type="char";
    return T_STRING;
  }
  return YYEOF;
}

//-----------some functions------------------------
std::unique_ptr<Tree> fill(std::unique_ptr<Tree> node, int size, int depth, int len) {
  if (node->kind == "ArraySubscriptExpr") return std::move(node);
  int startIndex = -1;
  int endIndex = 0;
  int cleanIndex = 0;
  int currentIndex = 0;
  std::unique_ptr<Tree> returnNode(new Tree(node->kind));
  while (currentIndex < node->sons.size()) {
    if (node->sons[currentIndex]->kind != "IntegerLiteral" ) {
      returnNode->addSon(fill(std::move(node->sons[currentIndex]), size, depth + 1, len));
      currentIndex++;
    } else {
      cleanIndex = currentIndex;
      startIndex = currentIndex;
      endIndex = currentIndex;
      while (endIndex < node->sons.size() && (node->sons[endIndex]->kind == "IntegerLiteral" )) {
        endIndex++;
      }
      for (int j = currentIndex; j < endIndex; j++) {
        if ((j - startIndex + 1) % size == 0 && endIndex - startIndex != size) {
          auto initExpr = new Tree("InitListExpr");
          while (startIndex <= j) {
            initExpr->addSon(std::move(node->sons[startIndex]));
            startIndex++;
          }
          returnNode->addSon(initExpr);
        }
      }
      if (endIndex - startIndex == size) {
        auto initExpr = new Tree("InitListExpr");
        while (startIndex < endIndex) {
          initExpr->addSon(std::move(node->sons[startIndex]));
          startIndex++;
        }
        returnNode->addSon(initExpr);
      }
      if (startIndex < endIndex && (endIndex - startIndex) % size != 0) {
        auto initExpr = new Tree("InitListExpr");
        auto filler = new Tree("array_filler");
        initExpr->addSon(filler);
        while (startIndex < endIndex) {
          initExpr->addSon(std::move(node->sons[startIndex]));
          startIndex++;
        }
        returnNode->addSon(initExpr);
      }
      currentIndex = endIndex;
    }
  }
  node->sons.clear();
  return std::move(returnNode);
}

//遍历树，对树中的某些节点进行类型转换，将其转换为指定的类型
std::unique_ptr<Tree> arry_ImplicitCast(std::unique_ptr<Tree> node,std::string cur_type ){
    int N=node->sons.size();
    for(int i=0;i<N;i++){
        if(node->sons[i]->kind!="IntegerLiteral" && node->sons[i]->kind!="ImplicitCastExpr" )
        node->sons[i]=arry_ImplicitCast(std::move(node->sons[i]),cur_type);
        else{
            if(node->sons[i]->type!=cur_type){
                //创建一个ImplicitCastExpr的树节点，并将当前节点作为其子节点
                auto ptr=new Tree("ImplicitCastExpr");
                ptr->addSon(std::move(node->sons[i]));
                ptr->type=cur_type;
                std::unique_ptr<Tree> k(ptr);
                node->sons[i]=std::move(k);
            }
        }
    }
    return std::move(node);
}

//遍历树，对返回值进行类型转换
std::unique_ptr<Tree> return_ImplicitCast(std::unique_ptr<Tree> node,std::string cur_type){
  if(node->sons.size()==0){return std::move(node);}
  for(int i=0;i<node->sons.size();i++){
    if(node->sons[i]->kind!="ReturnStmt") node->sons[i]=return_ImplicitCast(std::move(node->sons[i]),cur_type);
    else if((node->sons[i]->type!=cur_type) && (node->sons[i]->type!="const "+cur_type) && (node->sons[i]->type!="")){
        auto ptr=new Tree("ReturnStmt");
        ptr->type="int";
        auto temp=std::move(node->sons[i]);
        temp->kind="ImplicitCastExpr";
        temp->type="int";
        ptr->addSon(std::move(temp));
        std::unique_ptr<Tree> k(ptr);
        node->sons[i]=std::move(k);
    }
  }
  return std::move(node);
}

//----------------------------------------------------------------------
std::vector<Tree*> ConstDecl_vec;
std::vector<Tree*> Block_vec;
std::vector<Tree*> VarDecl_vec;
//CompUnit的子节点块
std::vector<Tree*> CompUnit_vec;
//暂时存储函数参数的参数栈
std::vector<Tree*> FuncFParams_vec;
std::vector<Tree*> FuncCall_P_vec;
std::vector<Tree*> ConstInitValList_vec;
std::vector<Tree*> ConstInitValList_num_vec;
std::vector<Tree*> InitValList_vec;
std::vector<Tree*> InitValList_num_vec;
std::vector<Tree*> const_variables;
std::vector<Tree*> variables;
std::vector<int> MultiSquare_vec;
std::string mizuno_str;
std::vector<Tree*> MultiLVal_vec;

int main() {
  yyparse();
  //root->print();
  llvm::outs() << root->toJson() << "\n";
}
%}
%define api.value.type { Tree* }

// TO-DO：你需要在这里补充更多的TOKEN
%token T_NUMERIC_CONSTANT
%token T_IDENTIFIER
%token T_INT
%token T_RETURN
%token T_SEMI
%token T_L_PAREN
%token T_R_PAREN
%token T_L_BRACE
%token T_R_BRACE
//新加
%token T_ELLIPSIS
%token T_STRING
%token T_PLUS
%token T_STAR
%token T_MINUS
%token T_EQUAL
%token T_EQUALEQUAL
%token T_SLASH
%token T_LESS
%token T_GREATER
%token T_LESSEQUAL
%token T_GREATEREQUAL
%token T_EXCLAIMEQUAL
%token T_identifier
%token T_PERCENT
%token T_L_SQUARE
%token T_R_SQUARE
%token T_AMPAMP
%token T_PIPEPIPE
%token T_COMMA
%token T_CONST
%token T_IF
%token T_ELSE
%token T_DO
%token T_WHILE
%token T_BREAK
%token T_CONTINUE
%token T_VOID
%token T_CHAR
%token T_LONG
%token T_DOUBLE
%token T_FLOAT
%token T_EXCLAIM
%token T_LONGLONG
%start Begin
%%

Begin: CompUnit {root = $1;};

CompUnit:GlobalDecl{
    auto ptr = new Tree("TranslationUnitDecl");
    for(int i=CompUnit_vec.size()-1;i>=0;i--){
      if(CompUnit_vec[i]->kind=="FunctionDecl"){
        ptr->addSon(CompUnit_vec[i]);
      }
      else{
        for(int j=0;j<CompUnit_vec[i]->sons.size();j++){
          ptr->addSon(std::move(CompUnit_vec[i]->sons[j]));
        }
        delete CompUnit_vec[i];
      }
    }
    CompUnit_vec.clear();
    $$ = ptr;
  };

GlobalDecl: FuncDef{CompUnit_vec.push_back($1);$$=$1;}
  |FuncDef GlobalDecl{CompUnit_vec.push_back($1);}
  |VarDecl GlobalDecl{CompUnit_vec.push_back($1);}
  |ConstDecl GlobalDecl{CompUnit_vec.push_back($1);};

FuncDef:BType T_IDENTIFIER T_L_PAREN T_R_PAREN Block {
    //printf("FuncDef:BType T_IDENTIFIER T_L_PAREN T_R_PAREN Block\n");
    auto ptr = new Tree("FunctionDecl", $2->name);
    //函数返回值
    ptr->rtype = $1->kind;
    $2->rtype = $1->kind;
    //返回值不是void，调用函数检查是否需要转换
    if(ptr->rtype!="void"){
      std::unique_ptr<Tree> tmp($5);
      tmp=return_ImplicitCast(std::move(tmp),ptr->rtype);
      ptr->addSon(std::move(tmp));
    }
    else{
      ptr->addSon($5);
    }
    $$ = ptr;
  }
  |BType T_IDENTIFIER T_L_PAREN FuncFParams T_R_PAREN Block{
    auto ptr = new Tree("FunctionDecl", $2->name);
    ptr->rtype=$1->kind;
    $2->rtype=ptr->rtype;
    for(int i=0;i<FuncFParams_vec.size();i++){
      
      ptr->addSon(FuncFParams_vec[i]);
      $2->function_params.push_back(FuncFParams_vec[i]->type);
    }
    if(ptr->rtype!="void"){
      std::unique_ptr<Tree> tmp($6);
      tmp=return_ImplicitCast(std::move(tmp),ptr->rtype);
      ptr->addSon(std::move(tmp));
    }
    else{
      ptr->addSon($6);
    }
    FuncFParams_vec.clear();
    $$=ptr;
  }
  |BType T_IDENTIFIER T_L_PAREN T_R_PAREN T_SEMI{
    //printf("FuncDef:BType T_IDENTIFIER T_L_PAREN T_R_PAREN T_SEMI\n");
    auto ptr = new Tree("FunctionDecl", $2->name);
    //函数返回值
    ptr->rtype = $2->rtype = $1->kind;
    $$ = ptr;
  }
  |BType T_IDENTIFIER T_L_PAREN FuncFParams T_R_PAREN T_SEMI{
    //printf("FuncDef:BType T_IDENTIFIER T_L_PAREN FuncFParams T_R_PAREN T_SEMI\n");
    auto ptr = new Tree("FunctionDecl", $2->name);
    ptr->rtype=$1->kind;
    $2->rtype=ptr->rtype;
    for(int i=0;i<FuncFParams_vec.size();i++){
      ptr->addSon(FuncFParams_vec[i]);
      $2->function_params.push_back(FuncFParams_vec[i]->type);
    }
    FuncFParams_vec.clear();
    $$=ptr;
  };

FuncFParams: FuncFParam{
  if($1!=NULL) {
    FuncFParams_vec.push_back($1);
    //printf("push\n");
  }
}
| FuncFParams T_COMMA FuncFParam{if($3!=NULL) FuncFParams_vec.push_back($3);}

FuncFParam: T_ELLIPSIS{$$=NULL;}
| BType T_IDENTIFIER{
  //printf("FuncFParam: BType T_IDENTIFIER\n");
  auto ptr=new Tree("ParmVarDecl",$2->name);
  ptr->type=$1->kind;
  $2->type=$1->kind;
  $$=ptr;
}
| T_CONST BType T_IDENTIFIER{
  //printf("FuncFParam: T_CONST BType T_IDENTIFIER\n");
  auto ptr=new Tree("ParmVarDecl",$2->name);
  $3->type="const "+ $2->kind;
  ptr->type="const "+ $2->kind;
  $$=ptr;
}
| BType T_IDENTIFIER MultiSquare{
  //printf("FuncFParam:BType T_IDENTIFIER MultiSquare\n");
  auto ptr=new Tree("ParmVarDecl",$2->name);
  ptr->type=$1->kind;
  $2->type=$1->kind;
  $$=ptr;
}
| T_CONST BType T_IDENTIFIER MultiSquare{
  //printf("FuncFParam:T_CONST BType T_IDENTIFIER MultiSquare\n");
  auto ptr=new Tree("ParmVarDecl",$3->name);
  $3->type="const "+ $2->kind;
  ptr->type="const "+ $2->kind;
  $$=ptr;
}
;

BType:T_VOID{
  auto ptr=new Tree("void");
  $$=ptr;
}|T_INT{
  auto ptr=new Tree("int");
  $$=ptr;
}|T_CHAR{
  auto ptr=new Tree("char");
  $$=ptr;
}|T_DOUBLE{
  auto ptr=new Tree("double");
  $$=ptr;
}|T_FLOAT{
  auto ptr=new Tree("float");
  $$=ptr;
}|T_LONG T_LONG{
  auto ptr=new Tree("long");
  $$=ptr;
}|T_LONG{
  auto ptr=new Tree("long");
  $$=ptr;
}

Block: T_L_BRACE BlockItem T_R_BRACE {
    auto ptr = new Tree("CompoundStmt");
    std::reverse(Block_vec.begin(), Block_vec.end());
    for(int i=0;i<Block_vec.size();i++){
      ptr->addSon(Block_vec[i]);
    }
    Block_vec.clear();
    $$ = ptr;
  }
  | T_L_BRACE T_R_BRACE{//空块
    auto ptr = new Tree("CompoundStmt");
    $$=ptr;
};

BlockItem:Decl{Block_vec.push_back($1);}
|Stmt{Block_vec.push_back($1);}
|Stmt BlockItem{Block_vec.push_back($1);$$=$2;} 
|Decl BlockItem{Block_vec.push_back($1);$$=$2;}

Stmt: OpenStmt{$$=$1;}|NonIfStmt{$$=$1;}

OpenStmt: T_IF T_L_PAREN Exp T_R_PAREN Stmt{
  auto ptr=new Tree("IfStmt");
  ptr->addSon($3);
  ptr->addSon($5);
  $$=ptr;
  $$->result=$3->result;
}
| T_IF T_L_PAREN Exp T_R_PAREN Stmt T_ELSE Stmt{
  auto ptr=new Tree("IfStmt");
  ptr->addSon($3);
  ptr->addSon($5);
  ptr->addSon($7);
  $$=ptr;
  $$->result=$3->result;
}


NonIfStmt:
  LVal T_EQUAL Exp T_SEMI {
    auto ptr=new Tree("BinaryOperator","","'='");
    ptr->addSon($1);
    //类型不等，进行转换
    if(($3->type!="") && ($1->type!=$3->type)){
      auto temp=new Tree("ImplicitCastExpr");
      temp->addSon($3);
      temp->type=$1->type;
      ptr->addSon(temp);
    }
    else ptr->addSon($3);
    $1->result=$3->result;
    $$=ptr;
  }
  | Exp T_SEMI{$$=$1;}
  | T_SEMI {
    auto ptr = new Tree("NullStmt");
    $$ = ptr;
  }
  | Block {$$=$1;}
  | T_RETURN Exp T_SEMI{
    auto ptr = new Tree("ReturnStmt");
    ptr->addSon($2);
    ptr->type=$2->type;
    $$ = ptr;
  }
  | T_RETURN T_SEMI{
    auto ptr = new Tree("ReturnStmt");
    $$ = ptr;
  }
  | T_WHILE T_L_PAREN Exp T_R_PAREN Stmt{
    auto ptr = new Tree("WhileStmt");
    ptr->addSon($3);
    ptr->addSon($5);
    $$ = ptr;
    $$->result=$3->result;
  }
  | T_DO Stmt T_WHILE T_L_PAREN Exp T_R_PAREN T_SEMI{
    auto ptr = new Tree("DoStmt");
    ptr->addSon($2);
    ptr->addSon($5);
    $$ = ptr;
    $$->result=$5->result;
  }
  | T_BREAK T_SEMI{
    auto ptr = new Tree("BreakStmt");
    $$ = ptr;
  }
  | T_CONTINUE T_SEMI{
    auto ptr = new Tree("ContinueStmt");
    $$ = ptr;
  }

// TO-DO：你需要在这里实现文法和树，通过测例

Decl:ConstDecl{$$=$1;}
| VarDecl{$$=$1;}

VarDecl: BType MultiVarDef T_SEMI{
  //printf("VarDecl: BType MultiVarDef T_SEMI\n");
  for(int i=0;i<variables.size();i++){
    variables[i]->type=$1->kind;
  }
  variables.clear();
  auto ptr=new Tree("DeclStmt");
  for(int i=0;i<VarDecl_vec.size();i++){
    //需要类型转换
    if(VarDecl_vec[i]->type!=$1->kind && VarDecl_vec[i]->sons.size()>=1 && VarDecl_vec[i]->type!=""){
      auto cast=new Tree("ImplicitCastExpr");
      cast->type=$1->kind;
      cast->addSon(std::move(VarDecl_vec[i]->sons[0]));
      std::unique_ptr<Tree> temp_ptr(cast);
      VarDecl_vec[i]->sons[0]=std::move(temp_ptr);
    }
    else{
      VarDecl_vec[i]->type=$1->kind;
    }
    ptr->addSon(VarDecl_vec[i]);
  }
  VarDecl_vec.clear();
  $$=ptr;
}

MultiVarDef: VarDef{ VarDecl_vec.push_back($1); }
|MultiVarDef T_COMMA VarDef{
  VarDecl_vec.push_back($3);
}

VarDef: T_IDENTIFIER T_EQUAL InitVal{
  //printf("T_IDENTIFIER T_EQUAL InitVal\n");
  $1->result=$3->result;
  auto ptr=new Tree("VarDecl",$1->name);
  variables.push_back($1);
  ptr->addSon($3);
  ptr->type=$3->type;
  $$=ptr;
}
| T_IDENTIFIER MultiSquare T_EQUAL InitVal{
  //printf("T_IDENTIFIER MultiSquare T_EQUAL InitVal\n");
  auto ptr=new Tree("VarDecl",$1->name);
  std::reverse(MultiSquare_vec.begin(),MultiSquare_vec.end());
  for(int i=0;i<MultiSquare_vec.size();i++){
    ptr->atype=ptr->atype+"["+std::to_string(MultiSquare_vec[i])+"]";
  }
  $1->atype=ptr->atype;
  $1->arry_dims=MultiSquare_vec;

  if($4!=NULL && $4->kind=="StringLiteral"){
    ptr->addSon($4);
  }
  else if($4!=NULL){
    int depth=MultiSquare_vec.back();
    std::unique_ptr<Tree> temp($4);
    temp=fill(std::move(temp),depth,0,MultiSquare_vec.size());
    ptr->addSon(std::move(temp));
  }
  MultiSquare_vec.clear();
  variables.push_back($1);
  $$=ptr;
}
| T_IDENTIFIER MultiSquare{
  //printf("T_IDENTIFIER MultiSquare\n");
  auto ptr=new Tree("VarDecl",$1->name);
  ptr->atype="";
  for(int i=MultiSquare_vec.size()-1;i>=0;i--){
    ptr->atype=ptr->atype+"["+std::to_string(MultiSquare_vec[i])+"]";
  }
  variables.push_back($1);
  $1->atype=ptr->atype;
  $1->arry_dims=MultiSquare_vec;
  $$=ptr;
  MultiSquare_vec.clear();
}
| T_IDENTIFIER{
  //printf("T_IDENTIFIER\n");
  auto ptr=new Tree("VarDecl",$1->name);
  variables.push_back($1);
  $$=ptr;
}

InitVal: Exp{$$=$1; }
| T_L_BRACE T_R_BRACE {
  auto ptr=new Tree("InitListExpr");
  auto ptr2=new Tree("array_filler");
  ptr->addSon(ptr2);
  $$=ptr;
}
| T_L_BRACE InitValList T_R_BRACE {
  std::reverse(InitValList_vec.back()->sons.begin(),InitValList_vec.back()->sons.end());
  $$=InitValList_vec.back();
  InitValList_vec.pop_back();
}

InitValList:A_Init_Val {
  auto ptr=new Tree("InitListExpr");
  ptr->addSon($1);
  InitValList_vec.push_back(ptr);
}
| A_Init_Val T_COMMA InitValList {
  InitValList_vec.back()->addSon($1);
}

A_Init_Val:T_L_BRACE InitValList T_R_BRACE{
  std::reverse(InitValList_vec.back()->sons.begin(),InitValList_vec.back()->sons.end());
  $$=InitValList_vec.back();
  InitValList_vec.pop_back();
}
| T_L_BRACE T_R_BRACE{
  auto ptr1=new Tree("InitListExpr");
  auto ptr2=new Tree("array_filler");
  ptr1->addSon(ptr2);
  $$=ptr1;
}
|Exp{$$=$1;};

ConstDecl: T_CONST BType MultiConstDef T_SEMI{
  //printf("ConstDecl:T_CONST BType MultiConstDef T_SEMI\n");
  for(int i=0;i<const_variables.size();i++){
    const_variables[i]->type=$2->kind;
  }
  const_variables.clear();
  auto ptr=new Tree("DeclStmt");
  for(int i=0;i<ConstDecl_vec.size();i++){
    //需要类型转换
    if(ConstDecl_vec[i]->type!=$2->kind && ConstDecl_vec[i]->sons.size()>=1 && ConstDecl_vec[i]->type!=""){
      auto cast=new Tree("ImplicitCastExpr");
      cast->type=$2->kind;
      cast->addSon(std::move(ConstDecl_vec[i]->sons[0]));
      std::unique_ptr<Tree> temp_ptr(cast);
      ConstDecl_vec[i]->sons[0]=std::move(temp_ptr);
    }
    ptr->addSon(ConstDecl_vec[i]);
  }
  ConstDecl_vec.clear();
  $$=ptr;
}

MultiConstDef: ConstDef{ConstDecl_vec.push_back($1);}
|MultiConstDef T_COMMA ConstDef{
  ConstDecl_vec.push_back($3);
}

ConstDef: T_IDENTIFIER T_EQUAL InitVal{
  //printf("ConstDef: T_IDENTIFIER T_EQUAL ConstInitVal\n");
  $1->result=$3->result;
  auto ptr=new Tree("VarDecl",$1->name);
  ptr->addSon($3);
  ptr->type=$3->type;
  const_variables.push_back($1);
  $$=ptr;
}
| T_IDENTIFIER MultiSquare T_EQUAL InitVal{
  //printf("ConstDef: T_IDENTIFIER MultiSquare T_EQUAL ConstInitVal\n");
  auto ptr=new Tree("VarDecl",$1->name);
  std::reverse(MultiSquare_vec.begin(),MultiSquare_vec.end());
  for(int i=0;i<MultiSquare_vec.size();i++){
    ptr->atype=ptr->atype+"["+std::to_string(MultiSquare_vec[i])+"]";
  }
  $1->atype=ptr->atype;
  $1->arry_dims=MultiSquare_vec;

  if($4!=NULL && $4->kind=="StringLiteral"){
    ptr->addSon($4);
  }
  else if($4!=NULL){
    int depth=MultiSquare_vec.back();
    std::unique_ptr<Tree> temp($4);
    temp=fill(std::move(temp),depth,0,MultiSquare_vec.size());
    ptr->addSon(std::move(temp));
  }
  else{
      auto ptr=new Tree("InitListExpr");
      auto t=new Tree("array_filler");
      ptr->addSon(t);
    }
  MultiSquare_vec.clear();
  const_variables.push_back($1);
  $$=ptr;
}
| T_IDENTIFIER{
  //printf("ConstDef:T_IDENTIFIER\n");
  auto ptr=new Tree("VarDecl",$1->name);
  const_variables.push_back($1);
  $$=ptr;
}

MultiSquare:T_L_SQUARE T_R_SQUARE{}
  |T_L_SQUARE T_R_SQUARE MultiSquare{}
  |T_L_SQUARE ConstExp T_R_SQUARE{
    int arry_dim=int($2->result);
    MultiSquare_vec.push_back(arry_dim);
  }
  |T_L_SQUARE ConstExp T_R_SQUARE MultiSquare{
    int arry_dim=int($2->result);
    MultiSquare_vec.push_back(arry_dim);
  }

  
//可能需要修改后面两条
ConstInitVal: ConstExp{ConstInitValList_num_vec.push_back($1);$$=$1;}
| T_L_BRACE T_R_BRACE {
  auto ptr=new Tree("InitListExpr");
  $$=ptr;
}
| T_L_BRACE ConstInitValList T_R_BRACE{
  auto ptr=new Tree("InitListExpr");
  for(int i=0;i<ConstInitValList_num_vec.size();i++){
    ptr->addSon(ConstInitValList_num_vec[i]);
  }
  ConstInitValList_num_vec.clear();
  $$=ptr;
}

ConstInitValList:ConstInitVal {ConstInitValList_vec.push_back($1);$$=$1;}
| ConstInitValList T_COMMA ConstInitVal {
  ConstInitValList_vec.push_back($3);
  $$=$1;
}

LVal:T_IDENTIFIER{
  //printf("LVal:T_IDENTIFIER\n");
  auto ptr=new Tree("DeclRefExpr");
  ptr->type=$1->type;
  ptr->name=$1->name;
  ptr->rtype=$1->rtype;
  ptr->atype=$1->atype;
  ptr->function_params=$1->function_params;
  ptr->result=$1->result;

  $$=ptr;
  }
  |T_IDENTIFIER MultiLVal{
    auto ptr= new Tree("DeclRefExpr",$1->name);
    MultiLVal_vec.back()->addSon(ptr);
    MultiLVal_vec.pop_back();
    $$=$2;
    $$->atype=$1->atype+" array";
    $$->type=$1->type;
    $$->name=$1->name;
  }

MultiLVal:T_L_SQUARE Exp T_R_SQUARE{
  auto p1=new Tree("ArraySubscriptExpr");
  auto p2=new Tree("ImplicitCastExpr");

  MultiLVal_vec.push_back(p2);
  p1->addSon(p2);
  p1->addSon($2);
  $$=p1;
}
| MultiLVal T_L_SQUARE Exp T_R_SQUARE {
  auto p1=new Tree("ArraySubscriptExpr");
  auto p2=new Tree("ImplicitCastExpr");

  p1->addSon(p2);
  p1->addSon($3);
  p2->addSon($1);
  $$=p1;
}
ConstExp: Exp{$$=$1;}

//----------Exps---------------------------------------------
Exp:LOrExp{$$=$1;}

RelExp: AddExp{$$=$1;}
|RelExp CompareOp AddExp{
  //if语句对符号进行分类
  if($1->type=="") $1->type=$3->type;
  if($3->type=="") $3->type=$1->type;
  if($1->type == $3->type) {
    $2->type=$1->type;
    $2->addSon($1);
    $2->addSon($3);
  }
  //需要类型转换
  //需要类型转换
  else{
    //printf("1:%s 3:%s\n",$1->type.c_str(),$3->type.c_str());
    int type_tar=0;
    if($1->type=="double" || $3->type=="double") {$2->type="double";}
    else if($1->type=="float" || $3->type=="float") {$2->type="float";}
    else if($1->type=="long" || $3->type=="long") {$2->type="long";}
    else if($1->type=="int" || $3->type=="int") {$2->type="int";}
    //前者需要转换
    if($1->type!=$2->type){
      auto son_ptr=new Tree("ImplicitCastExpr");
      son_ptr->type=$2->type;
      son_ptr->addSon($1);
      $2->addSon(son_ptr);
    }else $2->addSon($1);
    //后者转换
    if($3->type!=$2->type){
      auto son_ptr=new Tree("ImplicitCastExpr");
      son_ptr->type=$2->type;
      son_ptr->addSon($3);
      $2->addSon(son_ptr);
    }else $2->addSon($3);
  }
  $$=$2;
  if($2->value=="'>'") $$->result=($1->result > $2->result);
  if($2->value=="'>='") $$->result=($1->result >= $2->result);
  if($2->value=="'<'") $$->result=($1->result < $2->result);
  if($2->value=="'<='") $$->result=($1->result <= $2->result);
  
}

EqExp: RelExp{$$=$1;}
|EqExp EqualOp RelExp{
  $2->addSon($1);
  $2->addSon($3);
  $$=$2;
  if($2->value=="'=='") $$->result=($1->result == $2->result);
  if($2->value=="'!='") $$->result=($1->result != $2->result);
}

LAndExp:EqExp{$$=$1;}
| LAndExp T_AMPAMP EqExp{
  //不用类型转换
  auto ptr=new Tree("BinaryOperator","","'&&");
  ptr->addSon($1);
  ptr->addSon($3);
  ptr->type="int";
  $$=ptr; 
}

LOrExp: LAndExp{$$=$1;}
|LOrExp T_PIPEPIPE LAndExp{
  //不用类型转换
  auto ptr=new Tree("BinaryOperator","","'||'");
  ptr->addSon($1);
  ptr->addSon($3);
  ptr->type="int";
  $$=ptr;
}

PrimaryExp: T_L_PAREN Exp T_R_PAREN{
  auto ptr=new Tree("ParenExpr");
  //括号内的类型转换要放到括号外
  if($2->kind=="ImplicitCastExpr"){
    auto shell=new Tree("ImplicitCastExpr");
    shell->addSon(ptr);
    shell->type=$2->type;
    //将子节点转到括号下
    ptr->addSon(std::move($2->sons[0]));
    $$=shell;
  }
  else{
    $$=ptr; 
    ptr->addSon($2);
    ptr->type=$2->type;
  }
  $$->result=$2->result;
}
| Number{$$=$1;}
| Str {
  auto ptr =new Tree("StringLiteral");
  ptr->type="char";
  ptr->value='"'+mizuno_str+'"';
  mizuno_str="";
  ptr->const_str_tag=1;
  $$=ptr;
  }
|LVal{
  auto ptr=new Tree("ImplicitCastExpr");
  ptr->type=$1->type;

  ptr->addSon($1);
  $$=ptr;
  $$->result=$1->result;
  }

MulExp: UnaryExp{$$=$1;}
| MulExp MulOp UnaryExp{
  //if语句对符号进行分类
  if($1->type=="") $1->type=$3->type;
  if($3->type=="") $3->type=$1->type;
  if($1->type == $3->type) {
    $2->type=$1->type;
    $2->addSon($1);
    $2->addSon($3);
  }
  //需要类型转换
  else{
    //printf("1:%s 3:%s\n",$1->type.c_str(),$3->type.c_str());
    int type_tar=0;
    if($1->type=="double" || $3->type=="double") {$2->type="double";}
    else if($1->type=="float" || $3->type=="float") {$2->type="float";}
    else if($1->type=="long" || $3->type=="long") {$2->type="long";}
    else if($1->type=="int" || $3->type=="int") {$2->type="int";}

    //前者需要转换
    if($1->type!=$2->type){
      auto son_ptr=new Tree("ImplicitCastExpr");
      son_ptr->type=$2->type;
      son_ptr->addSon($1);
      $2->addSon(son_ptr);
    }else $2->addSon($1);
    //后者转换
    if($3->type!=$2->type){
      auto son_ptr=new Tree("ImplicitCastExpr");
      son_ptr->type=$2->type;
      son_ptr->addSon($3);
      $2->addSon(son_ptr);
    }else $2->addSon($3);
  }
  $$=$2;
  if($2->value=="'*'") $$->result=$1->result * $3->result;
  if($2->value=="'/'") $$->result=$1->result / $3->result;
}

AddExp: MulExp {$$=$1;}
| AddExp AddOp MulExp {
  //if语句对符号进行分类
  if($1->type=="") $1->type=$3->type;
  if($3->type=="") $3->type=$1->type;
  if($1->type == $3->type) {
    $2->type=$1->type;
    $2->addSon($1);
    $2->addSon($3);
  }
  //需要类型转换
  else{
    //printf("1:%s 3:%s\n",$1->type.c_str(),$3->type.c_str());
    int type_tar=0;
    if($1->type=="double" || $3->type=="double") {$2->type="double";}
    else if($1->type=="float" || $3->type=="float") {$2->type="float";}
    else if($1->type=="long" || $3->type=="long") {$2->type="long";}
    else if($1->type=="int" || $3->type=="int") {$2->type="int";}
    
    //前者需要转换
    if($1->type!=$2->type){
      auto son_ptr=new Tree("ImplicitCastExpr");
      son_ptr->type=$2->type;
      son_ptr->addSon($1);
      $2->addSon(son_ptr);
    }else $2->addSon($1);
    //后者转换
    if($3->type!=$2->type){
      auto son_ptr=new Tree("ImplicitCastExpr");
      son_ptr->type=$2->type;
      son_ptr->addSon($3);
      $2->addSon(son_ptr);
    }else $2->addSon($3);
  }
  $$=$2;
  if($2->value=="'+'")
  $$->result=$1->result+$3->result;
  else
  $$->result=$1->result-$3->result;
}

Number: T_NUMERIC_CONSTANT{$$=$1;}

Str: T_STRING{mizuno_str=mizuno_str+$1->value.substr(1,$1->value.size()-2);delete $1;}
|Str T_STRING{mizuno_str=mizuno_str+$1->value.substr(1,$1->value.size()-2);delete $1;}

UnaryExp: PrimaryExp{$$=$1;} 
| FuncCall{$$=$1;}
| UnaryOp UnaryExp{
  $1->type=$2->type;
  $1->addSon($2);
  $$=$1;
  //if 语句，对符号进行分类判断
  if($1->value=="'+'") $$->result=$2->result;
  if($1->value=="'-'") $$->result=-$2->result;
}

FuncCall:LVal T_L_PAREN T_R_PAREN {
  auto ptr=new Tree("CallExpr");
  auto imp=new Tree("ImplicitCastExpr");
  //callexpr的类型为函数的返回类型
  ptr->type=$1->rtype;
  ptr->name=$1->name;
  
  imp->addSon($1);
  ptr->addSon(imp);
  $$ =ptr;
}
| LVal T_L_PAREN FuncRParams T_R_PAREN {
  
  auto ptr=new Tree("CallExpr");
  auto imp=new Tree("ImplicitCastExpr");
  //callexpr的类型为函数的返回类型
  ptr->type=$1->rtype;
  ptr->name=$1->name;
  ptr->addSon(imp);
  imp->addSon($1);
  //printf("%d\n",$1->function_params.size());
  for(int i=0;i<FuncCall_P_vec.back()->sons.size();i++){
    if($1->function_params.size()==0){
      if(ptr->name=="sysu_putchar"){
        auto trick=new Tree("ImplicitCastExpr");
        trick->type="int";
        trick->addSon(std::move(FuncCall_P_vec.back()->sons[i]));
        ptr->addSon(trick);
      }
      else
      ptr->addSon(std::move(FuncCall_P_vec.back()->sons[i]));
      continue;
    }

    if(FuncCall_P_vec.back()->sons[i]->const_str_tag==1){
      auto str_cast=new Tree("ImplicitCastExpr");
      auto str_cast2=new Tree("ImplicitCastExpr");
      str_cast->type=FuncCall_P_vec.back()->sons[i]->type;
      str_cast2->addSon(std::move(FuncCall_P_vec.back()->sons[i]));
      str_cast->addSon(str_cast2);
      FuncCall_P_vec[i]=std::move(str_cast);
    }

    if(FuncCall_P_vec.back()->sons[i]->type!=$1->function_params[i]){
      auto temp=new Tree("ImplicitCastExpr");
      temp->type=$1->function_params[i];
      temp->addSon(std::move(FuncCall_P_vec.back()->sons[i]));
      ptr->addSon(temp);
    }
    else{
    ptr->addSon(std::move(FuncCall_P_vec.back()->sons[i]));
    }
  }
  //printf("size:%d",FuncCall_P_vec.size());
  FuncCall_P_vec.pop_back();
  $$ =ptr;
  
}

FuncRParams:Exp{
  auto call_ptr=new Tree("CallExpr");
  call_ptr->name=$1->name;
  call_ptr->addSon($1);
  FuncCall_P_vec.push_back(call_ptr);
} 
|FuncRParams T_COMMA Exp{
  FuncCall_P_vec.back()->addSon($3);
}

// ---------operator----------------------------------------------
EqualOp: T_EQUALEQUAL{
  auto ptr = new Tree("BinaryOperator","","'=='");
  $$=ptr;
} | T_EXCLAIMEQUAL {
  auto ptr = new Tree("BinaryOperator","","'!='");
  $$=ptr;
}

UnaryOp: T_PLUS{
  auto ptr = new Tree("UnaryOperator","","'+'");
  $$=ptr;
}
|T_MINUS{
  auto ptr = new Tree("UnaryOperator","","'-'");
  $$=ptr;
}
|T_EXCLAIM{
  auto ptr = new Tree("UnaryOperator","","'!'");
  $$=ptr;
}

MulOp: T_STAR{
  auto ptr = new Tree("BinaryOperator","","'*'");
  $$=ptr;
} 
| T_SLASH{
  auto ptr = new Tree("BinaryOperator","","'/'");
  $$=ptr;
} 
| T_PERCENT {
  auto ptr = new Tree("BinaryOperator","","'%'");
  $$=ptr;
}

AddOp: T_PLUS{
  auto ptr = new Tree("BinaryOperator","","'+'");
  $$=ptr;
} 
| T_MINUS {
  auto ptr = new Tree("BinaryOperator","","'-'");
  $$=ptr;
}

CompareOp: T_LESS{
  auto ptr = new Tree("BinaryOperator","","'<'");
  $$=ptr;
} 
| T_GREATER{
  auto ptr = new Tree("BinaryOperator","","'>'");
  $$=ptr;
} 
| T_LESSEQUAL{
  auto ptr = new Tree("BinaryOperator","","'<='");
  $$=ptr;
} | T_GREATEREQUAL {
  auto ptr = new Tree("BinaryOperator","","'>='");
  $$=ptr;
}
%%
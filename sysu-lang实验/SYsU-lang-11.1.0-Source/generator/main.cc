#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/JSON.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/raw_ostream.h>
#include <typeinfo>

namespace {
llvm::LLVMContext TheContext;
//包含函数和全局变量的 LLVM 结构，我们生成的所有 IR 都会储存在这里
llvm::Module TheModule("-", TheContext);
llvm::IRBuilder<> Builder(TheContext);
//符号表
std::map<llvm::StringRef, llvm::AllocaInst *> sym_table;
//记录函数
std::map<std::string, llvm::Function*> olFuncs_table;
// 定义全局变量字典
std::map<llvm::Value*, llvm::Constant*> globalVarDict; 


llvm::Value* build_BinaryOperator(const llvm::json::Object *O);
llvm::Value* build_Paren(const llvm::json::Object *O);
llvm::Value* build_ImplicitCastExpr(const llvm::json::Object *O);

llvm::Value* build_DeclRefExpr(const llvm::json::Object *O);
llvm::Value* build_referencedDecl(const llvm::json::Object *O);

llvm::Value* build_CallExpr(const llvm::json::Object *O);
llvm::FunctionType* get_func_type(const llvm::json::Object *O, bool &is_def);
llvm::Value* build_Array(const llvm::json::Object *O);
llvm::Value* build_localvar(const llvm::json::Object *O);
llvm::Value* build_InitListExpr(const llvm::json::Object *O, llvm::Value *base);
llvm::Value* build_string(const llvm::json::Object *O);
void build_Parms(const llvm::json::Object *O, llvm::Value *val);
void build_DeclStmt(const llvm::json::Object *O);
void build_CompoundStmt(const llvm::json::Object *O);
void build_return(const llvm::json::Object *O);
void build_If(const llvm::json::Object *O);
void build_While(const llvm::json::Object *O);
void build_Continue(const llvm::json::Object *O);
void build_Break(const llvm::json::Object *O);
void build_Do(const llvm::json::Object *O);
llvm::Constant* build_GlobalInitListExpr(const llvm::json::Object *O);
llvm::Constant *build_FloatLiteral(const llvm::json::Object *O);
llvm::Constant * build_IntegerLiteral(const llvm::json::Object *O);
llvm::Value* build_UnaryOperator(const llvm::json::Object *O);

struct Short_Circuit_stu {
  llvm::BasicBlock *then_block;
  llvm::BasicBlock *end_block;
  Short_Circuit_stu(llvm::BasicBlock *tthen_b, llvm::BasicBlock *tend_b) : then_block(tthen_b), end_block(tend_b) {}
  llvm::BasicBlock* then() {return then_block;}
  llvm::BasicBlock* end() {return end_block;}
};
llvm::SmallVector<Short_Circuit_stu,16> short_circuit_stack;


struct start_end_stu{
  llvm::BasicBlock *start;
  llvm::BasicBlock *end;
  start_end_stu(llvm::BasicBlock *start, llvm::BasicBlock *end) : start(start), end(end) {}
  llvm::BasicBlock* getstart() { return start; }
  llvm::BasicBlock* getend() { return end; }
};

llvm::SmallVector<start_end_stu, 100> pos_Stack;

void printType(llvm::Type *type) {
  llvm::raw_ostream &out = llvm::outs();
  type->print(out);
  out << "\n";
}
void printValueName(llvm::Value *val) {
  llvm::StringRef name = val->getName();
  llvm::outs() << "Value name: " << name << "\n";
}


int glo_flag=0;
int oldtag=0;
//#########################################################################################################################
//#########################################################################################################################

llvm::Type* get_type(const llvm::json::Object *O, int &flag){
  auto qual_type = *(O->getString("qualType"));
   //函数指针类型
  auto pt = qual_type.find("(*)");
  if (pt != std::string::npos) { 
    pt = qual_type.substr(pt+3).find('(');
    if (pt != std::string::npos)
      return llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), llvm::Type::getVoidTy(TheContext));
  }
  //split方法返回一个 pair<StringRef, StringRef>
  auto type_pair = qual_type.split(' ');
  //判断是不是常量
  if (type_pair.first == "const") {
    type_pair = type_pair.second.rsplit(' ');
    flag=1;
  } else {
    type_pair = qual_type.rsplit(' ');
    flag=0;
  }
  //printf("%s\n",typeid(qual_type).name());
  //判断是不是数组，rfind函数返回查找的字符最后一次的位置
  int left_square = type_pair.second.rfind('[');
  int right_square = type_pair.second.rfind(']');
  llvm::Type* type=llvm::Type::getInt32Ty(TheContext);
  //非数组类型
  if (left_square == std::string::npos){
    if(type_pair.first=="int") type=llvm::Type::getInt32Ty(TheContext);
    else if(type_pair.first=="char") type=llvm::Type::getInt8Ty(TheContext);
    else if(type_pair.first=="long") type=llvm::Type::getInt64Ty(TheContext);
    else if(type_pair.first=="unsigned") type=llvm::Type::getInt32Ty(TheContext);
    else if(type_pair.first=="float") type=llvm::Type::getFloatTy(TheContext);
    else if(type_pair.first=="double") type=llvm::Type::getDoubleTy(TheContext);
  }
  else{//数组类型
    long length;
    // 获取数组的大小
    type_pair.second.substr(left_square+1, right_square-left_square-1).getAsInteger(0, length);
    llvm::ArrayType* atype;
    if(type_pair.first=="int"){
      atype=llvm::ArrayType::get(llvm::Type::getInt32Ty(TheContext), length);
    }
    else if(type_pair.first=="char"){
      atype=llvm::ArrayType::get(llvm::Type::getInt8Ty(TheContext), length);
    }
    else if(type_pair.first=="float"){
      atype=llvm::ArrayType::get(llvm::Type::getFloatTy(TheContext), length);
    }
    else if(type_pair.first=="double"){
      atype=llvm::ArrayType::get(llvm::Type::getDoubleTy(TheContext), length);
    }
    
    auto tmp=type_pair.second.substr(0,left_square);
    left_square=tmp.rfind('[',left_square);
    right_square=tmp.rfind(']',right_square);
    while(left_square!=std::string::npos) {
      // 逐级创建多维数组类型
      tmp.substr(left_square+1,right_square-left_square-1).getAsInteger(0,length);
      atype=llvm::ArrayType::get(atype,length);
      tmp=tmp.substr(0,left_square);
      left_square=tmp.rfind('[',left_square);
      right_square=tmp.rfind(']',right_square);
    }
    type=atype;
  }
  //指针
  pt=qual_type.find('*');
  while(pt!= std::string::npos) {
    type=llvm::PointerType::get(type,0);
    qual_type=qual_type.substr(pt+1);
    pt=qual_type.find('*');
  }
  return type;
}

//获取函数类型
llvm::FunctionType* get_func_type(const llvm::json::Object *O, bool &is_def){
  //获取函数返回类型
  auto type_v=O->getObject("type")->getString("qualType")->split(' ').first;
  int para=0;
  is_def=false;
  llvm::Type *type;
  //选择对应的类型
  if (type_v=="int")type = llvm::Type::getInt32Ty(TheContext);
  else if (type_v=="long")type = llvm::Type::getInt64Ty(TheContext); 
  else if (type_v=="void")type = llvm::Type::getVoidTy(TheContext);
  else if (type_v=="float")type = llvm::Type::getFloatTy(TheContext);
  else if (type_v=="double")type = llvm::Type::getDoubleTy(TheContext);
  
  //printf("tag1\n");

  std::vector<llvm::Type*> vec;
  if (auto inner_point=O->getArray("inner")){
    for (const auto & i : *inner_point){
      if (auto obj=i.getAsObject()){
        if (*(obj->getString("kind"))=="ParmVarDecl")
          {
            //printf("tag2\n");
            vec.emplace_back(get_type(obj->getObject("type"), para));
            //printf("tag3\n");
          }
        else if (*(obj->getString("kind"))=="CompoundStmt")
          is_def = true;
        //为函数定义
      }
    }
  }
  return llvm::FunctionType::get(type,vec,0);
}


llvm::Value* build_string(const llvm::json::Object *O){
  std::vector<llvm::Constant*> context;
  int flag=0;
  auto stype=llvm::cast<llvm::ArrayType>(get_type(O->getObject("type"),flag));
  auto name=O->getString("id");
  llvm::GlobalVariable *str=nullptr;
  if (flag==0)
  str = new llvm::GlobalVariable(TheModule, stype, false,
              llvm::GlobalValue::PrivateLinkage, 0, *name);
  else
  str = new llvm::GlobalVariable(TheModule, stype, true,
              llvm::GlobalValue::PrivateLinkage, 0, *name);
  auto strs=O->getString("value")->str();
  for(int i=1;i<strs.size()-1;++i){
    char tmp=strs[i];
    if(strs[i]=='\\'){
      i=i+1;
      if(strs[i]=='n')tmp='\n';
      else if(strs[i]=='\\')tmp='\\';
      else if(strs[i]=='\"')tmp='\"';
    }
    context.emplace_back(llvm::ConstantInt::get(TheContext,llvm::APInt(8,tmp,false)));
  }
  while(context.size()<stype->getArrayNumElements()){
    context.emplace_back(llvm::ConstantInt::get(TheContext, llvm::APInt(8, 0, false)));
  }
  llvm::errs()<<context.size()<<" hahe\n";
  str->setInitializer(llvm::ConstantArray::get(stype,context));
  return str;
}



//构建初始化列表表达式InitListExpr
llvm::Value* build_InitListExpr(const llvm::json::Object *O, llvm::Value *base){
  //printf("build_InitListExpr\n");
  int flag,tag=0;
  auto type1 = O->getObject("type");
  llvm::Type* array_type=get_type(type1,flag);
  auto inner_point=O->getArray("inner");
  if(inner_point==nullptr)inner_point=O->getArray("array_filler");
  if(inner_point!=nullptr){
    for(const auto & i:*inner_point){
      if(auto obj=i.getAsObject()){
        if(auto array_kind=obj->getString("kind")){
          if(*array_kind=="ImplicitValueInitExpr")continue;
          // 表示索引的整数值
          llvm::Value* incics=llvm::ConstantInt::get(TheContext, llvm::APInt(32,tag++));
          // 用于存储元素的位置
          llvm::Value* store_pos=Builder.CreateInBoundsGEP(array_type,base,{llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0)) ,incics});
          if(*array_kind=="InitListExpr")build_InitListExpr(obj,store_pos);
          else if(*array_kind=="IntegerLiteral"){
            llvm::Value * value=build_IntegerLiteral(obj);
            Builder.CreateStore(value,store_pos);
          }
          else if(*array_kind=="FloatingLiteral"){
            llvm::Value * value=build_FloatLiteral(obj);
            Builder.CreateStore(value,store_pos);
          }
          else if(*array_kind== "BinaryOperator"){
            llvm::Value *value= build_BinaryOperator(obj);
            Builder.CreateStore(value,store_pos);
          }
          else if(*array_kind=="CallExpr"){
            llvm::Value *value=build_CallExpr(obj);
            Builder.CreateStore(value,store_pos);
          }
          else if(*array_kind=="ImplicitCastExpr"){
            llvm::Value* value=build_ImplicitCastExpr(obj);
            Builder.CreateStore(value,store_pos);
          }
          else if(*array_kind!="ImplicitValueInitExpr")
            printf("error(build_InitListExpr)\n");
        }
      }
    }
  }
  return nullptr;
}

llvm::Value* build_Array(const llvm::json::Object *O){
  //printf("build_Array\n");
  if(auto inner_point=O->getArray("inner")){
    auto arry_obj=inner_point->front().getAsObject();
    llvm::Value* array;
    if(auto arry_kind=arry_obj->getString("kind")){
      if(*arry_kind=="ImplicitCastExpr")array=build_ImplicitCastExpr(arry_obj);
      else if(*arry_kind=="BinaryOperator")array=build_BinaryOperator(arry_obj);
      else printf("error build_Array_1\n");
    }
    auto idx_obj=inner_point->back().getAsObject();
    llvm::Value* arry_idx;
    if (auto idx_kind = idx_obj->getString("kind")){
      if(*idx_kind=="IntegerLiteral")arry_idx=build_IntegerLiteral(idx_obj);
      else if(*idx_kind=="FloatingLiteral")arry_idx=build_FloatLiteral(idx_obj);
      else if(*idx_kind=="BinaryOperator")arry_idx=build_BinaryOperator(idx_obj);
      else if(*idx_kind=="CallExpr")arry_idx=build_CallExpr(idx_obj);
      else if(*idx_kind=="ImplicitCastExpr")arry_idx=build_ImplicitCastExpr(idx_obj);
      else printf("error build_Array_2\n");
    }
    // 创建一个 SmallVector 来存储 GEP 指令的索引
    llvm::SmallVector<llvm::Value*,3> smvect_idics{llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0))};
    // 获取地址的指针元素类型
    llvm::Type *atype=array->getType()->getPointerElementType();
    // 检查类型是否为指针类型
    if (atype->isPointerTy()) {
        smvect_idics.pop_back();
        atype = atype->getPointerElementType();
        // 创建一个加载指令来获取地址中存储的值
        array = Builder.CreateLoad(array);
    }
    // 将索引添加到索引向量中
    smvect_idics.push_back(arry_idx);
    // 创建一个 InBoundsGEP 指令来计算基地址
    llvm::Value* address_base = Builder.CreateInBoundsGEP(atype, array, smvect_idics);
    // 返回基地址
    return address_base;
  }
  //printf("error build_Array_3\n");
  return nullptr;
}


llvm::Value* build_DeclRefExpr(const llvm::json::Object *O){
  //printf("build_DeclRefExpr\n");
  if (auto Ref=O->getObject("referencedDecl")){
    return build_referencedDecl(Ref);
  }
  else{
    return nullptr;
  }
}

llvm::Value* build_referencedDecl(const llvm::json::Object *O){
  //printf("build_referencedDecl\n");
  auto name=O->getString("name");
  auto theid=O->getString("id");
  // 局部变量，找到了就返回
  if (sym_table.find(*theid)!=sym_table.end()){
    //printf("local\n");
    return sym_table[*theid];
  }  
  // 全局变量
  llvm::GlobalVariable* Global=TheModule.getNamedGlobal(*theid);  
  if (Global!= nullptr){
    //printf("global\n");
    return Global;
  }  
  // 重载函数
  auto ftype=O->getObject("type")->getString("qualType");
  auto onlyname=((*name) + (*ftype)).str();  
  if (olFuncs_table.find(onlyname)!=olFuncs_table.end())  
    return olFuncs_table[onlyname];  
  // 检查引用的声明是否是一个函数
  auto function=TheModule.getFunction(*name);
  if (function!=nullptr){
    return function; 
  }
  //找不到
  //printf("error(ReferencedDecl):can't find: %s", name->str().c_str());
  return nullptr; 
}

//隐式类型转换
llvm::Value* build_ImplicitCastExpr(const llvm::json::Object *O){
  //printf("build_ImplicitCastExpr\n");
  llvm::Value *val;
  int flag;
  auto first_type=O->getObject("type");
  llvm::Type * the_type=get_type(first_type,flag);
  if (auto inner_point=O->getArray("inner")){ 
    for (const auto & i : *inner_point){ 
      if (auto obj=i.getAsObject()){
        if (auto kind=obj->getString("kind")){
          if(*kind=="IntegerLiteral"){
            val=build_IntegerLiteral(obj);
            // 若类型不匹配，则进行类型扩展
            val = build_IntegerLiteral(obj);
            //printf("Builder.CreateZExt\n");
            if (val->getType() != the_type && the_type->isFloatingPointTy()) {
              llvm::Type* targetType = the_type;
              val = Builder.CreateSIToFP(val, llvm::Type::getFloatTy(TheContext));
            }
            else if(val->getType()!=the_type)val=Builder.CreateZExt(val, the_type);
          }
          else if(*kind=="FloatingLiteral"){
            val=build_FloatLiteral(obj);
            // 若类型不匹配，则进行类型扩展
            if (val->getType() != the_type && the_type->isIntegerTy()) {
              llvm::Type* targetType = the_type;
              if (the_type->isIntegerTy(1)) {
                // 如果目标类型为i1（布尔类型），使用CreateFPToUI将浮点数转换为无符号整数类型
                val = Builder.CreateFPToUI(val, targetType);
              } 
              else {
                // 否则使用CreateFPToSI将浮点数转换为有符号整数类型
                //printf("CreateFPToSI 1\n");
                val = Builder.CreateFPToSI(val, targetType);
              }
            }
            else if(val->getType()!=the_type)val=Builder.CreateFPExt(val, the_type);
          } 
          else if(*kind=="BinaryOperator"){
            //printf("*kind== BinaryOperator\n");
            val=build_BinaryOperator(obj);
            //printType(val->getType());
            //printType(the_type);

            //llvm::outs() << "Value: ";
            //val->print(llvm::outs());
            //printf("\n");

            if (val->getType() != the_type && the_type->isIntegerTy()){
              llvm::Type* targetType = the_type;
              //printf("CreateFPToSI 2\n");
              val = Builder.CreateFPToSI(val, targetType);
              //printf("change...\n");
            }
            else if(val->getType() != the_type && the_type->isFloatingPointTy()){
              llvm::Type* targetType = the_type;
              val = Builder.CreateSIToFP(val,llvm::Type::getFloatTy(TheContext));
            }

            //llvm::outs() << "Value: ";
            //val->print(llvm::outs());
            //printf("\n");

          }
          else if(*kind=="UnaryOperator")val=build_UnaryOperator(obj);
          else if(*kind=="ImplicitCastExpr")val=build_ImplicitCastExpr(obj);
          else if(*kind=="CallExpr")val=build_CallExpr(obj);
          else if(*kind=="ParenExpr")val=build_Paren(obj);
          else if(*kind=="DeclRefExpr"){
            val=build_DeclRefExpr(obj);
            //printf("out\n");
          }
          else if(*kind=="ArraySubscriptExpr")val=build_Array(obj);
          else if (*kind == "StringLiteral"){
            auto zero = llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0));
            val = Builder.CreateInBoundsGEP(build_string(obj), {zero, zero});
          } 
          else llvm::errs()<<"error(build_ImplicitCastExpr)\n";
        }
      }
    }
    //printf("out2\n");
    // 如果目标类型是函数类型，则返回结果
    //printf("the_type->isFunctionTy():%d\n",the_type->isFunctionTy());
    if(the_type->isFunctionTy()){
      //printf("the_type->isFunctionTy()\n"); 
      return val;
    }
    else if(the_type->isPointerTy()||the_type->isArrayTy()){
      //printf("isPointerTy\n"); 
      return val;
    }
    else if(*(O->getString("valueCategory"))=="rvalue" && val->getType()->isPointerTy()){
      //printf("return Builder.CreateLoad(val)\n");
      //printType(val->getType());
      //printValueName(val);
      //printf("glo_flag:%d\n",glo_flag);
      if(glo_flag==1){
        // 创建全局变量
        return val;
      }
      else return Builder.CreateLoad(val);
    }
    else {
      //printf("return else\n"); 
      return val;
    }
  }
  //printf("return nullptr\n");
  return nullptr;
}

llvm::Value* build_CallExpr(const llvm::json::Object *O) {
  //printf("build_CallExpr\n");
  if (auto inner_point = O->getArray("inner")) {
    //printf("h2\n");
    auto var=inner_point->begin();
    llvm::Function *function_p;
    if (auto obj=var->getAsObject()){
      if (auto kind=obj->getString("kind")){
        if (*kind=="ImplicitCastExpr"){
          //printf("h3\n");
          function_p = llvm::cast<llvm::Function>(build_ImplicitCastExpr(obj));
          //printf("h4\n");
        }
      }
    }
    var = std::next(var);
    //存储函数参数的向量
    std::vector<llvm::Value*> args;
    auto arg_var=function_p->arg_begin();
    //printf("enter while?\n");
    while(var!=inner_point->end()){
      //printf("while\n");
      llvm::Value *arg;
      if(auto obj = var->getAsObject()){
        if(auto kind=obj->getString("kind")){
          if(*kind=="ImplicitCastExpr")arg=build_ImplicitCastExpr(obj);
          else if(*kind=="IntegerLiteral"){arg=build_IntegerLiteral(obj);}
          else if(*kind=="FloatingLiteral")arg=build_FloatLiteral(obj);
          else if(*kind=="UnaryOperator")arg=build_UnaryOperator(obj);
          else if(*kind=="BinaryOperator")arg=build_BinaryOperator(obj);
          else if(*kind=="CallExpr"){arg=build_CallExpr(obj);}
          else llvm::errs()<<"error(CallExpr)\n";
        }
      }
      while(arg->getType()->isPointerTy()&&arg->getType()!=arg_var->getType()&&arg->getType()->getPointerElementType()->isArrayTy()) {
        auto ConstZero = llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0));
        arg = Builder.CreateInBoundsGEP(arg, {ConstZero, ConstZero});
      }
      while(arg->getType()->isPointerTy()&&arg->getType()!=arg_var->getType()&&arg->getType()->getPointerElementType()->isPointerTy()){
        arg = Builder.CreateLoad(arg);
      }

      if (arg->getType() != arg_var->getType()&& arg_var->getType()->isIntegerTy()&&arg->getType()->isFloatingPointTy()){
        llvm::Type* targetType = arg_var->getType();
        //printf("CreateFPToSI 3\n");
        //printType(arg->getType());
        //printType(arg_var->getType());
        arg = Builder.CreateFPToSI(arg, llvm::Type::getInt32Ty(TheContext));
      }
      else if(arg->getType() != arg_var->getType()&& arg_var->getType()->isFloatingPointTy()){
        llvm::Type* targetType = arg_var->getType();
        arg = Builder.CreateSIToFP(arg, llvm::Type::getFloatTy(TheContext));
      }
      else if(arg->getType()!=arg_var->getType())
        arg = Builder.CreateZExt(arg, arg_var->getType());
      
      
      //llvm::outs() << "arg value: ";
      //arg->print(llvm::outs());
      //printf("\n");



      args.emplace_back(arg);// 将处理后的参数添加到参数向量中
      var=std::next(var);
      arg_var=std::next(arg_var);
    }
    //创建函数调用
    //printf("return Builder.CreateCall(function_p, args);\n");
    return Builder.CreateCall(function_p, args);
  }
  //printf("error(build_CallExpr)\n");
  return nullptr;
}


// 返回语句
void build_return(const llvm::json::Object *O){
  //printf("build_return\n");
  // 函数的返回类型
  auto *ReturnType = Builder.GetInsertBlock()->getParent()->getReturnType();
  llvm::Value *the_val; // 返回值
  // 如果存在内部数组
  if (auto inner_point = O->getArray("inner")) {
    for (const auto &i : *inner_point) {
      if (auto obj = i.getAsObject()) {
        if (auto kind = obj->getString("kind")) {
          // 根据不同的表达式类型构建返回值
          if (*kind=="IntegerLiteral")the_val=build_IntegerLiteral(obj);
          else if(*kind=="FloatingLiteral")the_val=build_FloatLiteral(obj);
          else if (*kind=="UnaryOperator"){the_val=build_UnaryOperator(obj);}
          else if (*kind=="BinaryOperator")the_val=build_BinaryOperator(obj);
          else if (*kind=="ParenExpr")the_val=build_Paren(obj);
          else if (*kind=="ImplicitCastExpr")the_val=build_ImplicitCastExpr(obj);
          else if (*kind=="ArraySubscriptExpr")the_val=build_Array(obj);
          else if (*kind=="CallExpr")the_val=build_CallExpr(obj);
        }
      }
    }
    // 如果返回值类型与函数的返回类型不匹配，则进行类型转换
    //printType(the_val->getType());
    //printType(ReturnType);
    

    if (the_val->getType() != ReturnType && ReturnType->isFloatingPointTy()&&the_val->getType()->isIntegerTy()) {
      llvm::Type* targetType = ReturnType;
      the_val = Builder.CreateSIToFP(the_val,llvm::Type::getFloatTy(TheContext));
    }
    else if(the_val->getType() != ReturnType && ReturnType->isIntegerTy()&&the_val->getType()->isFloatingPointTy()) {
      //printf("c2\n");
      llvm::Type* targetType = ReturnType;
      //printf("CreateFPToSI 4\n");
      the_val = Builder.CreateFPToSI(the_val, targetType);
    }
    else if(the_val->getType()!=ReturnType){
      the_val = Builder.CreateSExtOrTrunc(the_val, ReturnType);
    }

    //llvm::outs() << "return value: ";
    //the_val->print(llvm::outs());
    //printf("\n");
    // 创建返回语句
    Builder.CreateRet(the_val);
  } 
  else Builder.CreateRetVoid();// 创建空返回语句（void 返回类型）
}

//参数
void build_Parms(const llvm::json::Object *O, llvm::Value *val){
  //printf("build_Parms\n");
  int flag;
  auto parm_name=O->getString("id");  // 获取参数名称
  auto parm_type=O->getObject("type");  // 获取参数类型信息
  llvm::Type *type=get_type(parm_type,flag); // 获取参数类型
  llvm::AllocaInst *a_ptr=Builder.CreateAlloca(type, 0, *parm_name);
  Builder.CreateStore(val,a_ptr);
  sym_table.insert({*parm_name,a_ptr});// 插入符号表中
}

void build_DeclStmt(const llvm::json::Object *O){
  //printf("build_DeclStmt\n");
  if (auto inner_point = O->getArray("inner")) {
    for (auto &i : *inner_point) {
      if (auto obj = i.getAsObject()){
        if (auto kind = obj->getString("kind")){
          if(*kind=="VarDecl")build_localvar(obj);
        }
      }
    }
  }
  return;
}

llvm::Value* build_localvar(const llvm::json::Object *O){
  //printf("build_localvar\n");
  auto name=O->getString("id");// 作为变量名
  int flag=0;
  llvm::Type* type = llvm::Type::getInt32Ty(TheContext);
  if (auto type_oject = O->getObject("type"))
    type = get_type(type_oject, flag);
  // 获取当前插入点所在的基本块
  llvm::BasicBlock *Curr_B = Builder.GetInsertBlock();
  // 如果当前块不是最初始的块(如果不是初始块，函数返回一个终结指令（Terminator）)
  if (Curr_B->getParent()->getEntryBlock().getTerminator() != nullptr)
    // 将插入点设置为最初始块的终结符之后
    Builder.SetInsertPoint(Curr_B->getParent()->getEntryBlock().getTerminator());
  llvm::AllocaInst * a_ptr = Builder.CreateAlloca(type, 0, *name);
  // 插入符号表
  sym_table.insert({*name, a_ptr});
  // 插入点恢复到之前的位置
  Builder.SetInsertPoint(Curr_B);
  // 计算变量的总大小
  unsigned size=1;
  llvm::Type * temp_type=type;
  while (temp_type->isArrayTy()) {
    size *= temp_type->getArrayNumElements();
    temp_type = temp_type->getArrayElementType();
  }
  size *= temp_type->getIntegerBitWidth() / 8;
  //若为数组类型，进行初始化
  if (type->isArrayTy()) 
    Builder.CreateMemSet(a_ptr, llvm::ConstantInt::get(TheContext, llvm::APInt(8, 0)),
      llvm::ConstantInt::get(TheContext, llvm::APInt(32, size)), llvm::MaybeAlign(8));
  //分类
  if (auto inner_point = O->getArray("inner")) {
    for (auto &i : *inner_point) {
      if (auto obj = i.getAsObject()){
        if (auto kind = obj->getString("kind")){
          if(*kind=="IntegerLiteral"){
            llvm::Constant* cons=build_IntegerLiteral(obj);
            Builder.CreateStore(cons,a_ptr);
          }
          else if(*kind=="FloatingLiteral"){
            llvm::Constant* cons=build_FloatLiteral(obj);
            Builder.CreateStore(cons,a_ptr);
          }
          else if(*kind=="UnaryOperator"){
            llvm::Value *cons=build_UnaryOperator(obj);
            Builder.CreateStore(cons,a_ptr);
          }
          else if (*kind=="StringLiteral"){
            auto zero = llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0));
            auto cons = Builder.CreateInBoundsGEP(build_string(obj),{zero,zero});
            Builder.CreateMemCpy(a_ptr,llvm::MaybeAlign(8),cons,llvm::MaybeAlign(8),size);
          }
          else if(*kind=="BinaryOperator"){
            llvm::Value *cons=build_BinaryOperator(obj);
            Builder.CreateStore(cons,a_ptr);
          }
          else if(*kind=="CallExpr"){
            llvm::Value *cons=build_CallExpr(obj);

            //llvm::outs() << "value: ";
            //cons->print(llvm::outs());
            //printf("\n");

            Builder.CreateStore(cons,a_ptr);
          }
          else if(*kind=="ImplicitCastExpr"){
            llvm::Value *cons=build_ImplicitCastExpr(obj);
            Builder.CreateStore(cons,a_ptr);
          }
          else if(*kind=="InitListExpr"){
            build_InitListExpr(obj,a_ptr);
          }
          else if(*kind=="ParenExpr"){
            llvm::Value *cons=build_Paren(obj);
            Builder.CreateStore(cons,a_ptr);
          }
          else{return nullptr;}
        }
      }
    }
  }
  return nullptr;
}


llvm::Function *buildFunctionDecl(const llvm::json::Object *O) {
  //printf("buildFunctionDecl\n");
  // 首先，检查之前的声明中是否已存在该函数。
  auto TheName = O->get("name")->getAsString()->str();
  llvm::Function *TheFunction = TheModule.getFunction(TheName);
  bool flag_def=false;
  llvm::FunctionType *func_type=get_func_type(O,flag_def);
  //如果函数不存在，则创建一个新的函数声明
  if (!TheFunction)
    TheFunction = llvm::Function::Create(
        func_type,
        llvm::Function::ExternalLinkage, TheName, &TheModule);
  else{
    // 根据返回类型添加函数重载后缀
    std::string new_thename=TheName;
    if(func_type->getReturnType()==llvm::Type::getInt32Ty(TheContext))
    new_thename+=std::string("_ri");
    else if(func_type->getReturnType()==llvm::Type::getVoidTy(TheContext))
    new_thename+=std::string("_rv");
    else if(func_type->getReturnType()==llvm::Type::getInt64Ty(TheContext))
    new_thename+=std::string("_rl");
    else if(func_type->getReturnType()==llvm::Type::getFloatTy(TheContext))
    new_thename+=std::string("_rf");
    else llvm::errs() << "error(buildFunctionDecl1)\n";
    //在 LLVM 中，函数类型由返回类型和参数类型组成。
    //每个参数类型表示函数接受的参数的类型。
    //getNumParams() 函数用于获取函数类型中参数的数量
    int num=func_type->getNumParams();
    for(int i=0;i<num;i++){
      if (func_type->getParamType(i)==llvm::Type::getInt32Ty(TheContext))
      new_thename += std::string("_i");//int
      else if (func_type->getParamType(i) == llvm::Type::getInt64Ty(TheContext))
      new_thename += std::string("_l");//long
      else if (func_type->getParamType(i) == llvm::Type::getInt32PtrTy(TheContext))
      new_thename += std::string("_ip");//int指针类型
      else if (func_type->getParamType(i) == llvm::Type::getFloatTy(TheContext))
      new_thename += std::string("_f");//浮点
      else if (func_type->getParamType(i)->isArrayTy())
      new_thename += std::string("_a");//数组
      else if (func_type->getParamType(i)->isPointerTy())
      new_thename += std::string("_p");//指针
      else llvm::errs()<<"error(buildFunctionDecl2)\n";
    }
    auto typestr = O->getObject("type")->getString("qualType")->str();
    TheFunction = llvm::Function::Create(func_type,
        llvm::Function::ExternalLinkage, new_thename, &TheModule);
    olFuncs_table.insert({TheName+typestr, TheFunction});
  }
  if(!TheFunction)return nullptr;
  // 创建一个新的基本块以开始插入指令。
  // 在函数TheFunction中创建名为entry的基本块
  if(auto inner_point=O->getArray("inner")){
    if(!flag_def)return nullptr;
    auto BB = llvm::BasicBlock::Create(TheContext, "entry", TheFunction);
    Builder.SetInsertPoint(BB);

    auto funargs = TheFunction->arg_begin();
    for (const auto & i : *inner_point){
      if (auto obj=i.getAsObject()){
        if (auto kind=obj->getString("kind")) {
          if (*kind=="CompoundStmt")build_CompoundStmt(obj);
          else if (*kind=="ParmVarDecl") {
            llvm::Value* av = &(*funargs);
            funargs = std::next(funargs);
            build_Parms(obj, av);
          }
        }
      }
    }
    //void
    if (Builder.GetInsertBlock()->getTerminator() == nullptr){
      if (TheFunction->getReturnType()->isVoidTy())Builder.CreateRetVoid();
      else{
      Builder.CreateRet(llvm::Constant::getNullValue(TheFunction->getReturnType()));
      }
    }
    llvm::verifyFunction(*TheFunction);
    return TheFunction;
  }
  // Error reading body, remove function.
  //TheFunction->eraseFromParent();
  return nullptr;
}


llvm::Value* build_BinaryOperator(const llvm::json::Object *O){
  //printf("build_BinaryOperator\n");
  int flag;
  auto type1 = O->getObject("type");
  llvm::Type* re_type=get_type(type1,flag);


  llvm::BasicBlock *BBlock;
  llvm::Value *left_re;
  llvm::Value *right_re;
  auto sign=*(O->getString("opcode"));
  if(auto inner_array=O->getArray("inner")){
    
    if(sign=="||"){
      BBlock= llvm::BasicBlock::Create(TheContext,"orR",Builder.GetInsertBlock()->getParent());
      short_circuit_stack.push_back({short_circuit_stack.back().then(),BBlock});
    }
    else if(sign=="&&"){
      BBlock = llvm::BasicBlock::Create(TheContext,"andR",Builder.GetInsertBlock()->getParent());
      short_circuit_stack.push_back({BBlock, short_circuit_stack.back().end()});
    }


    auto obj=inner_array->front().getAsObject();
    if(*(obj->getString("kind"))=="DeclRefExpr"){
      left_re=build_DeclRefExpr(obj);
      if(sign!="=")left_re=Builder.CreateLoad(left_re);
    }
    else if(*(obj->getString("kind"))=="IntegerLiteral"){left_re=build_IntegerLiteral(obj);}
    else if(*(obj->getString("kind"))=="FloatingLiteral"){left_re=build_FloatLiteral(obj);}
    else if(*(obj->getString("kind"))=="UnaryOperator"){left_re=build_UnaryOperator(obj);}
    else if(*(obj->getString("kind"))=="BinaryOperator"){left_re=build_BinaryOperator(obj);}
    else if(*(obj->getString("kind"))=="ImplicitCastExpr"){
      left_re=build_ImplicitCastExpr(obj);
      //printf("left ok\n");
    }
    else if(*(obj->getString("kind"))=="ArraySubscriptExpr"){left_re=build_Array(obj);}
    else if(*(obj->getString("kind"))=="CallExpr"){left_re=build_CallExpr(obj);}
    else if(*(obj->getString("kind"))=="ParenExpr"){left_re=build_Paren(obj);}
    else llvm::errs()<<"error(build_BinaryOperator_1)\n";

    //printType(left_re->getType());

    if(glo_flag==1 && globalVarDict.find(left_re)!=globalVarDict.end()){
      left_re=globalVarDict[left_re];
      //printf("left_tag\n");
    }
    //else printf("no left_tag\n");
    
    //rintf("left type:\n");
    //printType(left_re->getType());

    // cal short_circuit on the left
    if ((sign=="||")||(sign=="&&")){
      //整数
      if (left_re->getType()->isIntegerTy()){
        if (left_re->getType()!=llvm::Type::getInt1Ty(TheContext))
          left_re = Builder.CreateICmpNE(left_re, llvm::ConstantInt::get(TheContext, llvm::APInt(32, "0", 10)));
      } 
      //浮点
      else if(left_re->getType()->isFloatingPointTy()) {
        //printf("left_re->getType()->isFloatingPointTy()\n");
        left_re=Builder.CreateFCmpONE(left_re, llvm::ConstantFP::get(left_re->getType(), 0.0));
      }
      Builder.CreateCondBr(left_re,short_circuit_stack.back().then(),short_circuit_stack.back().end());
      BBlock->moveAfter(Builder.GetInsertBlock());
      Builder.SetInsertPoint(BBlock);
      short_circuit_stack.pop_back();
    }

    //llvm::outs() << "left value: ";
    //left_re->print(llvm::outs());
    //printf("\n");
    

    auto obj_2=inner_array->back().getAsObject();
    if(*(obj_2->getString("kind"))=="IntegerLiteral"){right_re=build_IntegerLiteral(obj_2);}
    else if(*(obj_2->getString("kind"))=="FloatingLiteral"){
      right_re=build_FloatLiteral(obj_2);
      //llvm::outs() << "no1 right value: ";
      //right_re->print(llvm::outs());
      //printf("\n");
    }
    else if(*(obj_2->getString("kind"))=="UnaryOperator"){right_re=build_UnaryOperator(obj_2);}
    else if(*(obj_2->getString("kind"))=="BinaryOperator"){right_re=build_BinaryOperator(obj_2);}
    else if(*(obj_2->getString("kind"))=="ImplicitCastExpr"){right_re=build_ImplicitCastExpr(obj_2);}
    else if(*(obj_2->getString("kind"))=="ArraySubscriptExpr"){right_re=build_Array(obj_2);}
    else if(*(obj_2->getString("kind"))=="CallExpr"){right_re=build_CallExpr(obj_2);}
    else if(*(obj_2->getString("kind"))=="ParenExpr"){right_re=build_Paren(obj_2);}
    else llvm::errs()<<"error(build_BinaryOperator_2)\n";

    if(glo_flag==1 && globalVarDict.find(right_re)!=globalVarDict.end()){
      right_re=globalVarDict[right_re];
      //printf("right_tag\n");
    }
    //else printf("no right_tag\n");
    //printf("right type:\n");
    //printType(right_re->getType());
  }



  if(sign=="=="||sign=="!="){
    if (right_re->getType() != left_re->getType()) {
      if (left_re->getType()->isIntegerTy()) {
        if (left_re->getType() == llvm::Type::getInt1Ty(TheContext))
          right_re = Builder.CreateICmpNE(right_re, llvm::ConstantInt::get(right_re->getType(), 0));
        else if (right_re->getType() == llvm::Type::getInt1Ty(TheContext))
          left_re = Builder.CreateICmpNE(left_re, llvm::ConstantInt::get(left_re->getType(), 0));
      } else if (left_re->getType()->isFloatingPointTy()) {
        if (left_re->getType()->isFloatTy())
          right_re = Builder.CreateFCmpONE(right_re, llvm::ConstantFP::get(right_re->getType(), 0.0f));
        else if (left_re->getType()->isDoubleTy())
          right_re = Builder.CreateFCmpONE(right_re, llvm::ConstantFP::get(right_re->getType(), 0.0));
      }
    }
  }

  //printType(re_type);
  //printType(left_re->getType());
  //printType(right_re->getType());
  //printf("------------------------------\n");
  if(sign!="||" && sign!="&&" && (left_re->getType()!=right_re->getType()) ){
    if (left_re->getType() != re_type && re_type->isFloatingPointTy()&&left_re->getType()->isIntegerTy()) {
      llvm::Type* targetType = re_type;
      left_re = Builder.CreateSIToFP(left_re, llvm::Type::getFloatTy(TheContext));
    }
    else if (left_re->getType() != re_type && re_type->isIntegerTy()&&left_re->getType()->isFloatingPointTy()) {
      llvm::Type* targetType = re_type;
      //printf("CreateFPToSI 5\n");
      left_re = Builder.CreateFPToSI(left_re, targetType);
    }

    if (right_re->getType() != re_type && re_type->isFloatingPointTy() &&right_re->getType()->isIntegerTy()) {
      llvm::Type* targetType = re_type;
      right_re = Builder.CreateSIToFP(right_re, llvm::Type::getFloatTy(TheContext));
    }
    else if (right_re->getType() != re_type && re_type->isIntegerTy()&&right_re->getType()->isFloatingPointTy()) {
      llvm::Type* targetType = re_type;
      //printf("CreateFPToSI 6\n");
      right_re = Builder.CreateFPToSI(right_re, targetType);
    }
  }

  //llvm::outs() << "left value: ";
  //left_re->print(llvm::outs());
  //printf("\n");
  //printf("opcode:%s\n",sign);
  //llvm::outs() << "right value: ";
  //right_re->print(llvm::outs());
  //printf("\n");

  if (sign == "=")return Builder.CreateStore(right_re,left_re);
  else if (sign == "%")return Builder.CreateSRem(left_re, right_re);
  else if (sign == "==") {
    if (left_re->getType()->isIntegerTy())
      return Builder.CreateICmpEQ(left_re, right_re); 
    else if (left_re->getType()->isFloatingPointTy())
      return Builder.CreateFCmpOEQ(left_re, right_re);
  }
  else if (sign == "!=") {
    if (left_re->getType()->isIntegerTy())
      return Builder.CreateICmpNE(left_re, right_re);
    else if (left_re->getType()->isFloatingPointTy())
      return Builder.CreateFCmpONE(left_re, right_re);
  }
  else if (sign == ">") {
    if (left_re->getType()->isIntegerTy())
      return Builder.CreateICmpSGT(left_re, right_re);
    else if (left_re->getType()->isFloatingPointTy())
      return Builder.CreateFCmpOGT(left_re, right_re);
  }
  else if (sign == "<") {
    if (left_re->getType()->isIntegerTy())
      return Builder.CreateICmpSLT(left_re, right_re);
    else if (left_re->getType()->isFloatingPointTy())
      return Builder.CreateFCmpOLT(left_re, right_re);
  }
  else if (sign == ">=") {
    if (left_re->getType()->isIntegerTy())
      return Builder.CreateICmpSGE(left_re, right_re);
    else if (left_re->getType()->isFloatingPointTy()) 
      return Builder.CreateFCmpOGE(left_re, right_re);
  }
  else if (sign == "<=") {
    if (left_re->getType()->isIntegerTy())
      return Builder.CreateICmpSLE(left_re, right_re);
    else if (left_re->getType()->isFloatingPointTy())
      return Builder.CreateFCmpOLE(left_re, right_re);
  }
  else if (sign == "+") {
    if (left_re->getType()->isIntegerTy())
      return Builder.CreateAdd(left_re, right_re);
    else if (left_re->getType()->isFloatingPointTy())
      return Builder.CreateFAdd(left_re, right_re);
  }
  else if (sign == "-") {
    if (left_re->getType()->isIntegerTy())
      return Builder.CreateSub(left_re, right_re);
    else if (left_re->getType()->isFloatingPointTy())
      return Builder.CreateFSub(left_re, right_re);
  }
  else if (sign == "*") {
    if (left_re->getType()->isIntegerTy())
      return Builder.CreateMul(left_re, right_re);
    else if (left_re->getType()->isFloatingPointTy()){

      //llvm::outs() << "left value: ";
      //left_re->print(llvm::outs());
      //printf("\n");
    
      //llvm::outs() << "right value: ";
      //right_re->print(llvm::outs());
      //printf("\n");

      auto tmp_res=Builder.CreateFMul(left_re, right_re);
      //llvm::outs() << "res value: ";
      //tmp_res->print(llvm::outs());
      //printf("\n");

      return tmp_res;
    }
  }
  else if (sign == "/") {
    if (left_re->getType()->isIntegerTy()){
      //printf("Builder.CreateSDiv\n");
      return Builder.CreateSDiv(left_re, right_re);
    }
    else if (left_re->getType()->isFloatingPointTy()){
      //printf("Builder.CreateFDiv\n");
      auto tmp_res=Builder.CreateFDiv(left_re, right_re);
      //llvm::outs() << "res value: ";
      //tmp_res->print(llvm::outs());
      //printf("\n");
      return tmp_res;
    }
  }
  else if(sign=="||"||sign=="&&"){
    //llvm::outs() <<"no2 right value: ";
    //right_re->print(llvm::outs());
    //printf("\n");
    if (right_re->getType()!= llvm::Type::getInt1Ty(TheContext)){
      if(right_re->getType()->isFloatingPointTy()){
        // 如果操作数类型为浮点数类型
        //printf("isFloatingPointTy()\n");
        llvm::Value* zeroValue = llvm::ConstantFP::get(right_re->getType(), 0.0);
        llvm::Value* cmpResult = Builder.CreateFCmpUNE(right_re, zeroValue);
        return Builder.CreateSIToFP(cmpResult, llvm::Type::getInt1Ty(TheContext));
      }
      else{
        // 如果操作数类型为整数类型
        //printf("zhengshuleixing\n");
        llvm::Value* zeroValue = llvm::ConstantInt::get(right_re->getType(), 0);
        return Builder.CreateICmpNE(right_re, zeroValue);
      }
    } 
    else return right_re;
  }
  else llvm::errs()<<"error(build_BinaryOperator_3)\n";
  return nullptr;
}


llvm::Value* build_Paren(const llvm::json::Object *O){
  //printf("build_Paren\n");
  if (auto inner_point=O->getArray("inner")) {
    if(inner_point->size()!=1){return nullptr;}
    auto fobj = inner_point->front().getAsObject();
    if(*(fobj->getString("kind"))=="IntegerLiteral"){return build_IntegerLiteral(fobj);}
    else if(*(fobj->getString("kind"))=="FloatingLiteral"){return build_FloatLiteral(fobj);}
    else if(*(fobj->getString("kind"))=="UnaryOperator"){return build_UnaryOperator(fobj);}
    else if(*(fobj->getString("kind"))=="BinaryOperator"){return build_BinaryOperator(fobj);}
    else if(*(fobj->getString("kind"))=="DeclRefExpr"){return build_DeclRefExpr(fobj);}
    else if(*(fobj->getString("kind"))=="ImplicitCastExpr"){return build_ImplicitCastExpr(fobj);}
    else if(*(fobj->getString("kind"))=="CallExpr"){return build_CallExpr(fobj);}
    else if(*(fobj->getString("kind"))=="ArraySubscriptExpr"){return build_Array(fobj);}
    else if(*(fobj->getString("kind"))=="ParenExpr"){return build_Paren(fobj);}
  }
  return nullptr;
}


llvm::Constant * build_IntegerLiteral(const llvm::json::Object *O) {
  //printf("build_IntegerLiteral\n");
  auto value = O->getString("value");
  return llvm::ConstantInt::get(TheContext, /* i32 3(decimal) */ llvm::APInt(32, *value, 10));
}
llvm::Constant *build_FloatLiteral(const llvm::json::Object *O) {
    //printf("build_FloatLiteral\n");
    auto value = O->getString("value");
    //这里偷懒了，没有用double
    llvm::APFloat floatValue(llvm::APFloat::IEEEsingle(), *value);
    return llvm::ConstantFP::get(TheContext, floatValue);
}



llvm::Value* build_UnaryOperator(const llvm::json::Object *O){
  //printf("build_UnaryOperator\n");
  int flag=0;
  auto sign=*(O->getString("opcode"));
  if(auto inner_array=O->getArray("inner")){
    //获取数组的第一个元素（也应该只有一个元素）IntegerLiteral
    auto temp=inner_array->front().getAsObject();
    llvm::Value * ptr=nullptr;
    auto node_type=temp->getString("kind");
    if(*node_type=="IntegerLiteral") ptr=build_IntegerLiteral(temp);
    else if(*node_type=="FloatingLiteral"){ptr=build_FloatLiteral(temp);flag=1;}
    else if(*node_type=="UnaryOperator") ptr=build_UnaryOperator(temp);
    else if(*node_type=="BinaryOperator") ptr=build_BinaryOperator(temp);
    else if(*node_type=="ParenExpr") ptr=build_Paren(temp);
    else if(*node_type=="ImplicitCastExpr"){ptr=build_ImplicitCastExpr(temp);}
    else if(*node_type=="CallExpr") ptr=build_CallExpr(temp);
    else if(*node_type=="ArraySubscriptExpr") ptr=build_Array(temp);
    else ptr=nullptr;

    //printType(ptr->getType());
    if(ptr->getType()==llvm::Type::getFloatTy(TheContext))flag=1;

    if(sign=="+"){return ptr;}
    else if(sign=="-"){
      //printf("- flag:%d\n",flag);
      if (flag==0){
        auto zero=llvm::ConstantInt::get(ptr->getType(), 0);
        return Builder.CreateSub(zero,ptr);
      }
      else{
        auto zero=llvm::ConstantFP::get(ptr->getType(), 0.0);
        return Builder.CreateFSub(zero,ptr);
      }
    }
    else if(sign=="!"){
      //bool类型，直接
      if (ptr->getType() == llvm::Type::getInt1Ty(TheContext)){
        if(flag==0){
          auto res=Builder.CreateXor(ptr, llvm::ConstantInt::get(ptr->getType(), 1));
          return res;
        }
        else{
          auto res=Builder.CreateXor(ptr, llvm::ConstantInt::get(ptr->getType(), 1));
          return res;
        }
      }
      //若不是bool类型，需要转换
      else{
        if(flag==0){
          ptr=Builder.CreateICmpNE(ptr, llvm::ConstantInt::get(ptr->getType(), 0));
          auto res=Builder.CreateXor(ptr, llvm::ConstantInt::get(ptr->getType(), 1));
          return res;
        }
        else{
          ptr = Builder.CreateFCmpONE(ptr, llvm::ConstantFP::get(ptr->getType(), 0.0));
          auto res = Builder.CreateXor(ptr, llvm::ConstantInt::get(ptr->getType(), 1));
          return res;
        }
      }
    }
    else return ptr;
  }
  return nullptr;
}

//finish------------------------------------------
void build_CompoundStmt(const llvm::json::Object *O) {
  //printf("build_CompoundStmt\n");
  if (auto inner_point = O->getArray("inner")) {
    for (auto &it : *inner_point) {
      if (auto P = it.getAsObject()){
        if (auto kind = P->getString("kind")){
          if (*kind == "UnaryOperator") build_UnaryOperator(P);
          else if (*kind == "DeclStmt") build_DeclStmt(P);
          else if (*kind == "ReturnStmt") build_return(P);
          else if(*kind=="NullStmt") continue;
          else if (*kind == "BinaryOperator") build_BinaryOperator(P);
          else if (*kind == "CallExpr") build_CallExpr(P);
          else if (*kind == "IfStmt") build_If(P);
          else if (*kind == "WhileStmt") build_While(P);
          else if (*kind == "ContinueStmt") build_Continue(P);
          else if (*kind == "BreakStmt") build_Break(P);
          else if (*kind == "DoStmt") build_Do(P);
          else if (*kind == "CompoundStmt") build_CompoundStmt(P);
          else return;
        }
      }
    }
  }
}


//-----------------------------------------------------------------------------------
void build_If(const llvm::json::Object *O){
  //printf("build_If\n");
  llvm::Function *the_function=Builder.GetInsertBlock()->getParent();
  if(auto inner_point=O->getArray("inner")){
    llvm::BasicBlock *IF_THEN_pt,*IF_ELSE_pt,*IF_END_pt;
    IF_THEN_pt=llvm::BasicBlock::Create(TheContext,"ifThen",the_function);
    IF_ELSE_pt=llvm::BasicBlock::Create(TheContext,"ifElse",the_function);
    IF_END_pt=llvm::BasicBlock::Create(TheContext,"ifEnd",the_function);
    auto obj_1=(*inner_point)[0].getAsObject();
    auto obj_ifthen=(*inner_point)[1].getAsObject();
    if(inner_point->size()==2){
      short_circuit_stack.push_back({IF_THEN_pt,IF_END_pt});
      IF_ELSE_pt->eraseFromParent();// 删除原本存在的 else 基本块
    }
    else if(inner_point->size()==3)short_circuit_stack.push_back({IF_THEN_pt,IF_ELSE_pt});
    // 将if语句的then块和else块压入栈

    llvm::Value *v;
    if(*(obj_1->getString("kind"))=="UnaryOperator")v=build_UnaryOperator(obj_1);
    else if(*(obj_1->getString("kind"))=="BinaryOperator"){
      v=build_BinaryOperator(obj_1);
      //printf("v=build_BinaryOperator((obj_1));\n");
      //llvm::outs() << "before value: ";
      //v->print(llvm::outs());
      //printf("\n");
    }
    else if(*(obj_1->getString("kind"))=="CallExpr")v=build_CallExpr(obj_1);
    else if(*(obj_1->getString("kind"))=="ImplicitCastExpr")v=build_ImplicitCastExpr(obj_1);
    else if(*(obj_1->getString("kind"))=="FloatingLiteral")v=build_FloatLiteral(obj_1);
    else if(*(obj_1->getString("kind"))=="IntegerLiteral")v=build_IntegerLiteral(obj_1);
    else printf("error(build_If)_1    %s \n",*(obj_1->getString("kind")));

    // 检查类型，根据类型进行处理
    if(v->getType()!=llvm::Type::getInt1Ty(TheContext)&&
      v->getType()!=llvm::Type::getFloatTy(TheContext)&&
      v->getType()!=llvm::Type::getDoubleTy(TheContext)&&
      v->getType()!=llvm::Type::getInt32Ty(TheContext)&&
      v->getType()!=llvm::Type::getInt64Ty(TheContext)){
      //llvm::errs() << "Error: Unsupported condition type1\n";
    }
    llvm::Value *bool_v = nullptr;
    
    
    //printType(v->getType());
    

    if (v->getType() == llvm::Type::getInt1Ty(TheContext)) bool_v = v;
    else {
      if (v->getType() == llvm::Type::getFloatTy(TheContext)) {
        llvm::Type *bool_type = llvm::Type::getInt1Ty(TheContext);
        bool_v = Builder.CreateFCmpONE(v, llvm::ConstantFP::get(v->getType(), 0.0));
      } 
      else if (v->getType() == llvm::Type::getDoubleTy(TheContext)) {
        llvm::Type *bool_type = llvm::Type::getInt1Ty(TheContext);
        bool_v = Builder.CreateFCmpONE(v, llvm::ConstantFP::get(v->getType(), 0.0));
      } 
      else //整数
        bool_v = Builder.CreateICmpNE(v, llvm::ConstantInt::get(v->getType(), 0));
    }

    //printType(bool_v->getType());
    //llvm::outs() << "after:value: ";
    //bool_v->print(llvm::outs());
    //printf("\n-----------------\n");

    //条件分支指令
    Builder.CreateCondBr(bool_v, short_circuit_stack.back().then(),short_circuit_stack.back().end());
    short_circuit_stack.pop_back();
    //将then块移动到当前插入指令的后面，并将插入指令设置为then块的起始位置
    IF_THEN_pt->moveAfter(Builder.GetInsertBlock());
    Builder.SetInsertPoint(IF_THEN_pt);
    if(*(obj_ifthen->getString("kind"))=="CompoundStmt")build_CompoundStmt(obj_ifthen);
    else if(*(obj_ifthen->getString("kind"))=="ImplicitCastExpr")build_ImplicitCastExpr(obj_ifthen);
    else if(*(obj_ifthen->getString("kind"))=="VarDecl")build_localvar(obj_ifthen);
    else if(*(obj_ifthen->getString("kind"))=="CallExpr")build_CallExpr(obj_ifthen);
    else if(*(obj_ifthen->getString("kind"))=="BinaryOperator")build_BinaryOperator(obj_ifthen);
    else if(*(obj_ifthen->getString("kind"))=="ContinueStmt")build_Continue(obj_ifthen);
    else if(*(obj_ifthen->getString("kind"))=="ReturnStmt")build_return(obj_ifthen);
    else if(*(obj_ifthen->getString("kind"))=="BreakStmt")build_Break(obj_ifthen);
    else if(*(obj_ifthen->getString("kind"))=="DoStmt")build_Do(obj_ifthen);
    else if(*(obj_ifthen->getString("kind"))=="IfStmt")build_If(obj_ifthen);
    else if(*(obj_ifthen->getString("kind"))=="WhileStmt")build_While(obj_ifthen);
    else if(*(obj_ifthen->getString("kind"))=="NullStmt") ;
    else printf("error(build_If)_2\n");

    if (Builder.GetInsertBlock()->getTerminator()==nullptr)Builder.CreateBr(IF_END_pt);
    //if{}else{}
    if(inner_point->size()==3){
      auto obj_ifelse=(*inner_point)[2].getAsObject();
      IF_ELSE_pt->moveAfter(Builder.GetInsertBlock());
      Builder.SetInsertPoint(IF_ELSE_pt);
      if(*(obj_ifelse->getString("kind"))=="CompoundStmt")build_CompoundStmt(obj_ifelse);
      else if(*(obj_ifelse->getString("kind"))=="ImplicitCastExpr")build_ImplicitCastExpr(obj_ifelse);
      else if(*(obj_ifelse->getString("kind"))=="VarDecl")build_localvar(obj_ifelse);
      else if(*(obj_ifelse->getString("kind"))=="CallExpr")build_CallExpr(obj_ifelse);
      else if(*(obj_ifelse->getString("kind"))=="BinaryOperator")build_BinaryOperator(obj_ifelse);
      else if(*(obj_ifelse->getString("kind"))=="ContinueStmt")build_Continue(obj_ifelse);
      else if(*(obj_ifelse->getString("kind"))=="ReturnStmt")build_return(obj_ifelse);
      else if(*(obj_ifelse->getString("kind"))=="BreakStmt")build_Break(obj_ifelse);
      else if(*(obj_ifelse->getString("kind"))=="DoStmt")build_Do(obj_ifelse);
      else if(*(obj_ifelse->getString("kind"))=="IfStmt")build_If(obj_ifelse);
      else if(*(obj_ifelse->getString("kind"))=="WhileStmt")build_While(obj_ifelse);
      else if(*(obj_ifelse->getString("kind"))=="NullStmt") ;
      else printf("error(build_If)_3\n");

      if (Builder.GetInsertBlock()->getTerminator()==nullptr)Builder.CreateBr(IF_END_pt);
    }
    IF_END_pt->moveAfter(Builder.GetInsertBlock());
    Builder.SetInsertPoint(IF_END_pt);
  }
  return;
}

void build_While(const llvm::json::Object *O){
  //printf("build_While\n");
  llvm::Function *the_function=Builder.GetInsertBlock()->getParent();
  if(auto inner_point=O->getArray("inner")){
    llvm::BasicBlock *con_pt,*body_pt,*end_pt;
    con_pt=llvm::BasicBlock::Create(TheContext,"whileCond",the_function);
    body_pt=llvm::BasicBlock::Create(TheContext,"whileBody",the_function);
    end_pt=llvm::BasicBlock::Create(TheContext,"whileEnd",the_function);
    auto obj1=inner_point->front().getAsObject();
    auto obj2=inner_point->back().getAsObject();
    // 跳转到条件判断的基本块
    Builder.CreateBr(con_pt);
    llvm::Value *v;
    // 将循环体和结束的基本块添加到短路栈中
    short_circuit_stack.push_back({body_pt,end_pt});
    Builder.SetInsertPoint(con_pt);
    
    if(*(obj1->getString("kind"))=="UnaryOperator")v=build_UnaryOperator((obj1));
    else if(*(obj1->getString("kind"))=="BinaryOperator")v=build_BinaryOperator((obj1));
    else if(*(obj1->getString("kind"))=="CallExpr")v=build_CallExpr((obj1));
    else if(*(obj1->getString("kind"))=="ImplicitCastExpr")v=build_ImplicitCastExpr((obj1));
    else if(*(obj1->getString("kind"))=="IntegerLiteral")v=build_IntegerLiteral((obj1));
    else if(*(obj1->getString("kind"))=="FloatingLiteral")v=build_FloatLiteral((obj1));
    else printf("error(build_while)_1\n");

    // 检查类型，根据类型进行处理
    if(v->getType()!=llvm::Type::getInt1Ty(TheContext)&&
      v->getType()!=llvm::Type::getFloatTy(TheContext)&&
      v->getType()!=llvm::Type::getDoubleTy(TheContext)&&
      v->getType()!=llvm::Type::getInt32Ty(TheContext)&&
      v->getType()!=llvm::Type::getInt64Ty(TheContext)){
      //llvm::errs() << "Error: Unsupported condition type2\n";
    }
    llvm::Value *bool_v = nullptr;
    if (v->getType() == llvm::Type::getInt1Ty(TheContext)) bool_v = v;
    else {
      if (v->getType() == llvm::Type::getFloatTy(TheContext)) {
        llvm::Type *bool_type = llvm::Type::getInt1Ty(TheContext);
        bool_v = Builder.CreateFCmpONE(v, llvm::ConstantFP::get(v->getType(), 0.0));
      } 
      else if (v->getType() == llvm::Type::getDoubleTy(TheContext)) {
        llvm::Type *bool_type = llvm::Type::getInt1Ty(TheContext);
        bool_v = Builder.CreateFCmpONE(v, llvm::ConstantFP::get(v->getType(), 0.0));
      } 
      else //整数
        bool_v = Builder.CreateICmpNE(v, llvm::ConstantInt::get(v->getType(), 0));
    }
    Builder.CreateCondBr(bool_v,body_pt,end_pt);
    pos_Stack.push_back({con_pt,end_pt});
    Builder.SetInsertPoint(body_pt);

    if (*(obj2->getString("kind"))=="CompoundStmt")build_CompoundStmt(obj2);
    else if(*(obj2->getString("kind"))=="ImplicitCastExpr")build_ImplicitCastExpr(obj2);
    else if(*(obj2->getString("kind"))=="CallExpr")build_CallExpr(obj2);
    else if(*(obj2->getString("kind"))=="BinaryOperator")build_BinaryOperator(obj2);
    else if(*(obj2->getString("kind"))=="IfStmt")build_If(obj2);
    else if(*(obj2->getString("kind"))=="WhileStmt")build_While(obj2);
    else if(*(obj2->getString("kind"))=="ReturnStmt")build_return(obj2);
    else printf("error(build_while)_2\n");
    
    if(Builder.GetInsertBlock()->getTerminator()==nullptr)Builder.CreateBr(con_pt);
    pos_Stack.pop_back();
    short_circuit_stack.pop_back();
    end_pt->moveAfter(Builder.GetInsertBlock());
    Builder.SetInsertPoint(end_pt);
  }
  return;
}

void build_Continue(const llvm::json::Object *O){
  //printf("build_continue\n");
  auto kind_str=*(O->getString("kind"));
  if(kind_str=="ContinueStmt"){
    llvm::BasicBlock * val=pos_Stack.back().getstart();
    Builder.CreateBr(val);
  }
}

void build_Break(const llvm::json::Object *O){
  //printf("build_break\n");
  auto kind_str=*(O->getString("kind"));
  if(kind_str=="BreakStmt"){
    llvm::BasicBlock * val=pos_Stack.back().getend();
    Builder.CreateBr(val);
  }
}

void build_Do(const llvm::json::Object *O){
  //printf("build_do\n");
  llvm::Function *the_function=Builder.GetInsertBlock()->getParent();
  if(auto inner_point=O->getArray("inner")){
    //顺序和while反着
    llvm::BasicBlock *con_pt,*body_pt,*end_pt;
    body_pt=llvm::BasicBlock::Create(TheContext,"doBody",the_function);
    con_pt=llvm::BasicBlock::Create(TheContext,"doCond",the_function);
    end_pt=llvm::BasicBlock::Create(TheContext,"doEnd",the_function);
    auto obj1=inner_point->front().getAsObject();
    auto obj2=inner_point->back().getAsObject();
    Builder.CreateBr(body_pt);
    Builder.SetInsertPoint(body_pt);
    if(*(obj1->getString("kind"))=="CompoundStmt")build_CompoundStmt(obj1);
    if(Builder.GetInsertBlock()->getTerminator()==nullptr)Builder.CreateBr(con_pt);
    con_pt->moveAfter(Builder.GetInsertBlock());
    Builder.SetInsertPoint(con_pt);
    llvm::Value *v;
    if(*(obj2->getString("kind"))=="BinaryOperator")v=build_BinaryOperator(obj2);
    else if(*(obj2->getString("kind"))=="ImplicitCastExpr")v=build_ImplicitCastExpr(obj2);
    // 检查类型，根据类型进行处理
    if(v->getType()!=llvm::Type::getInt1Ty(TheContext)&&
      v->getType()!=llvm::Type::getFloatTy(TheContext)&&
      v->getType()!=llvm::Type::getDoubleTy(TheContext)&&
      v->getType()!=llvm::Type::getInt32Ty(TheContext)&&
      v->getType()!=llvm::Type::getInt64Ty(TheContext)){
      //llvm::errs() << "Error: Unsupported condition type3\n";
    }
    llvm::Value *bool_v = nullptr;
    if (v->getType() == llvm::Type::getInt1Ty(TheContext)) bool_v = v;
    else {
      if (v->getType() == llvm::Type::getFloatTy(TheContext)) {
        llvm::Type *bool_type = llvm::Type::getInt1Ty(TheContext);
        bool_v = Builder.CreateFCmpONE(v, llvm::ConstantFP::get(v->getType(), 0.0));
      } 
      else if (v->getType() == llvm::Type::getDoubleTy(TheContext)) {
        llvm::Type *bool_type = llvm::Type::getInt1Ty(TheContext);
        bool_v = Builder.CreateFCmpONE(v, llvm::ConstantFP::get(v->getType(), 0.0));
      } 
      else //整数
        bool_v = Builder.CreateICmpNE(v, llvm::ConstantInt::get(v->getType(), 0));
    }
    Builder.CreateCondBr(bool_v,body_pt,end_pt);
    end_pt->moveAfter(Builder.GetInsertBlock());
    Builder.SetInsertPoint(end_pt);
  }
  return;
}
//-----------------------------------------------------------------------------------

//全局初始化列表
llvm::Constant* build_GlobalInitListExpr(const llvm::json::Object *O){
  //("build_GlobalInitListExpr\n");
  std::vector<llvm::Constant*> elements_vec;
  int flag;
  auto type1=O->getObject("type");
  llvm::ArrayType *atype=llvm::cast<llvm::ArrayType>(get_type(type1,flag));
  if(auto inner_point=O->getArray("inner")){
    for(const auto &i:*inner_point){
      if(auto obj=i.getAsObject()){
        if(auto kind=obj->getString("kind")){
          if(*kind=="IntegerLiteral"){
            auto var=llvm::cast<llvm::Constant>(build_IntegerLiteral(obj));
            elements_vec.emplace_back(var);
          }
          else if(*kind=="FloatingLiteral"){
            auto var=llvm::cast<llvm::Constant>(build_FloatLiteral(obj));
            elements_vec.emplace_back(var);
          }
          else if(*kind=="UnaryOperator"){
            auto var=llvm::cast<llvm::Constant>(build_UnaryOperator(obj));
            elements_vec.emplace_back(var);
          }
          else if(*kind=="BinaryOperator"){
            auto var=llvm::cast<llvm::Constant>(build_BinaryOperator(obj));
            elements_vec.emplace_back(var);
          }
          else if(*kind=="InitListExpr"){
            auto var=llvm::cast<llvm::Constant>(build_GlobalInitListExpr(obj));
            elements_vec.emplace_back(var);
          }
          //else printf("error(build_GlobalInitListExpr)\n");
        }
      }
    }
  }
  else if(auto inner_f=O->getArray("array_filler")){
    for(const auto & i:*inner_f){
      if(auto obj=i.getAsObject()){
        if(auto kind=obj->getString("kind")){
          if(*kind=="IntegerLiteral"){
            auto var=llvm::cast<llvm::Constant>(build_IntegerLiteral(obj));
            elements_vec.emplace_back(var);
          }
          else if(*kind=="FloatingLiteral"){
            auto var=llvm::cast<llvm::Constant>(build_FloatLiteral(obj));
            elements_vec.emplace_back(var);
          }
          else if(*kind=="UnaryOperator"){
            auto var=llvm::cast<llvm::Constant>(build_UnaryOperator(obj));
            elements_vec.emplace_back(var);
          }
          else if(*kind=="BinaryOperator"){
            auto var=llvm::cast<llvm::Constant>(build_BinaryOperator(obj));
            elements_vec.emplace_back(var);
          }
          else if(*kind=="InitListExpr"){
            auto var=llvm::cast<llvm::Constant>(build_GlobalInitListExpr(obj));
            elements_vec.emplace_back(var);
          }
          //else printf("error(build_GlobalInitListExpr)\n");
        }
      }
    }
  }
  while(elements_vec.size()<atype->getArrayNumElements()){
    auto a=llvm::Constant::getNullValue(atype->getArrayElementType());
    elements_vec.emplace_back(a);
  }
  return llvm::ConstantArray::get(atype,elements_vec);
}


void buildGlobalVarDecl(const llvm::json::Object *O){
  //printf("buildGlobalVarDecl\n");
  //标记常量
  int flag=0;
  llvm::Type* type = llvm::Type::getInt32Ty(TheContext);
  if (auto type_oject = O->getObject("type"))
    type = get_type(type_oject, flag);
  
  auto name = O->getString("id"); // 作为变量名
  llvm::GlobalVariable *globalVar=nullptr;
  if (flag==0)
  globalVar = new llvm::GlobalVariable(TheModule, type, false,
              llvm::GlobalValue::PrivateLinkage, 0, *name);
  else
  globalVar = new llvm::GlobalVariable(TheModule, type, true,
              llvm::GlobalValue::PrivateLinkage, 0, *name);

  globalVarDict.insert({globalVar, llvm::Constant::getNullValue(type)}); // 存储全局变量指针和初始值到字典中
  if (auto inner = O->getArray("inner")){
    for (const auto &it : *inner)
      if (auto P = it.getAsObject())
        if (auto kind = P->getString("kind")){
          if (*kind == "IntegerLiteral"){
            globalVar->setInitializer(build_IntegerLiteral(P));
            auto it = globalVarDict.find(globalVar);
            if (it != globalVarDict.end()) {
              it->second = build_IntegerLiteral(P); // 更新全局变量字典的对应值
            }
          }
          //double
          else if(*kind == "FloatingLiteral"){
            globalVar->setInitializer(build_FloatLiteral(P));
            auto it = globalVarDict.find(globalVar);
            if (it != globalVarDict.end()) {
              it->second = build_FloatLiteral(P); // 更新全局变量字典的对应值
            }
          }
          //float
          else if(*kind == "UnaryOperator"){
            //from 'llvm::Value *' ---> 'llvm::Constant *'
            globalVar->setInitializer(llvm::cast<llvm::Constant>(build_UnaryOperator(P)));
          }
          else if(*kind == "ImplicitCastExpr"){
            glo_flag=1;
            globalVar->setInitializer(llvm::cast<llvm::Constant>(build_ImplicitCastExpr(P)));
            auto it = globalVarDict.find(globalVar);
            if (it != globalVarDict.end()) {
              it->second = llvm::cast<llvm::Constant>(build_ImplicitCastExpr(P)); // 更新全局变量字典的对应值
            }
            glo_flag=0;
          }
          else if(*kind=="InitListExpr")
            globalVar->setInitializer(build_GlobalInitListExpr(P));
          else if(*kind=="BinaryOperator"){
            glo_flag=1;
            globalVar->setInitializer(llvm::cast<llvm::Constant>(build_BinaryOperator(P)));
            glo_flag=0;
          }
          else globalVar->setInitializer(llvm::Constant::getNullValue(type));
        }
  } 
  else 
    globalVar->setInitializer(llvm::Constant::getNullValue(type));
}

//finish------------------------------------------------------------------
void buildTranslationUnitDecl(const llvm::json::Object *O) {
  //printf("buildTranslationUnitDecl\n");
  //根节点
  if (O == nullptr)
    return;
  if (auto kind = O->get("kind")->getAsString()) {
    assert(*kind == "TranslationUnitDecl");
  } else {
    assert(0);
  }
  //内部节点，进行遍历
  if (auto inner = O->getArray("inner"))
    for (const auto &it : *inner)
      if (auto temp = it.getAsObject())
        if (auto kind = temp->get("kind")->getAsString()) {
          if (*kind == "FunctionDecl"){
            buildFunctionDecl(temp);
          }
          if (*kind == "VarDecl"){
            //printf("vardecl\n");
            buildGlobalVarDecl(temp);
          }
        }
}

} // namespace
//finish------------------------------------------------------------------
int main() {
  //从文件或stdin获取AST文本
  auto llvmin = llvm::MemoryBuffer::getFileOrSTDIN("-");
  //解析为JSON
  auto json = llvm::json::parse(llvmin.get()->getBuffer());
  //遍历AST，生成IR
  buildTranslationUnitDecl(json->getAsObject());
  //输出IR
  TheModule.print(llvm::outs(), nullptr);
}
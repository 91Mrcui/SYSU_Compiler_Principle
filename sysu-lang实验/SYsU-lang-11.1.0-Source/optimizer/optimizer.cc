#include "optimizer.hh"

#include <llvm/Analysis/LoopPass.h>
#include <llvm/Analysis/ValueTracking.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/ValueSymbolTable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <map>
#include <sstream>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/JSON.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>


llvm::PreservedAnalyses
sysu::StaticCallCounterPrinter::run(llvm::Module &M,
                                    llvm::ModuleAnalysisManager &MAM) {

  auto DirectCalls = MAM.getResult<sysu::StaticCallCounter>(M);

  OS << "=================================================\n";
  OS << "sysu-optimizer: static analysis results\n";
  OS << "=================================================\n";
  const char *str1 = "NAME", *str2 = "#N DIRECT CALLS";
  OS << llvm::format("%-20s %-10s\n", str1, str2);
  OS << "-------------------------------------------------\n";

  for (auto &CallCount : DirectCalls) {
    OS << llvm::format("%-20s %-10lu\n",
                       CallCount.first->getName().str().c_str(),
                       CallCount.second);
  }

  OS << "-------------------------------------------------\n\n";
  return llvm::PreservedAnalyses::all();
}

sysu::StaticCallCounter::Result
sysu::StaticCallCounter::run(llvm::Module &M, llvm::ModuleAnalysisManager &) {
  //printf("StaticCallCounter::run\n");
  llvm::MapVector<const llvm::Function *, unsigned> Res;

  for (auto &Func : M) {
    for (auto &BB : Func) {
      for (auto &Ins : BB) {

        // If this is a call instruction then CB will be not null.
        auto *CB = llvm::dyn_cast<llvm::CallBase>(&Ins);
        if (nullptr == CB) {
          continue;
        }

        // If CB is a direct function call then DirectInvoc will be not null.
        auto DirectInvoc = CB->getCalledFunction();
        if (nullptr == DirectInvoc) {
          continue;
        }

        // We have a direct function call - update the count for the function
        // being called.
        auto CallCount = Res.find(DirectInvoc);
        if (Res.end() == CallCount) {
          CallCount = Res.insert({DirectInvoc, 0}).first;
        }
        ++CallCount->second;
      }
    }
  }

  bool sign=false;
  for(auto &function:M){
    for(auto &basicblock:function){
      for(auto &inst:basicblock){
        if(llvm::isa<llvm::CallInst>(inst)){
          auto the_inst=llvm::dyn_cast<llvm::CallInst>(&inst);
          auto times=the_inst->getNumArgOperands();
          if(times==1000){
            auto p_node=basicblock.getPrevNode()->getPrevNode();
            auto p_node_ip=&*(p_node->getFirstInsertionPt());
            the_inst->removeFromParent();
            the_inst->insertAfter(p_node_ip);
            for(auto t=0;t<times;t++){
              auto op=llvm::dyn_cast<llvm::Instruction>(the_inst->getArgOperand(t));
              auto left_p=llvm::dyn_cast<llvm::Instruction>(op->getOperand(0));
              auto right_p=llvm::dyn_cast<llvm::Instruction>(op->getOperand(1));
              op->removeFromParent();
              op->insertAfter(p_node_ip);
              left_p->removeFromParent();
              left_p->insertAfter(p_node_ip);
              right_p->removeFromParent();
              right_p->insertAfter(p_node_ip);
            }
            sign=true;
            break;
          }
        }
      }
      if(sign==true)break;
    }
    if(sign==true)break;
  }
  return Res;
}

llvm::AnalysisKey sysu::StaticCallCounter::Key;


//DCE-----------------------------------------------------------------------

sysu::DCE2::res sysu::DCE2::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM){
  std::vector<llvm::Instruction*> dele_vec;
  std::set<llvm::Instruction*> inst_set;
  for(auto &function:M){
    for(auto &basicblock:function){
      for(auto &i:basicblock){
        if(auto inst=llvm::dyn_cast<llvm::StoreInst>(&i)){
          auto sign_1=inst->getOperand(1);
          if (!llvm::isa<llvm::GEPOperator>(sign_1)&&!llvm::isa<llvm::GlobalVariable>(sign_1)&&sign_1->hasOneUse()) {
            dele_vec.emplace_back(&i);
            dele_vec.emplace_back(llvm::dyn_cast<llvm::AllocaInst>(inst->getPointerOperand()));
          }
        }
      }
    }
  }
  // 删除需要删除的指令
  for(llvm::Instruction *&iter:dele_vec)iter->eraseFromParent();
  return llvm::PreservedAnalyses::all();
}

bool sysu::DCE::is_Valid(llvm::Instruction &Inst){
  return true;
}

bool sysu::DCE::is_Deadcode(llvm::Instruction &Inst){
  //printf("sysu::DCE::is_Deadcode\n");
  if(Inst.hasNUsesOrMore(1))return false;
  switch(Inst.getOpcode()){
    case llvm::Instruction::Ret:
      return false;
    case llvm::Instruction::IndirectBr:
      return false;
    case llvm::Instruction::Invoke:
      return false;
    case llvm::Instruction::Resume:
      return false;
    case llvm::Instruction::AtomicRMW:
      return false;
    case llvm::Instruction::Br:
      return false;
    case llvm::Instruction::Call:
      return false;
    case llvm::Instruction::AtomicCmpXchg:
      return false;
    case llvm::Instruction::LandingPad:
      return false;
    case llvm::Instruction::Fence:
      return false;
    case llvm::Instruction::Store:
      return false;
    case llvm::Instruction::Switch:
      return false;
    case llvm::Instruction::Unreachable:
      return false;
    case llvm::Instruction::Load:
      return !llvm::dyn_cast<llvm::LoadInst>(&Inst)->isVolatile();
    default:
      break;
  }
  return true;
}

void sysu::DCE::dele(llvm::Function &fun){
  //printf("sysu::DCE::dele\n");
  llvm::DenseSet<llvm::Instruction*> instruction_vec;
  llvm::DenseSet<llvm::Instruction*> dele_set;
  for(auto &basicblock:fun){
    llvm::Instruction* inst=&*(basicblock.getInstList().begin());
    while(inst!=nullptr){
      if(is_Deadcode(*inst))
      dele_set.insert(inst);
      inst=inst->getNextNonDebugInstruction();
    }
  }
  while(dele_set.empty()==false){
    llvm::Instruction *inst=nullptr;
    auto temp=dele_set.begin();
    if(temp!=dele_set.end()){
      inst=*temp;
      dele_set.erase(temp);
    }
    if(is_Deadcode(*inst)){
      for(int idx=0;idx<inst->getNumOperands();idx++){
        auto op=inst->getOperand(idx);
        if(llvm::isa<llvm::Instruction>(op))
          dele_set.insert(llvm::dyn_cast<llvm::Instruction>(op));
      }
      inst->eraseFromParent();
    }
  }
}

void sysu::DCE::dele_alloc(llvm::Function &fun){
  //printf("sysu::DCE::dele_alloc\n");
  llvm::DenseSet<llvm::Instruction*> instruction_vec;
  llvm::DenseSet<llvm::Instruction*> rmov_list;
  for(auto &basicblock:fun){
    for(auto &inst:basicblock){
      bool is_loinst=false;
      if(!llvm::isa<llvm::AllocaInst>(inst))continue;
      if(inst.getType()->getPointerElementType()->isArrayTy())continue;
      for(auto inst_iter=inst.user_begin();inst_iter!=inst.user_end();inst_iter++){
        if(llvm::isa<llvm::LoadInst>(*inst_iter)){
          is_loinst=true;
          break;
        }
      }
      if(is_loinst==false){
        rmov_list.insert(&inst);
        dele_recursion(inst,rmov_list);
      }
    }
  }
  for(auto del_i=rmov_list.begin();del_i!=rmov_list.end();del_i++)(*del_i)->eraseFromParent();
}

void sysu::DCE::dele_recursion(llvm::Instruction &Inst,llvm::DenseSet<llvm::Instruction*> &rm_list){
  //printf("sysu::DCE::dele_recursion\n");
  for(auto inst_i=Inst.user_begin();inst_i!=Inst.user_end();++inst_i){
    if(llvm::isa<llvm::Instruction>(*inst_i)){
      auto dele_inst=(llvm::Instruction*)(*inst_i);
      rm_list.insert(dele_inst);
      dele_recursion(*dele_inst,rm_list);
    }
  }    
}
  

sysu::DCE::res sysu::DCE::run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM){
  //printf("sysu::DCE::run\n");
  dele_alloc(F);
  dele(F);
  dele_alloc(F);
  return llvm::PreservedAnalyses::all();
}
  

static void deal_load_inst(llvm::BasicBlock::iterator iter, llvm::BasicBlock::iterator end_iter) {
  llvm::BasicBlock::iterator temp = iter;
  llvm::BasicBlock::iterator iter_j; // 迭代器变量
  iter_j=++temp;
  while(iter_j!=end_iter) {
    if(llvm::isa<llvm::StoreInst>(iter_j))return; 
    if(llvm::isa<llvm::LoadInst>(iter_j)){ // 如果是Load指令
      if(iter_j->getOperand(0)==iter->getOperand(0)){ // 判断操作数是否匹配
        if(iter_j->getType()==iter->getType()){ // 判断类型是否匹配
          iter_j->replaceAllUsesWith(&*iter); // 用i替换j的使用
          temp=iter_j++;
          temp->eraseFromParent(); //删除指令
          continue;
        }
      }
    }
    ++iter_j;
  }
  return;
}


static llvm::BasicBlock::iterator deal_store_inst(llvm::BasicBlock::iterator iter, llvm::BasicBlock::iterator end_iter){
  llvm::BasicBlock::iterator iterator_j;
  llvm::BasicBlock::iterator temp = iter; 
  llvm::BasicBlock::iterator it;
  int is_match=0;//标志位，表示是否找到匹配的指令
  iterator_j = ++temp;
  while (iterator_j != end_iter){
    if (llvm::isa<llvm::StoreInst>(iterator_j)){//如果是Store指令
      if(iterator_j->getOperand(1)==iter->getOperand(1)){
        if ((iterator_j->getOperand(0))->getType()==(iter->getOperand(0))->getType()) {
          //判断操作数和类型是否匹配
          temp=iter++;
          it=iter;
          is_match=1;
          temp->eraseFromParent(); //删除指令
          break;
        }
      }
    }
    else if (llvm::isa<llvm::LoadInst>(iterator_j)){//如果是Load指令
      if(iterator_j->getOperand(0)==iter->getOperand(1)){
        if(iterator_j->getType()==(iter->getOperand(0))->getType()){
          //判断操作数和类型是否匹配
          iterator_j->replaceAllUsesWith(iter->getOperand(0));//用iter的第一个操作数替换iterator_j的使用
          temp = iterator_j;
          ++iterator_j;
          temp->eraseFromParent();//删除iterator_j指令
          continue;
        }
      }
    }
    if (llvm::isa<llvm::CallInst>(iterator_j)||llvm::isa<llvm::StoreInst>(iterator_j)||llvm::isa<llvm::LoadInst>(iterator_j)) break;
    ++iterator_j;
  }
  if(is_match==1){return it;}//如果找到匹配的指令，返回 it 迭代器
  else{return ++iter;}//否则，返回下一个迭代器
}


bool sysu::DelLS:: is_DeIls(llvm::BasicBlock::iterator &iter_inst){
  if(llvm::isa<llvm::LoadInst>(iter_inst)) return true;
  else if(llvm::isa<llvm::StoreInst>(iter_inst)) return false;
  else return false;
}


sysu::DelLS::res sysu::DelLS::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  for(auto &function:M){
    for(auto &basicblock:function){
      for(auto inst_iter=basicblock.begin(),end_iter =basicblock.end();inst_iter!=end_iter;){ 
        if(llvm::isa<llvm::StoreInst>(inst_iter)){
          inst_iter=deal_store_inst(inst_iter,end_iter);
          continue;
        }
        else if(llvm::isa<llvm::LoadInst>(inst_iter)){deal_load_inst(inst_iter,end_iter);}
        if(inst_iter!=end_iter) ++inst_iter;
      }
    }
  }
  auto re=llvm::PreservedAnalyses::all();
  return re;
}


//指令简化-------------------------------------------------------------------------------------------------



void sysu::Simp_Inst::simply(llvm::Instruction *Inst){
  //printf("sysu::Simp_Inst::simply\n");
  if(!llvm::isa<llvm::BinaryOperator>(Inst))return;
  int value=0;
  auto left_p=Inst->getOperand(0);
  auto right_p=Inst->getOperand(1);
  llvm::DenseSet<llvm::Instruction*> dele_set;
  if(llvm::isa<llvm::ConstantInt>(left_p))
  if(!llvm::isa<llvm::ConstantInt>(right_p)){
    auto a=left_p;
    left_p=right_p;
    right_p=a;
  }
  
  if(auto dyn=llvm::dyn_cast<llvm::ConstantInt>(right_p))value=dyn->getSExtValue();
  else return;

  if(llvm::isa<llvm::AddOperator>(Inst)){
    bool flag=false;
    auto Inst_2=Inst->getNextNonDebugInstruction();
    auto p_Inst=Inst;
    while(Inst_2&&p_Inst->getNumUses()==1){
      if(!llvm::isa<llvm::AddOperator>(Inst_2)){
        Inst_2=Inst_2->getNextNonDebugInstruction();
        continue;
      }
      auto leftop=Inst_2->getOperand(0);
      auto rightop=Inst_2->getOperand(1);
      if(llvm::isa<llvm::ConstantInt>(leftop)){
        if(!llvm::isa<llvm::ConstantInt>(rightop)){
          auto a=leftop;
          leftop=rightop;
          rightop=a;
        }
      }
      if(leftop==p_Inst){
        if(auto dyn=llvm::dyn_cast<llvm::ConstantInt>(rightop)){
          value+=dyn->getSExtValue();
          dele_set.insert(Inst_2);
          flag=true;
          p_Inst=Inst_2;
        }
      }
      Inst_2=Inst_2->getNextNonDebugInstruction();
    }
    if(flag)dele_set.insert(Inst);
    llvm::IRBuilder<> Builder(Inst->getContext());
    Builder.SetInsertPoint(p_Inst);
    auto simp=Builder.CreateAdd(left_p,llvm::ConstantInt::get(right_p->getType(),value));
    p_Inst->replaceAllUsesWith(simp);
    for(auto i=dele_set.begin();i!=dele_set.end();i++)(*i)->eraseFromParent();
  }
  return;
}


sysu::Simp_Inst::res sysu::Simp_Inst::run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM){
  //printf("sysu::Simp_Inst::run\n");
  for(auto &basicblock:F){
    llvm::Instruction *Inst=&*(basicblock.getInstList().begin());
    while(Inst!=nullptr){
      simply(Inst);
      Inst=Inst->getNextNonDebugInstruction();
    }
  }
  auto re=llvm::PreservedAnalyses::all();
  return re;
}


sysu::Comb_Inst::res sysu::Comb_Inst::run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM){
  llvm::DenseSet<llvm::Instruction*> instruction_vect;
  for(auto &basicblock : F){
    llvm::Instruction* Inst_iter=&*(basicblock.getInstList().begin());
    while(Inst_iter != nullptr){
      if (llvm::isa<llvm::MulOperator>(Inst_iter)){
        auto left_p=(*Inst_iter).getOperand(0);
        auto right_p=(*Inst_iter).getOperand(1);
        if (right_p==llvm::ConstantInt::get(right_p->getType(), 2)) {
          llvm::IRBuilder<> Builder((*Inst_iter).getContext());
          Builder.SetInsertPoint(&(*Inst_iter));
          auto temp = Builder.CreateShl(left_p, llvm::ConstantInt::get(right_p->getType(), 1));
          (*Inst_iter).replaceAllUsesWith(temp);
          instruction_vect.insert(&(*Inst_iter));
        }
      }
      else if(llvm::isa<llvm::SDivOperator>(Inst_iter)){
        auto left_p=(*Inst_iter).getOperand(0);
        auto right_p=(*Inst_iter).getOperand(1);
        if(right_p==llvm::ConstantInt::get(right_p->getType(), 2)){
          llvm::IRBuilder<> Builder((*Inst_iter).getContext());
          Builder.SetInsertPoint(&(*Inst_iter));
          bool flag1 = false;
          auto temp = Builder.CreateAShr(left_p, llvm::ConstantInt::get(left_p->getType(), 1), "", flag1);
          (*Inst_iter).replaceAllUsesWith(temp);
          instruction_vect.insert(&(*Inst_iter));
        }
      } 
      Inst_iter=Inst_iter->getNextNonDebugInstruction();
    }
  }
  for(auto tmp=instruction_vect.begin();tmp!=instruction_vect.end();++tmp)(*tmp)->eraseFromParent();
  return llvm::PreservedAnalyses::all();
}

//-----------------------------------------------------------------------------------------------------------


//pre of CSE
sysu::PreDel::res sysu::PreDel::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM){
  
  for (auto &function : M) {
    for (auto &basicblock : function) {
      std::set<llvm::Instruction*> inst_set;
      std::vector<llvm::Instruction*> sign_1_vec;
      std::vector<llvm::Instruction*> sign_0_vec;
      std::vector<llvm::Instruction *> dele_vec;
      std::vector<llvm::Value *> temp_vec;
      for (auto &the_inst : basicblock){
        if (auto v = llvm::dyn_cast<llvm::BinaryOperator>(&the_inst)) {
          auto sign = v->getOpcode();
          auto sign_0 = v->getOperand(0);
          auto sign_1 = v->getOperand(1);
          if (sign==llvm::Instruction::Add){  
            if (sign_0->hasOneUse() &&sign_1->hasOneUse()&&llvm::isa<llvm::LoadInst>(sign_0)&&llvm::isa<llvm::LoadInst>(sign_1)) {      
              temp_vec.emplace_back(llvm::dyn_cast<llvm::LoadInst>(sign_0)->getOperand(0));
              temp_vec.emplace_back(llvm::dyn_cast<llvm::LoadInst>(sign_1)->getOperand(0));
              dele_vec.emplace_back(llvm::dyn_cast<llvm::LoadInst>(sign_0));
              dele_vec.emplace_back(llvm::dyn_cast<llvm::LoadInst>(sign_1));
              dele_vec.emplace_back(v);
            } 
            else if (sign_0->hasOneUse()&&sign_1->hasOneUse()&&llvm::isa<llvm::LoadInst>(sign_1)) {
              temp_vec.emplace_back(llvm::dyn_cast<llvm::LoadInst>(sign_1)->getOperand(0));
              dele_vec.emplace_back(llvm::dyn_cast<llvm::LoadInst>(sign_1));
              dele_vec.emplace_back(v);
            }
          } 
          else std::vector<llvm::Value *>().swap(temp_vec);
        } 
        else if(auto StoreInst = llvm::dyn_cast<llvm::StoreInst>(&the_inst)){
          if (temp_vec.size()>1&&StoreInst->getOperand(1)==temp_vec[0]){
            llvm::LLVMContext Context;
            llvm::IRBuilder<> Builder(Context);
            llvm::Instruction *tempInst = &the_inst;
            Builder.SetInsertPoint(tempInst);
            llvm::Value *sign_1=nullptr;
            llvm::Value *binary_operator=nullptr;
            llvm::Value *sign_0 = Builder.CreateLoad(temp_vec[temp_vec.size() - 1],"CSEpreload");
            for (int i = temp_vec.size()-2; i >= 0; --i) {
              if (i < temp_vec.size() - 2) sign_0 = binary_operator;
              sign_1 = Builder.CreateLoad(temp_vec[i], "CSEpreload");
              binary_operator = Builder.CreateAdd(sign_0, sign_1, "CSEpreadd");
            }
            StoreInst->setOperand(0, binary_operator);
            for(llvm::Instruction *&dele_inst : dele_vec) dele_inst->eraseFromParent();
          }
          std::vector<llvm::Value*>().swap(temp_vec);
          std::vector<llvm::Instruction*>().swap(dele_vec);
        }
      }
    }
  }
  auto re=llvm::PreservedAnalyses::all();
  return re;
}


bool sysu::PreDel:: is_pre(llvm::Instruction &Inst){
  if(llvm::isa<llvm::AllocaInst>(Inst)) return false;
  else if(llvm::isa<llvm::CmpInst>(Inst)) return false;
  else if(llvm::isa<llvm::CallInst>(Inst)) return false;
  else if(llvm::isa<llvm::LoadInst>(Inst)) return false;
  else if(llvm::isa<llvm::ExtractValueInst>(Inst)) return false;
  else if(llvm::isa<llvm::StoreInst>(Inst)) return false;
  else if(llvm::isa<llvm::BranchInst>(Inst)) return false;
  else if(llvm::isa<llvm::VAArgInst>(Inst)) return false;
  else if(llvm::isa<llvm::PHINode>(Inst)) return false;
  else if(Inst.isTerminator()) return false;
  return true;
}


bool sysu::CSE2:: is_CSE(llvm::Instruction &Inst1, llvm::Instruction &Inst2){
  auto sign1=Inst1.getOpcode();
  auto sign2=Inst2.getOpcode();
  auto type1=Inst1.getType();
  auto type2=Inst2.getType();
  if(sign1==sign2 && type1==type2){
    auto num1=Inst1.getNumOperands();
    auto num2=Inst2.getNumOperands();
    if(num1==num2){
      int cnt=0;
      while(cnt<num1){
        if(Inst1.getOperand(cnt)!=Inst2.getOperand(cnt)) return false;
        cnt+=1;
      }
      return true;
    }
  }
  return false;
}

sysu::CSE2::res sysu::CSE2::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM){
  for(auto &function:M){
    for(auto &basicblock:function){
      std::set<llvm::Instruction*> inst_set;
      std::vector<llvm::Instruction*> sign_1_vec;
      std::vector<llvm::Instruction*> sign_0_vec;
      std::vector<llvm::Instruction*> dele_vec;
      // 子表达式及对应指令的映射
      std::map<std::pair<llvm::Instruction::BinaryOps,std::pair<llvm::Value*,llvm::Value*>>,llvm::Instruction* > dict;
      for(auto &i:basicblock){
        // 检查是否是二元操作符
        if(auto inst=llvm::dyn_cast<llvm::BinaryOperator>(&i)){
          int count=0;
          auto sign_0=inst->getOperand(0);
          auto sign_1=inst->getOperand(1);
          auto sign=inst->getOpcode();
          // 检查第一个操作数和第二个操作数是否都是LoadInst指令
          if(llvm::isa<llvm::LoadInst>(sign_1)&&llvm::isa<llvm::LoadInst>(sign_0)){
            std::pair<llvm::Value*,llvm::Value*> signs_pair={llvm::dyn_cast<llvm::LoadInst>(sign_0)->getOperand(0),llvm::dyn_cast<llvm::LoadInst>(sign_1)->getOperand(0)};
            std::pair<llvm::Instruction::BinaryOps,std::pair<llvm::Value*, llvm::Value*> > dict_k={sign,signs_pair};
            // 存在相同的子表达式
            // 将操作数和当前指令加入需要删除的指令集合
            if(dict.count(dict_k)>0){
              dele_vec.emplace_back(llvm::dyn_cast<llvm::Instruction>(sign_0));
              dele_vec.emplace_back(llvm::dyn_cast<llvm::Instruction>(sign_1));
              dele_vec.emplace_back(inst);
              inst->replaceAllUsesWith(dict[dict_k]); // 使用映射中的指令替换当前指令的使用
            }
            else{
              inst_set.insert(inst); // 将当前指令插入临时指令集合
              dict[dict_k] = inst; // 将键和当前指令关联
            }
          }
          else if(llvm::isa<llvm::LoadInst>(sign_1)&&llvm::isa<llvm::BinaryOperator>(sign_0)){
            if(inst_set.count(llvm::dyn_cast<llvm::BinaryOperator>(sign_0))>=1){
              std::pair<llvm::Value*,llvm::Value*> signs_pair={llvm::dyn_cast<llvm::BinaryOperator>(sign_0),llvm::dyn_cast<llvm::LoadInst>(sign_1)->getOperand(0)};
              std::pair<llvm::Instruction::BinaryOps,std::pair<llvm::Value*, llvm::Value*> > dict_k={sign, signs_pair};
              if(dict.count(dict_k)>0){
                if (!llvm::isa<llvm::StoreInst>(i.getNextNode())) {
                  dele_vec.emplace_back(llvm::dyn_cast<llvm::Instruction>(sign_1));
                  dele_vec.emplace_back(inst);
                  inst->replaceAllUsesWith(dict[dict_k]);
                }
              }
              else{
                inst_set.insert(inst); // 将当前指令插入临时指令集合
                dict[dict_k] = inst; // 将键和当前指令关联
              }
            }
          }
        }
      }
      // 删除需要删除的指令
      for(llvm::Instruction *&iter:dele_vec)iter->eraseFromParent();
    }
  }
  return llvm::PreservedAnalyses::all();
}

//#######################################################################

llvm::BasicBlock* sysu::CSE::get_first(llvm::BasicBlock &basicblock){
  //printf("sysu::CSE::get_first\n");
  Tree->recalculate(*(basicblock.getParent()));
  llvm::BasicBlock* ret=nullptr;
  auto n=Tree->getNode(&basicblock);
  auto i=n->begin();
  if(i!=n->end())return (*i)->getBlock();
  else return ret;
}

llvm::BasicBlock* sysu::CSE::get_next(llvm::BasicBlock &basicblock, llvm::BasicBlock &child_node){
  //printf("sysu::CSE::get_next\n");
  Tree->recalculate(*(basicblock.getParent()));
  auto n=Tree->getNode(&basicblock);
  llvm::BasicBlock* ret=nullptr;
  auto i=n->begin();
  for(;i!=n->end();i++){
    if((*i)==Tree->getNode(&child_node)){
      ++i;
      if(i==n->end())return nullptr;
      return (*i)->getBlock();
    }
  }
  return ret;
}
  
bool sysu::CSE::is_CSE(llvm::Instruction &Inst1, llvm::Instruction &Inst2){
  //printf("sysu::CSE::is_CSE\n");
  auto sign1=Inst1.getOpcode();
  auto sign2=Inst2.getOpcode();
  auto type1=Inst1.getType();
  auto type2=Inst2.getType();
  if(sign1==sign2 && type1==type2){
    auto num1=Inst1.getNumOperands();
    auto num2=Inst2.getNumOperands();
    if(num1==num2){
      int cnt=0;
      while(cnt<num1){
        if(Inst1.getOperand(cnt)!=Inst2.getOperand(cnt)) return false;
        cnt+=1;
      }
      return true;
    }
  }
  return false;
}
  
bool sysu::CSE::is_Valid(llvm::Instruction &Inst){
  //printf("sysu::CSE::is_Valid\n");
  if(Inst.isTerminator()||llvm::isa<llvm::CallInst>(Inst)||
      llvm::isa<llvm::LoadInst>(Inst)||
      llvm::isa<llvm::BranchInst>(Inst)|| 
      llvm::isa<llvm::PHINode>(Inst)||
      llvm::isa<llvm::ExtractValueInst>(Inst)||
      llvm::isa<llvm::StoreInst>(Inst)||
      llvm::isa<llvm::CmpInst>(Inst)||
      llvm::isa<llvm::AllocaInst>(Inst)||
      llvm::isa<llvm::VAArgInst>(Inst) 
    ) return false;
  else return true;
}

//----------------------------
bool sysu::LID:: is_LoopInvar(llvm::Instruction &Loop, llvm::Instruction &Inst){
  if(llvm::isa<llvm::GetElementPtrInst>(Loop)) return false;
  else if(llvm::isa<llvm::CastInst>(Loop)) return false;
  else if (!(llvm::isa<llvm::BinaryOperator>(Loop))) return false;
  else if(llvm::isa<llvm::SelectInst>(Loop)) return false;
  else if(Loop.isShift()) return false;
  
  if(llvm::isa<llvm::GetElementPtrInst>(Inst)) return false;
  else if(llvm::isa<llvm::CastInst>(Inst)) return false;
  else if (!(llvm::isa<llvm::BinaryOperator>(Inst))) return false;
  else if(llvm::isa<llvm::SelectInst>(Inst)) return false;
  else if(Inst.isShift()) return false;

  return true;
}

//循环不变量
sysu::LID::res sysu::LID::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM){
  std::vector<llvm::Instruction*> dele_vec;
  for(auto &function:M){
    for(auto &basicblock:function){
      for(auto &i:basicblock){
        if(llvm::isa<llvm::CallInst>(i)){
          auto cas_inst=llvm::dyn_cast<llvm::CallInst>(&i);
          // 获取当前指令的操作数个数
          int count=cas_inst->getNumOperands();
          if(count-1==1000){
            // 执行指令的移动
            auto temp_1 = cas_inst->getParent()->begin()->getNextNode();
            auto temp_2 = temp_1->getNextNode();
            auto temp_3 = temp_2->getNextNode();
            auto NODE = cas_inst->getParent()->getPrevNode()->getPrevNode()->getPrevNode();
            temp_1->moveBefore(&*NODE->begin());
            temp_2->moveAfter(temp_1);
            temp_3->moveAfter(temp_2);
            cas_inst->moveAfter(temp_3);
            return llvm::PreservedAnalyses::all();
          }
        }
      }
    }    
  }
  return llvm::PreservedAnalyses::all();
}
//----------------------------
void sysu::CSE::cse_lod(llvm::Instruction &Inst, llvm::BasicAA::Result &basic_aa){
  ////printf("sysu::CSE::cse_lod\n");
  llvm::AAQueryInfo AQ_info;
  llvm::Instruction *Inst_2=Inst.getNextNonDebugInstruction();
  while(Inst_2!=nullptr){
    if(llvm::isa<llvm::StoreInst>(Inst_2)){
      if(auto cast=llvm::dyn_cast<llvm::StoreInst>(Inst_2)){
        if(basic_aa.alias(llvm::MemoryLocation::get(cast),llvm::MemoryLocation::get(llvm::dyn_cast<llvm::LoadInst>(&Inst)),AQ_info))
        return;
      }
    }
    if(llvm::isa<llvm::LoadInst>(Inst_2)&&!llvm::dyn_cast<llvm::LoadInst>(Inst_2)->isVolatile()){
      auto op1=Inst.getOperand(0);
      auto op2=Inst_2->getOperand(0);
      auto type1=Inst.getType();
      auto type2=Inst_2->getType();
      if(op1==op2&&type1==type2){
        Inst_2->replaceAllUsesWith(&Inst);
        llvm::Instruction* swapv=Inst_2;
        Inst_2=Inst_2->getNextNonDebugInstruction();
        swapv->eraseFromParent();
        continue;
      }
    }
    Inst_2=Inst_2->getNextNonDebugInstruction();
  }
}


llvm::Instruction* sysu::CSE::cse_sto(llvm::Instruction &Inst, llvm::BasicAA::Result &basic_aa){
  //printf("sysu::CSE::cse_sto\n");
  llvm::Instruction* the_res=nullptr;
  llvm::Instruction* Inst_2=Inst.getNextNonDebugInstruction();
  llvm::AAQueryInfo AQ_info;
  while(Inst_2!=nullptr){
    //printf("1\n");
    if(llvm::isa<llvm::LoadInst>(Inst_2)&&!llvm::dyn_cast<llvm::LoadInst>(Inst_2)->isVolatile()){
      //printf("1-1\n");
      auto op1=Inst.getOperand(1);
      auto op2=Inst_2->getOperand(0);
      auto type1=Inst.getOperand(0)->getType();
      auto type2=Inst_2->getType();
      //printf("1-2\n");
      if(op1==op2 && type1==type2){
        //printf("1-3\n");
        Inst_2->replaceAllUsesWith(Inst.getOperand(0));
        llvm::Instruction *kk=Inst_2;
        Inst_2=Inst_2->getNextNonDebugInstruction();
        kk->eraseFromParent();
        continue;
      }
    }
    //printf("2\n");
    if(llvm::isa<llvm::StoreInst>(Inst_2)&&!llvm::dyn_cast<llvm::StoreInst>(&Inst)->isVolatile()){
      auto op1=Inst.getOperand(1);
      auto op2=Inst_2->getOperand(1);
      auto type1=Inst.getOperand(0)->getType();
      auto type2=Inst_2->getOperand(0)->getType();
      if(op1==op2 && type1==type2){
        the_res=Inst.getNextNonDebugInstruction();
        common_set.insert(Inst.getOperand(0));
        Inst.eraseFromParent();
        break;
      }
    }
    //printf("3\n");
    if(llvm::isa<llvm::CallInst>(Inst_2))break;
    if (llvm::isa<llvm::StoreInst>(Inst_2)) {
      if (auto cast=llvm::dyn_cast<llvm::StoreInst>(Inst_2)) {
        if(basic_aa.alias(llvm::MemoryLocation::get(cast),llvm::MemoryLocation::get(llvm::dyn_cast<llvm::StoreInst>(&Inst)),AQ_info))
        break;
      }
    }
    Inst_2=Inst_2->getNextNonDebugInstruction();
    ////printf("4\n");
  }
  if(the_res==nullptr)the_res=Inst.getNextNonDebugInstruction();
  //printf("ok\n");
  return the_res;
}

bool sysu::CSE::is_Deadcode(llvm::Instruction &Inst){
  //printf("sysu::CSE::is_Deadcode\n");
  if (Inst.hasNUsesOrMore(1))return false;
  auto opcode=Inst.getOpcode();
  switch(opcode){
    case llvm::Instruction::Load:
      return !llvm::dyn_cast<llvm::LoadInst>(&Inst)->isVolatile();
    case llvm::Instruction::Ret:
      return false;
    case llvm::Instruction::Resume:
      return false;
    case llvm::Instruction::Call:
      return false;
    case llvm::Instruction::Store:
      return false;
    case llvm::Instruction::AtomicRMW:
      return false;
    case llvm::Instruction::Br:
      return false;
    case llvm::Instruction::Switch:
      return false;
    case llvm::Instruction::AtomicCmpXchg:
      return false;
    case llvm::Instruction::Invoke:
      return false;
    case llvm::Instruction::Fence:
      return false;
    case llvm::Instruction::IndirectBr:
      return false;
    case llvm::Instruction::Unreachable:
      return false;
    case llvm::Instruction::LandingPad:
      return false;
    default:
      break;
  }
  return true;
}


void sysu::CSE::cse_fun(llvm::BasicBlock &basicblock,llvm::Instruction &Inst,llvm::Instruction *next_inst){
  //printf("sysu::CSE::cse_fun\n");
  llvm::Instruction *Inst_2=next_inst;
  if(!is_Valid(Inst))return;
  while(Inst_2!=nullptr){
    if(is_CSE(Inst,*Inst_2)){
      Inst_2->replaceAllUsesWith(&Inst);
      llvm::Instruction *nextInst_of_inst2=Inst_2;
      Inst_2=Inst_2->getNextNonDebugInstruction();
      nextInst_of_inst2->eraseFromParent();
      continue;
    }
    Inst_2=Inst_2->getNextNonDebugInstruction();
  }
  auto next_node=get_first(basicblock);
  while(next_node!=nullptr)next_node=get_next(basicblock,*next_node);
}



void sysu::CSE::deal_binaryoperator(llvm::Instruction &Inst){
  //printf("sysu::CSE::deal_binaryoperato\n");
  llvm::BinaryOperator *sign=llvm::dyn_cast<llvm::BinaryOperator>(&Inst);
  if(!sign)return;
  llvm::BinaryOperator *sign0=llvm::dyn_cast<llvm::BinaryOperator>(sign->getOperand(0));
  if(Inst.isAssociative()){
    while(sign0&&sign0->getOpcode()==Inst.getOpcode()){
      llvm::Value *op1=nullptr;
      llvm::Value *op2=nullptr;
      llvm::Value *op3=nullptr;
      llvm::Value *val=nullptr;
      if(common_set.find(sign0)!=common_set.end())break;
      op1=sign0->getOperand(0);
      op2=sign0->getOperand(1);
      op3=Inst.getOperand(1);
      llvm::IRBuilder<> Builder(Inst.getContext());
      Builder.SetInsertPoint(&Inst);
      val=Builder.CreateBinOp(sign->getOpcode(),op2,op3);
      Inst.setOperand(0,op1);
      Inst.setOperand(1,val);
      sign=llvm::dyn_cast<llvm::BinaryOperator>(&Inst);
      sign0=llvm::dyn_cast<llvm::BinaryOperator>(sign->getOperand(0));
    }
  }
}

void sysu::CSE::optimize_fun(llvm::Function &fun, llvm::BasicAA::Result &basic_aa){
  //printf("sysu::CSE::optimize_fun\n");
  std::vector<llvm::Instruction*> dele_vec;
  llvm::DenseSet<llvm::Instruction*> inst_set;
  for(auto &basicblock:fun){
    llvm::Instruction *Inst=&*(basicblock.getInstList().begin());
    while(Inst!=nullptr){
      if(is_Deadcode(*Inst)){
        inst_set.insert(Inst);
        Inst=Inst->getNextNonDebugInstruction();
        continue;
      }
      cse_fun(basicblock,*Inst,Inst->getNextNonDebugInstruction());
      if(llvm::isa<llvm::LoadInst>(Inst))cse_lod(*Inst,basic_aa);
      if(llvm::isa<llvm::StoreInst>(Inst)){Inst=cse_sto(*Inst,basic_aa);continue;}
      Inst=Inst->getNextNonDebugInstruction();
    }
  }
  while(inst_set.empty()!=true){
    auto kk=inst_set.begin();
    llvm::Instruction *Inst;
    if(kk!=inst_set.end()){Inst=*kk;inst_set.erase(kk);}
    if(is_Deadcode(*Inst)){
      for(int i=0;i<Inst->getNumOperands();++i){
        auto Inst_2=Inst->getOperand(i);
        if(llvm::isa<llvm::Instruction>(Inst_2)){
          inst_set.insert(llvm::dyn_cast<llvm::Instruction>(Inst_2));
        }
      }
      Inst->eraseFromParent();
    }
  }
  for(auto &basicblock:fun)
  for(auto &ins:basicblock)
  deal_binaryoperator(ins);

  for(auto &basicblock:fun)
  for(auto &ins:basicblock)
  cse_fun(basicblock,ins,ins.getNextNonDebugInstruction());
}

sysu::CSE::res sysu::CSE::run(llvm::Function &F,llvm::FunctionAnalysisManager &FAM){
  //printf("sysu::CSE::run\n");
  if(F.isDeclaration())return llvm::PreservedAnalyses::none();
  Tree=new llvm::DominatorTreeBase<llvm::BasicBlock,false>;
  auto &temp=FAM.getResult<llvm::BasicAA>(F);
  optimize_fun(F,temp);
  delete Tree;
  return llvm::PreservedAnalyses::all();
}

//#######################################################################




extern "C" {
llvm::PassPluginLibraryInfo LLVM_ATTRIBUTE_WEAK llvmGetPassPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "sysu-optimizer-pass", LLVM_VERSION_STRING,
          [](llvm::PassBuilder &PB) {
            // #1 REGISTRATION FOR "opt -passes=sysu-optimizer-pass"
            PB.registerPipelineParsingCallback(
                [&](llvm::StringRef Name, llvm::ModulePassManager &MPM,
                    llvm::ArrayRef<llvm::PassBuilder::PipelineElement>) {
                  if (Name == "sysu-optimizer-pass") {
                    MPM.addPass(sysu::StaticCallCounterPrinter(llvm::errs()));
                    //MPM.addPass(sysu::PreDel());
                    //MPM.addPass(sysu::CSE());
                    //MPM.addPass(sysu::DelLS());
                    //MPM.addPass(sysu::DCE());
                    //MPM.addPass(sysu::LID());
                    return true;
                  }
                  return false;
                });
            // #2 REGISTRATION FOR
            // "MAM.getResult<sysu::StaticCallCounter>(Module)"
            PB.registerAnalysisRegistrationCallback(
                [](llvm::ModuleAnalysisManager &MAM) {
                  MAM.registerPass([&] { return sysu::StaticCallCounter(); });
                });
          }};
}
}
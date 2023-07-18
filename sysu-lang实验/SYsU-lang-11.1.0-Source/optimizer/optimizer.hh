#pragma once
#ifndef __SYSU_OPTIMIZER_PLUGIN_HH_
#define __SYSU_OPTIMIZER_PLUGIN_HH_

#include <llvm/ADT/MapVector.h>
#include <llvm/IR/AbstractCallSite.h>
#include <llvm/IR/Module.h>
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Pass.h>
#include <llvm/Passes/PassPlugin.h>
#include <llvm/Support/raw_ostream.h>

namespace sysu {

class StaticCallCounter : public llvm::AnalysisInfoMixin<StaticCallCounter> {
public:
  using Result = llvm::MapVector<const llvm::Function *, unsigned>;
  Result run(llvm::Module &M, llvm::ModuleAnalysisManager &);

private:
  // A special type used by analysis passes to provide an address that
  // identifies that particular analysis pass type.
  static llvm::AnalysisKey Key;
  friend struct llvm::AnalysisInfoMixin<StaticCallCounter>;
};

class StaticCallCounterPrinter
    : public llvm::PassInfoMixin<StaticCallCounterPrinter> {
public:
  explicit StaticCallCounterPrinter(llvm::raw_ostream &OutS) : OS(OutS) {}
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM);
private:
  llvm::raw_ostream &OS;
};

//消除死代码
class DCE2:public llvm::PassInfoMixin<DCE2>{
public:
  using res=llvm::PreservedAnalyses;
  res run(llvm::Module &M, llvm::ModuleAnalysisManager &);
};

//消除死代码
class DCE:public llvm::PassInfoMixin<DCE>{
public:
  using res=llvm::PreservedAnalyses;
  res run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);
private:
  void dele_recursion(llvm::Instruction &Inst,llvm::DenseSet<llvm::Instruction*> &rm_list);
  void dele_alloc(llvm::Function &fun);
  void dele(llvm::Function &fun);
  bool is_Deadcode(llvm::Instruction &Inst);
  bool is_Valid(llvm::Instruction &Inst);
};


//指令简化
class Simp_Inst:public llvm::PassInfoMixin<Simp_Inst>{
public:
  using res=llvm::PreservedAnalyses;
  res run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);
private:
  void simply(llvm::Instruction *Inst);
};

//通用子表达式删除的预处理阶段
class PreDel:public llvm::PassInfoMixin<PreDel> {
public:
  using res=llvm::PreservedAnalyses;
  res run(llvm::Module &M, llvm::ModuleAnalysisManager &);
private:
  bool is_pre(llvm::Instruction &Inst);
};


//消除公共子表达式
class CSE2:public llvm::PassInfoMixin<CSE2>{
public:
  using res=llvm::PreservedAnalyses;
  res run(llvm::Module &M, llvm::ModuleAnalysisManager &);
private:
  bool is_CSE(llvm::Instruction &Inst1, llvm::Instruction &Inst2);
};



class CSE : public llvm::PassInfoMixin<CSE> {
public:
  using res=llvm::PreservedAnalyses;
  res run(llvm::Function &F,llvm::FunctionAnalysisManager &);
private:
  llvm::DenseSet<llvm::Value*> common_set;
  llvm::DenseSet<llvm::Instruction*> instru_set;
  llvm::DominatorTreeBase<llvm::BasicBlock, false> *Tree;
  llvm::Instruction* cse_sto(llvm::Instruction &Inst, llvm::BasicAA::Result &basic_aa);
  void cse_lod(llvm::Instruction &Inst, llvm::BasicAA::Result &basic_aa);
  void cse_fun(llvm::BasicBlock &basicblock,llvm::Instruction &Inst,llvm::Instruction *next_inst);
  void deal_binaryoperator(llvm::Instruction &Inst);
  void optimize_fun(llvm::Function &fun, llvm::BasicAA::Result &basic_aa);
  llvm::BasicBlock* get_first(llvm::BasicBlock &basicblock);
  llvm::BasicBlock* get_next(llvm::BasicBlock &basicblock, llvm::BasicBlock &child_node);
  bool is_CSE(llvm::Instruction &Inst1, llvm::Instruction &Inst2);
  bool is_Valid(llvm::Instruction &Inst);
  bool is_Deadcode(llvm::Instruction &Inst);
};

//指令合并
class Comb_Inst : public llvm::PassInfoMixin<Comb_Inst> {
public:
  using res = llvm::PreservedAnalyses;
  res run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);
  void make_comb(llvm::Instruction *Inst);
};

// 循环不变量
class LID : public llvm::PassInfoMixin<LID> {
public:
  using res=llvm::PreservedAnalyses;
  res run(llvm::Module &M, llvm::ModuleAnalysisManager &);
private:
  bool is_LoopInvar(llvm::Instruction &Loop, llvm::Instruction &Inst);
};



//减少内存访存
class DelLS : public llvm::PassInfoMixin<DelLS> {
public:
  using res=llvm::PreservedAnalyses;
  res run(llvm::Module &M, llvm::ModuleAnalysisManager &);
private:
  bool is_DeIls(llvm::BasicBlock::iterator &iter_inst);
};




} // namespace sysu



extern "C" {
llvm::PassPluginLibraryInfo LLVM_ATTRIBUTE_WEAK llvmGetPassPluginInfo();
}
#endif
#!/usr/bin/env python3
"""Apply all P1xP1 model changes to p1p1_closed_bps.cpp"""
import sys

with open('src/p1p1_closed_bps.cpp', 'r', encoding='utf-8') as f:
    content = f.read()

changes = 0

# 1. Class name: P2ClosedBPS -> P1P1ClosedBPS
old = 'class P2ClosedBPS {'
new = 'class P1P1ClosedBPS {'
assert old in content
content = content.replace(old, new)
changes += 1

# 2. Constructor name
old = 'explicit P2ClosedBPS(int maxDegree, int maxR = 0)'
new = 'explicit P1P1ClosedBPS(int maxDegree, int maxR = 0)'
assert old in content
content = content.replace(old, new)
changes += 1

# 3. Replace model functions (aTilde, qRShiftExp, rFluxS2)
old_funcs = '''  static int aTilde(int n) { return 3 * n * (3 * n + 1) / 2; }

  static int qRShiftExp(int n) { return n * (6 * n * n + 3 * n - 1) / 2; }

  static int rFluxS2(int beta, int n) {
    // 2 beta (n + 1/6) = 2 beta n + beta/3. Only beta divisible by 3 is nonzero.
    if (beta % 3 != 0) throw std::runtime_error("non-integral local P2 flux");
    return 2 * beta * n + beta / 3;
  }'''

new_funcs = '''  // ==== Model: local P1xP1 (F0), 1-parameter reduction ====
  // R(n) = n + R0,   R0 = R0_NUM / R0_DEN
  // aTilde(n)   = (A/2) [(n+R0)^2 - R0^2] = A n (R0_DEN n + 2 R0_NUM) / (2 R0_DEN)
  // rFluxS2     = 2 beta (n+R0) = 2 beta n + 2 beta R0
  // s2(n=0)     = 2 beta R0  (must be integer for all beta)
  // qRShiftExp  = (A/3)n^3 + A R0 n^2 + [A R0^2 - 2(b+b')] n
  //
  // Chosen parameters: A=6, R0=1/2, b+b'=3/4
  //   => aTilde(n)=3n(n+1),  rFluxS2=beta(2n+1),  qRShiftExp=n^2(2n+3)

  static constexpr int A      = 6;
  static constexpr int R0_NUM = 1;
  static constexpr int R0_DEN = 2;

  static int aTilde(int n) {
    return A * n * (R0_DEN * n + 2 * R0_NUM) / (2 * R0_DEN);
  }

  static int qRShiftExp(int n) {
    return n * n * ( (A / 3) * n + A * R0_NUM / R0_DEN );
  }

  static int rFluxS2(int beta, int n) {
    return 2 * beta * n + (2 * beta * R0_NUM) / R0_DEN;
  }'''

assert old_funcs in content
content = content.replace(old_funcs, new_funcs)
changes += 1

# 4. K function: remove `if (beta % 3 == 0)` guard, keep body
old = '''    Poly out;
    if (beta % 3 == 0) {
      const auto &inv = bps(beta);
      int s2 = rFluxS2(beta, n);
      for (const auto &kv : inv) {'''
new = '''    Poly out;
    {
      const auto &inv = bps(beta);
      int s2 = rFluxS2(beta, n);
      for (const auto &kv : inv) {'''
assert old in content
content = content.replace(old, new)
changes += 1

# 5. V function: remove `else if (beta % 3 != 0)` branch
old = '''    if (beta == 0) {
      out.addTerm(0, 0, Rat(1));
    } else if (beta % 3 != 0) {
      // Local P2 closed invariants are supported only on degrees divisible by 3.
    } else if (complete) {'''
new = '''    if (beta == 0) {
      out.addTerm(0, 0, Rat(1));
    } else if (complete) {'''
assert old in content
content = content.replace(old, new)
changes += 1

# 6. V function: change loop from `bp=3; bp<=beta; bp+=3` to `bp=1; bp<beta; bp+=1`
old = '''      for (int bp = 3; bp <= beta; bp += 3) {
        if (bp == beta) continue;'''
new = '''      for (int bp = 1; bp < beta; ++bp) {'''
assert old in content
content = content.replace(old, new)
changes += 1

# 7. solveBPS: remove beta%3 early return, fix s2
old = '''    if (beta % 3 != 0) {
      bps_[beta].clear();
      solved_[beta] = true;
      return;
    }

    Poly ii = I(beta).reflected();
    int s2 = -beta / 3;'''
new = '''    // P1xP1: invariants may exist for any beta >= 1
    Poly ii = I(beta).reflected();
    int s2 = -(2 * beta * R0_NUM) / R0_DEN;'''
assert old in content
content = content.replace(old, new)
changes += 1

# 8. solveWilson: remove beta%3 check, fix s2
old = '''    if (beta % 3 != 0) {
      throw std::runtime_error("non-integral Wilson s at beta " + std::to_string(beta) +
                               ", r " + std::to_string(r));
    }

    int s2 = beta / 3;'''
new = '''    int s2 = (2 * beta * R0_NUM) / R0_DEN;'''
assert old in content
content = content.replace(old, new)
changes += 1

# 9. main: change P2ClosedBPS -> P1P1ClosedBPS
old = 'P2ClosedBPS solver(degree, maxR);'
new = 'P1P1ClosedBPS solver(degree, maxR);'
assert old in content
content = content.replace(old, new)
changes += 1

# 10. main: change output labels from p2_ to p1p1_
old = 'p2_closed_cpp'
new = 'p1p1_closed_cpp'
n = content.count(old)
content = content.replace(old, new)
changes += n

old = 'p2_wilson_cpp'
new = 'p1p1_wilson_cpp'
n = content.count(old)
content = content.replace(old, new)
changes += n

with open('src/p1p1_closed_bps.cpp', 'w', encoding='utf-8') as f:
    f.write(content)

print(f"Applied {changes} changes to p1p1_closed_bps.cpp")

#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <cstdint>
#include <functional>
#include <iostream>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>

using i128 = __int128_t;
using Clock = std::chrono::steady_clock;

#ifndef P2_LEGACY_SPARSE
#define P2_LEGACY_SPARSE 0
#endif

static bool gProfileDetail = false;
#if !P2_LEGACY_SPARSE
static constexpr std::uint64_t kFlatMultiplyPairLimit = 16'000'000;
#endif

struct ProfileStats {
  double polyMulSeconds = 0.0;
  double addScaledSeconds = 0.0;
  double fTildeSeconds = 0.0;
  double ffTildeSeconds = 0.0;
  double kSeconds = 0.0;
  double vSeconds = 0.0;
  double iSeconds = 0.0;
  double decomposeSeconds = 0.0;
  std::uint64_t polyMulCalls = 0;
  std::uint64_t polyMulPairs = 0;
  std::uint64_t addScaledCalls = 0;
  std::uint64_t addScaledTerms = 0;
  std::uint64_t fTildeMisses = 0;
  std::uint64_t ffTildeMisses = 0;
  std::uint64_t kMisses = 0;
  std::uint64_t vMisses = 0;
  std::uint64_t maxTermCalls = 0;
  std::uint64_t decomposeSteps = 0;
};

static ProfileStats gProf;

struct ScopedProfileTimer {
  double *acc = nullptr;
  Clock::time_point start;

  explicit ScopedProfileTimer(double *target) : acc(gProfileDetail ? target : nullptr) {
    if (acc != nullptr) start = Clock::now();
  }

  ~ScopedProfileTimer() {
    if (acc != nullptr) *acc += std::chrono::duration<double>(Clock::now() - start).count();
  }
};

static void printProfileDetail() {
  if (!gProfileDetail) return;
  std::cout << "profile_detail poly_mul_seconds=" << gProf.polyMulSeconds
            << " calls=" << gProf.polyMulCalls
            << " term_pairs=" << gProf.polyMulPairs << "\n";
  std::cout << "profile_detail add_scaled_seconds=" << gProf.addScaledSeconds
            << " calls=" << gProf.addScaledCalls
            << " terms=" << gProf.addScaledTerms << "\n";
  std::cout << "profile_detail f_tilde_seconds=" << gProf.fTildeSeconds
            << " misses=" << gProf.fTildeMisses << "\n";
  std::cout << "profile_detail ff_tilde_seconds=" << gProf.ffTildeSeconds
            << " misses=" << gProf.ffTildeMisses << "\n";
  std::cout << "profile_detail K_seconds=" << gProf.kSeconds
            << " misses=" << gProf.kMisses << "\n";
  std::cout << "profile_detail V_seconds=" << gProf.vSeconds
            << " misses=" << gProf.vMisses << "\n";
  std::cout << "profile_detail I_seconds=" << gProf.iSeconds << "\n";
  std::cout << "profile_detail decompose_seconds=" << gProf.decomposeSeconds
            << " steps=" << gProf.decomposeSteps
            << " max_term_calls=" << gProf.maxTermCalls << "\n";
}

static i128 abs128(i128 x) { return x < 0 ? -x : x; }

static i128 gcd128(i128 a, i128 b) {
  a = abs128(a);
  b = abs128(b);
  while (b != 0) {
    i128 r = a % b;
    a = b;
    b = r;
  }
  return a == 0 ? 1 : a;
}

static std::string intToString(i128 x) {
  if (x == 0) return "0";
  bool neg = x < 0;
  if (neg) x = -x;
  std::string s;
  while (x > 0) {
    int d = static_cast<int>(x % 10);
    s.push_back(static_cast<char>('0' + d));
    x /= 10;
  }
  if (neg) s.push_back('-');
  std::reverse(s.begin(), s.end());
  return s;
}

struct Rat {
  i128 n = 0;
  i128 d = 1;

  Rat() = default;
  Rat(i128 num) : n(num), d(1) {}
  Rat(i128 num, i128 den) : n(num), d(den) { norm(); }

  void norm() {
    if (d == 0) throw std::runtime_error("zero denominator");
    if (n == 0) {
      d = 1;
      return;
    }
    if (d < 0) {
      n = -n;
      d = -d;
    }
    i128 g = gcd128(n, d);
    n /= g;
    d /= g;
  }

  bool isZero() const { return n == 0; }
  bool isInteger() const { return d == 1; }
  std::string str() const {
    if (d == 1) return intToString(n);
    return intToString(n) + "/" + intToString(d);
  }
};

static Rat operator+(const Rat &a, const Rat &b) { return Rat(a.n * b.d + b.n * a.d, a.d * b.d); }
static Rat operator-(const Rat &a) { return Rat(-a.n, a.d); }
static Rat operator*(const Rat &a, const Rat &b) { return Rat(a.n * b.n, a.d * b.d); }
static Rat operator/(const Rat &a, const Rat &b) { return Rat(a.n * b.d, a.d * b.n); }

struct Exp {
  int l = 0;
  int r = 0;
  bool operator<(const Exp &o) const {
    if (r != o.r) return r > o.r;
    return l > o.l;
  }
};

template <typename T>
static void hashCombine(std::size_t &seed, const T &value) {
  seed ^= std::hash<T>{}(value) + 0x9e3779b97f4a7c15ULL + (seed << 6) + (seed >> 2);
}

struct Tuple3Hash {
  template <typename A, typename B, typename C>
  std::size_t operator()(const std::tuple<A, B, C> &t) const {
    std::size_t seed = 0;
    hashCombine(seed, std::get<0>(t));
    hashCombine(seed, std::get<1>(t));
    hashCombine(seed, std::get<2>(t));
    return seed;
  }
};

struct Tuple4Hash {
  template <typename A, typename B, typename C, typename D>
  std::size_t operator()(const std::tuple<A, B, C, D> &t) const {
    std::size_t seed = 0;
    hashCombine(seed, std::get<0>(t));
    hashCombine(seed, std::get<1>(t));
    hashCombine(seed, std::get<2>(t));
    hashCombine(seed, std::get<3>(t));
    return seed;
  }
};

struct PairHash {
  template <typename A, typename B>
  std::size_t operator()(const std::pair<A, B> &p) const {
    std::size_t seed = 0;
    hashCombine(seed, p.first);
    hashCombine(seed, p.second);
    return seed;
  }
};

struct Term {
  int l;
  int r;
  Rat c;
};

struct IntTerm {
  int l;
  int r;
  i128 c;
};

struct FlatInputTerm {
  int l;
  int r;
  const Rat *c;
};

struct MulIndexTerm {
  int l;
  int r;
  std::uint32_t left;
  std::uint32_t right;
};

struct Poly {
  std::map<Exp, Rat> a;

  bool empty() const { return a.empty(); }

  void addTerm(int l, int r, const Rat &c) {
    if (c.isZero()) return;
    Exp e{l, r};
    auto it = a.find(e);
    if (it == a.end()) {
      a.emplace(e, c);
    } else {
      Rat v = it->second + c;
      if (v.isZero()) {
        a.erase(it);
      } else {
        it->second = v;
      }
    }
  }

  void addScaled(const Poly &p, const Rat &scale) {
    if (scale.isZero()) return;
    ScopedProfileTimer timer(&gProf.addScaledSeconds);
    if (gProfileDetail) {
      ++gProf.addScaledCalls;
      gProf.addScaledTerms += p.a.size();
    }
#if P2_LEGACY_SPARSE
    if (scale.d == 1 && scale.n == 1) {
      for (const auto &kv : p.a) addTerm(kv.first.l, kv.first.r, kv.second);
    } else if (scale.d == 1 && scale.n == -1) {
      for (const auto &kv : p.a) addTerm(kv.first.l, kv.first.r, -kv.second);
    } else {
      for (const auto &kv : p.a) addTerm(kv.first.l, kv.first.r, kv.second * scale);
    }
#else
    auto scaledCoeff = [&scale](const Rat &c) {
      if (scale.d == 1 && scale.n == 1) return c;
      if (scale.d == 1 && scale.n == -1) return -c;
      return c * scale;
    };

    if (a.empty()) {
      for (const auto &kv : p.a) {
        Rat c = scaledCoeff(kv.second);
        if (!c.isZero()) a.emplace(kv.first, c);
      }
      return;
    }

    if (p.a.size() * 8 < a.size()) {
      if (scale.d == 1 && scale.n == 1) {
        for (const auto &kv : p.a) addTerm(kv.first.l, kv.first.r, kv.second);
      } else if (scale.d == 1 && scale.n == -1) {
        for (const auto &kv : p.a) addTerm(kv.first.l, kv.first.r, -kv.second);
      } else {
        for (const auto &kv : p.a) addTerm(kv.first.l, kv.first.r, kv.second * scale);
      }
      return;
    }

    auto it = a.begin();
    for (const auto &kv : p.a) {
      while (it != a.end() && it->first < kv.first) ++it;
      Rat c = scaledCoeff(kv.second);
      if (c.isZero()) continue;
      if (it != a.end() && it->first.l == kv.first.l && it->first.r == kv.first.r) {
        Rat v = it->second + c;
        if (v.isZero()) {
          it = a.erase(it);
        } else {
          it->second = v;
          ++it;
        }
      } else {
        it = a.emplace_hint(it, kv.first, c);
        ++it;
      }
    }
#endif
  }

  Poly scaled(const Rat &scale) const {
    Poly out;
    out.addScaled(*this, scale);
    return out;
  }

  Poly shifted(int dl, int dr) const {
    Poly out;
    for (const auto &kv : a) out.a.emplace(Exp{kv.first.l + dl, kv.first.r + dr}, kv.second);
    return out;
  }

  Poly reflected() const {
    Poly out;
    for (const auto &kv : a) out.a.emplace(Exp{-kv.first.l, -kv.first.r}, kv.second);
    return out;
  }

  Poly scaleExponents(int k) const {
    Poly out;
    for (const auto &kv : a) out.a.emplace(Exp{k * kv.first.l, k * kv.first.r}, kv.second);
    return out;
  }

  bool maxTerm(Term &out) const {
    if (gProfileDetail) ++gProf.maxTermCalls;
    if (a.empty()) return false;
    const auto &kv = *a.begin();
    out = {kv.first.l, kv.first.r, kv.second};
    return true;
  }

  std::string str() const {
    if (a.empty()) return "0";
    std::ostringstream os;
    bool first = true;
    for (const auto &kv : a) {
      if (!first) os << " + ";
      first = false;
      os << kv.second.str() << "*qL^" << kv.first.l << "*qR^" << kv.first.r;
    }
    return os.str();
  }
};

struct IntIntervalPolyBuilder {
  struct RowDiff {
    std::unordered_map<int, i128> diff[2];
  };

  std::unordered_map<int, RowDiff> rows;

  static int parity(int x) { return x & 1; }

  static void addDiff(std::unordered_map<int, i128> &diff, int pos, i128 c) {
    auto it = diff.find(pos);
    if (it == diff.end()) {
      diff.emplace(pos, c);
    } else {
      it->second += c;
      if (it->second == 0) diff.erase(it);
    }
  }

  void reserveRows(std::size_t n) { rows.reserve(n); }

  void addChiTilde(int v, int qrExp, i128 coeff) {
    if (v == 0 || coeff == 0) return;
    i128 signedCoeff = v > 0 ? coeff : -coeff;
    int hi = (v > 0 ? v - 1 : -v - 1);
    int lo = -hi;
    auto &diff = rows[qrExp].diff[parity(lo)];
#if !P2_LEGACY_SPARSE
    if (diff.empty()) diff.reserve(32);
#endif
    addDiff(diff, lo, signedCoeff);
    addDiff(diff, hi + 2, -signedCoeff);
  }

  void addSumU(int start, int end, int chiBase, int chiUSign, int qrBase,
               int qrUSign, i128 coeff) {
    if (start > end) return;
    for (int u = start; u <= end; ++u) {
      addChiTilde(chiBase + chiUSign * u, qrBase + qrUSign * u, coeff);
    }
  }

  void addTilded(int m, int n, int chiBase, int chiUSign, int qrBase,
                 int qrUSign, i128 coeff) {
    if (n > m) {
      addSumU(m, n - 1, chiBase, chiUSign, qrBase, qrUSign, coeff);
    } else if (n < m) {
      addSumU(n, m - 1, chiBase, chiUSign, qrBase, qrUSign, -coeff);
    }
  }

  Poly toPoly() const {
#if P2_LEGACY_SPARSE
    Poly p;
    for (const auto &rowKv : rows) {
      int r = rowKv.first;
      for (const auto &diff : rowKv.second.diff) {
        if (diff.empty()) continue;
        std::vector<std::pair<int, i128>> events(diff.begin(), diff.end());
        std::sort(events.begin(), events.end(), [](const auto &a, const auto &b) {
          return a.first < b.first;
        });
        i128 cur = 0;
        int prev = events.front().first;
        bool havePrev = false;
        for (const auto &event : events) {
          int pos = event.first;
          if (havePrev && cur != 0) {
            for (int l = prev; l < pos; l += 2) {
              p.a.emplace(Exp{l, r}, Rat(cur));
            }
          }
          cur += event.second;
          prev = pos;
          havePrev = true;
        }
      }
    }
    return p;
#else
    std::vector<IntTerm> terms;
    for (const auto &rowKv : rows) {
      int r = rowKv.first;
      for (const auto &diff : rowKv.second.diff) {
        if (diff.empty()) continue;
        std::vector<std::pair<int, i128>> events(diff.begin(), diff.end());
        std::sort(events.begin(), events.end(), [](const auto &a, const auto &b) {
          return a.first < b.first;
        });
        i128 cur = 0;
        int prev = events.front().first;
        bool havePrev = false;
        for (const auto &event : events) {
          int pos = event.first;
          if (havePrev && cur != 0) {
            for (int l = prev; l < pos; l += 2) {
              terms.push_back({l, r, cur});
            }
          }
          cur += event.second;
          prev = pos;
          havePrev = true;
        }
      }
    }
    std::sort(terms.begin(), terms.end(), [](const IntTerm &x, const IntTerm &y) {
      if (x.r != y.r) return x.r > y.r;
      return x.l > y.l;
    });
    Poly p;
    for (const auto &t : terms) p.a.emplace_hint(p.a.end(), Exp{t.l, t.r}, Rat(t.c));
    return p;
#endif
  }
};

static Poly multiplyPoly(const Poly &a, const Poly &b) {
  ScopedProfileTimer timer(&gProf.polyMulSeconds);
  if (gProfileDetail) {
    ++gProf.polyMulCalls;
    gProf.polyMulPairs += static_cast<std::uint64_t>(a.a.size()) * static_cast<std::uint64_t>(b.a.size());
  }
#if !P2_LEGACY_SPARSE
  std::uint64_t pairs = static_cast<std::uint64_t>(a.a.size()) *
                        static_cast<std::uint64_t>(b.a.size());
  if (pairs <= kFlatMultiplyPairLimit) {
    std::vector<FlatInputTerm> left;
    std::vector<FlatInputTerm> right;
    left.reserve(a.a.size());
    right.reserve(b.a.size());
    for (const auto &x : a.a) left.push_back({x.first.l, x.first.r, &x.second});
    for (const auto &y : b.a) right.push_back({y.first.l, y.first.r, &y.second});

    std::vector<MulIndexTerm> terms;
    terms.reserve(static_cast<std::size_t>(pairs));
    for (std::uint32_t i = 0; i < left.size(); ++i) {
      for (std::uint32_t j = 0; j < right.size(); ++j) {
        terms.push_back({left[i].l + right[j].l, left[i].r + right[j].r, i, j});
      }
    }
    std::sort(terms.begin(), terms.end(), [](const MulIndexTerm &x, const MulIndexTerm &y) {
      if (x.r != y.r) return x.r > y.r;
      return x.l > y.l;
    });

    Poly out;
    for (std::size_t i = 0; i < terms.size();) {
      int l = terms[i].l;
      int r = terms[i].r;
      Rat c = *left[terms[i].left].c * *right[terms[i].right].c;
      ++i;
      while (i < terms.size() && terms[i].l == l && terms[i].r == r) {
        c = c + (*left[terms[i].left].c * *right[terms[i].right].c);
        ++i;
      }
      if (!c.isZero()) out.a.emplace(Exp{l, r}, c);
    }
    return out;
  }
#endif

  Poly out;
  for (const auto &x : a.a) {
    for (const auto &y : b.a) {
      out.addTerm(x.first.l + y.first.l, x.first.r + y.first.r, x.second * y.second);
    }
  }
  return out;
}

static i128 binomInt(int n, int k) {
  if (k < 0 || k > n) return 0;
  if (k > n - k) k = n - k;
  i128 out = 1;
  for (int i = 1; i <= k; ++i) {
    out = out * (n - k + i) / i;
  }
  return out;
}

static int parityMod2(int x) {
  int r = x % 2;
  return r < 0 ? r + 2 : r;
}

static Poly powPoly(Poly base, int exp) {
  Poly out;
  out.addTerm(0, 0, Rat(1));
  while (exp > 0) {
    if (exp & 1) out = multiplyPoly(out, base);
    exp >>= 1;
    if (exp > 0) base = multiplyPoly(base, base);
  }
  return out;
}

static Poly divideExact(Poly numerator, const Poly &denominator) {
  if (denominator.empty()) throw std::runtime_error("division by zero polynomial");
  if (denominator.a.size() == 1) {
    const auto &lead = *denominator.a.begin();
    Poly out;
    for (const auto &kv : numerator.a) {
      out.addTerm(kv.first.l - lead.first.l, kv.first.r - lead.first.r, kv.second / lead.second);
    }
    return out;
  }

  Poly quotient;
  Term denLead{};
  denominator.maxTerm(denLead);
  int guard = 0;
  int guardMax = 1000 + 20 * static_cast<int>(numerator.a.size() + denominator.a.size());
  while (!numerator.empty()) {
    if (++guard > guardMax) throw std::runtime_error("non-terminating polynomial division");
    Term numLead{};
    numerator.maxTerm(numLead);
    int dl = numLead.l - denLead.l;
    int dr = numLead.r - denLead.r;
    Rat coeff = numLead.c / denLead.c;
    quotient.addTerm(dl, dr, coeff);
    numerator.addScaled(denominator.shifted(dl, dr), -coeff);
  }
  return quotient;
}

static int mod3(int x) {
  int r = x % 3;
  return r < 0 ? r + 3 : r;
}

static int tt(int c, int p0, int p1, int p2) {
  int r = mod3(c);
  if (r == 0) return p0;
  if (r == 1) return p1;
  return p2;
}

class P1P1ClosedBPS {
	 public:
	  using Spin = std::pair<int, int>;  // doubled spins {2 jL, 2 jR}
	  using Inv = std::map<Spin, Rat>;

  explicit P1P1ClosedBPS(int maxDegree, int maxR = 0)
      : maxDegree_(maxDegree),
        maxR_(maxR),
        bps_(maxDegree + 1),
        solved_(maxDegree + 1, false),
        wilson_(maxDegree + 1, std::vector<Inv>(std::max(1, maxR + 1))),
        wilsonSolved_(maxDegree + 1, std::vector<bool>(std::max(1, maxR + 1), false)) {
#if !P2_LEGACY_SPARSE
    std::size_t d = static_cast<std::size_t>(maxDegree_ + 1);
    kCache_.reserve(d * 8);
    vCache_.reserve(d * 4);
    fCache_.reserve(d * d * 16);
    ffCache_.reserve(d * 4);
    kTildeCache_.reserve(d * static_cast<std::size_t>(std::max(1, maxR_ + 1)));
    wTildeCache_.reserve(d * static_cast<std::size_t>(std::max(1, maxR_ + 1)) * 2);
    kkCache_.reserve(d * 8);
    lhsCache_.reserve(d * static_cast<std::size_t>(std::max(1, maxR_ + 1)) * 2);
#endif
    if (maxR_ >= 1 && maxDegree_ >= 0) {
      wilson_[0][1][Spin{0, 0}] = Rat(1);
      wilsonSolved_[0][1] = true;
    }
  }

  const Inv &bps(int beta) {
    if (beta < 0 || beta > maxDegree_) throw std::runtime_error("beta out of range");
    if (!solved_[beta]) solveBPS(beta);
    return bps_[beta];
  }

	  void solveThrough(int degree) {
	    for (int d = 1; d <= degree; ++d) (void)bps(d);
	  }

  const Inv &wilsonBps(int beta, int r) {
    if (beta < 0 || beta > maxDegree_) throw std::runtime_error("wilson beta out of range");
    if (r <= 0 || r > maxR_) throw std::runtime_error("wilson r out of range");
    if (wilsonSolved_[beta][r]) return wilson_[beta][r];
    if (beta <= r) {
      wilson_[beta][r].clear();
      wilsonSolved_[beta][r] = true;
      return wilson_[beta][r];
    }
    solveWilson(beta, r);
    return wilson_[beta][r];
  }

  void solveWilsonThrough(int degree, int maxR) {
    for (int r = 1; r <= maxR; ++r) {
      (void)lambdaRR(r);
      for (int beta = 0; beta <= degree; ++beta) (void)wilsonBps(beta, r);
    }
  }

  void printNonEmpty(int degree) {
    for (int d = 1; d <= degree; ++d) {
      const auto &inv = bps(d);
      if (inv.empty()) continue;
      std::cout << "BPS[{" << d << "}] = ";
      printInv(inv);
      std::cout << "\n";
    }
  }

  void printWilsonNonEmpty(int degree, int maxR) {
    for (int r = 1; r <= maxR; ++r) {
      for (int beta = 0; beta <= degree; ++beta) {
        const auto &inv = wilsonBps(beta, r);
        if (inv.empty()) continue;
        std::cout << "W[{" << beta << "},{" << r << "}] = ";
        printInv(inv);
        std::cout << "\n";
      }
    }
  }

  static void printInv(const Inv &inv) {
    std::cout << "{";
    bool first = true;
    for (const auto &kv : inv) {
      if (!first) std::cout << ", ";
      first = false;
      std::cout << "{{" << spinStr(kv.first.first) << ", " << spinStr(kv.first.second)
                << "}, " << kv.second.str() << "}";
    }
    std::cout << "}";
  }

 private:
	  int maxDegree_;
  int maxR_;
	  std::vector<Inv> bps_;
	  std::vector<bool> solved_;
  std::vector<std::vector<Inv>> wilson_;
  std::vector<std::vector<bool>> wilsonSolved_;
#if P2_LEGACY_SPARSE
  std::map<std::tuple<int, int, int>, Poly> kCache_;
  std::map<std::tuple<int, int, bool>, Poly> vCache_;
  std::map<std::tuple<int, int, int>, Poly> fCache_;
  std::map<std::pair<int, int>, Poly> ffCache_;
#else
  std::unordered_map<std::tuple<int, int, int>, Poly, Tuple3Hash> kCache_;
  std::unordered_map<std::tuple<int, int, bool>, Poly, Tuple3Hash> vCache_;
  std::unordered_map<std::tuple<int, int, int>, Poly, Tuple3Hash> fCache_;
  std::unordered_map<std::pair<int, int>, Poly, PairHash> ffCache_;
#endif
  std::unordered_map<std::pair<int, int>, Poly, PairHash> kTildeCache_;
  std::unordered_map<std::tuple<int, int, bool>, Poly, Tuple3Hash> wTildeCache_;
  std::unordered_map<std::pair<int, int>, Poly, PairHash> kkCache_;
  std::unordered_map<std::tuple<int, int, bool>, Poly, Tuple3Hash> lhsCache_;
  std::unordered_map<std::pair<int, int>, Poly, PairHash> lFunctionCache_;
  std::unordered_map<int, std::map<int, Poly>> lambdaCache_;
  std::unordered_map<std::tuple<int, int, int, int>, Poly, Tuple4Hash> hCache_;
  std::unordered_map<int, Poly> dPairPowerCache_;
  std::unordered_map<int, Poly> dQ1PowerCache_;

  static std::string spinStr(int doubled) {
    if (doubled % 2 == 0) return std::to_string(doubled / 2);
    return std::to_string(doubled) + "/2";
  }

  // ==== Model: local P1xP1 (F0), 1-parameter reduction ====
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
  }

  static void addChiTilde(Poly &p, int v, int qrExp, const Rat &coeff) {
    if (v == 0 || coeff.isZero()) return;
    int sign = v > 0 ? 1 : -1;
    int hi = (v > 0 ? v - 1 : -v - 1);
    int lo = -hi;
    for (int e = lo; e <= hi; e += 2) p.addTerm(e, qrExp, coeff * Rat(sign));
  }

  static void addSumU(Poly &p, int start, int end, int chiBase, int chiUSign,
                      int qrBase, int qrUSign, const Rat &coeff) {
    if (start > end) return;
    for (int u = start; u <= end; ++u) {
      addChiTilde(p, chiBase + chiUSign * u, qrBase + qrUSign * u, coeff);
    }
  }

  static void addTilded(Poly &p, int m, int n, int chiBase, int chiUSign,
                        int qrBase, int qrUSign, const Rat &coeff) {
    if (n > m) {
      addSumU(p, m, n - 1, chiBase, chiUSign, qrBase, qrUSign, coeff);
    } else if (n < m) {
      addSumU(p, n, m - 1, chiBase, chiUSign, qrBase, qrUSign, -coeff);
    }
  }

  const Poly &fTilde(int jl2, int jr2, int s2) {
    auto key = std::make_tuple(jl2, jr2, s2);
    auto it = fCache_.find(key);
    if (it != fCache_.end()) return it->second;
    ScopedProfileTimer timer(&gProf.fTildeSeconds);
    if (gProfileDetail) ++gProf.fTildeMisses;
    IntIntervalPolyBuilder builder;
    builder.reserveRows(static_cast<std::size_t>(std::max(8, std::abs(s2) + jl2 + jr2 + 8)));
    for (int m1 = -jl2; m1 <= jl2; m1 += 2) {
      for (int m2 = -jr2; m2 <= jr2; m2 += 2) {
        int num = s2 - m1 + m2 + 1;
        if (num % 2 != 0) throw std::runtime_error("nonintegral fTilde bound");
        int lower = m2;
        int upper = num / 2;
        builder.addTilded(lower, upper, m1 + m2, -1, 0, 1, 1);
      }
    }
    Poly p = builder.toPoly();
    auto inserted = fCache_.emplace(key, std::move(p));
    return inserted.first->second;
  }

  const Poly &ffTilde(int jr42, int s2) {
    auto key = std::make_pair(jr42, s2);
    auto it = ffCache_.find(key);
    if (it != ffCache_.end()) return it->second;
    ScopedProfileTimer timer(&gProf.ffTildeSeconds);
    if (gProfileDetail) ++gProf.ffTildeMisses;
    Poly p;
    int J = jr42;

    addSumU(p, -J + 1, 0, -2 * s2 + 2 * J - 1, 1, s2, 1, Rat(1));
    addChiTilde(p, -2 * s2 + J - 2, s2 - J + 1, Rat(-1));

    for (int o = 1; o <= J / 3; ++o) {
      addSumU(p, -J + 3 * o - 1, -2, -2 * s2 + 2 * J - 6 * o + 5, 1, s2, 1, Rat(-1));
      addSumU(p, -J + 3 * o, -1, -2 * s2 + 2 * J - 6 * o + 1, 1, s2, 1, Rat(-1));
      if (J - 3 * o - s2 >= 2) {
        addSumU(p, -J + 3 * o + 1, 0, -2 * s2 + 2 * J - 6 * o - 3, 1, s2, 1, Rat(-1));
      }
      addSumU(p, -J + 3 * o, 0, -s2 + 2 * J - 6 * o + 1, 1, 0, 1, Rat(-1));
      addSumU(p, -J + 3 * o, -1, -s2 + 2 * J - 6 * o - 1, 1, 0, 1, Rat(-1));
      addSumU(p, -J + 3 * o, 0, -s2 + 2 * J - 6 * o + 3, 1, 0, 1, Rat(-1));
    }

    if (J >= 2) addChiTilde(p, -s2 + 2 * J - 1, 0, Rat(1));

    for (int k = -J + 4; k <= 0; ++k) {
      addSumU(p, -1 + k, tt(-k, 0, 0, -1), -s2 + 2 * J + 2 * k - 5, -1, 0, 1, Rat(1));
    }

    if (J >= 2) addChiTilde(p, -s2 + J - 2, -J + 1, Rat(1));
    if (J >= 3) {
      addChiTilde(p, -s2 + J - 1, -J + 2, Rat(1));
      addChiTilde(p, -s2 + J - 2, -J + 3, Rat(1));
    }

    for (int k = -J + 3; k <= 0; ++k) {
      addTilded(p, s2 + 1 + k, s2 + tt(-k, -1, 0, 1),
                -s2 + 2 * J + 2 * k - 3, -1, 0, 1, Rat(1));
    }

    if (J >= 2) {
      addTilded(p, s2 - J + 3, -J + 1, -s2 + 1, -1, 0, 1, Rat(1));
    }

    if (J >= 3) {
      int c = J - 3 * (J / 3);
      if (mod3(c) == 0) {
        addTilded(p, s2, 0, -s2 + 1, -1, 0, 1, Rat(-1));
        if (s2 <= -2) addTilded(p, s2 + 1, 0, -s2 - 1, -1, 0, 1, Rat(-1));
      } else if (mod3(c) == 1) {
        addChiTilde(p, -s2, -1, Rat(1));
        addTilded(p, s2 + 1, -1, -s2 + 1, -1, 0, 1, Rat(-1));
      }
    }

    for (int o = 1; o <= J / 3; ++o) {
      int kMax = std::min(J - 3 * o, 1);
      for (int k = 0; k <= kMax; ++k) {
        int uMax = std::min(J - 3 * o - 3, -J + 3 * o + 2 * k + 1);
        addSumU(p, -J + 3 * o, uMax, -s2 + 2 * k - 1, -1, 0, 1, Rat(1));
      }
    }

    auto inserted = ffCache_.emplace(key, std::move(p));
    return inserted.first->second;
  }

  const Poly &K(int k, int beta, int n) {
    auto key = std::make_tuple(k, beta, n);
    auto it = kCache_.find(key);
    if (it != kCache_.end()) return it->second;
    ScopedProfileTimer timer(&gProf.kSeconds);
    if (gProfileDetail) ++gProf.kMisses;
    Poly out;
    {
      const auto &inv = bps(beta);
      int s2 = rFluxS2(beta, n);
      for (const auto &kv : inv) {
        int jl2 = kv.first.first;
        int jr2 = kv.first.second;
        int parity = (jl2 + jr2) * (1 + k) + k;
        Rat coeff = kv.second * Rat((parity % 2 == 0) ? 1 : -1, k);
        if (k == 1) {
          out.addScaled(fTilde(jl2, jr2, s2), coeff);
        } else {
          out.addScaled(fTilde(jl2, jr2, s2).scaleExponents(k), coeff);
        }
      }
    }
    auto inserted = kCache_.emplace(key, std::move(out));
    return inserted.first->second;
  }

  const Poly &V(int beta, int n, bool complete) {
    auto key = std::make_tuple(beta, n, complete);
    auto it = vCache_.find(key);
    if (it != vCache_.end()) return it->second;
    ScopedProfileTimer timer(&gProf.vSeconds);
    if (gProfileDetail) ++gProf.vMisses;
    Poly out;
    if (beta == 0) {
      out.addTerm(0, 0, Rat(1));
    } else if (complete) {
      out = V(beta, n, false);
      out.addScaled(K(1, beta, n), Rat(1));
    } else {
      for (int bp = 1; bp < beta; ++bp) {
        for (int k = 1; k * bp <= beta; ++k) {
          const Poly &right = K(k, bp, n);
          if (right.empty()) continue;
          const Poly &left = V(beta - k * bp, n, true);
          if (left.empty()) continue;
          Poly prod = multiplyPoly(left, right);
          out.addScaled(prod, Rat(static_cast<i128>(k) * bp, beta));
        }
      }
    }
    auto inserted = vCache_.emplace(key, std::move(out));
    return inserted.first->second;
  }

	  Poly I(int beta) {
	    ScopedProfileTimer timer(&gProf.iSeconds);
	    Poly out;
    int lo = 0;
    int hi = 0;
    while (aTilde(lo - 1) <= beta) --lo;
    while (aTilde(hi + 1) <= beta) ++hi;
    for (int n = lo; n <= hi; ++n) {
      int a = aTilde(n);
      if (a < 0 || a > beta) continue;
      Poly term = V(beta - a, n, a != 0).shifted(0, qRShiftExp(n));
      out.addScaled(term, Rat((n % 2 == 0) ? 1 : -1));
	    }
	    return out;
	  }

  static Poly toQ1Q2(const Poly &qlqr) {
    Poly out;
    for (const auto &kv : qlqr.a) {
      int qlExp = kv.first.l;
      int qrExp = kv.first.r;
      out.addTerm(qlExp + qrExp, -qlExp + qrExp, kv.second);
    }
    return out;
  }

  static Poly substituteQ2OverQ1(const Poly &p) {
    Poly out;
    for (const auto &kv : p.a) {
      out.addTerm(kv.first.l - kv.first.r, kv.first.r, kv.second);
    }
    return out;
  }

  const Poly &dPairPower(int exp) {
    auto it = dPairPowerCache_.find(exp);
    if (it != dPairPowerCache_.end()) return it->second;
    Poly base;
    base.addTerm(1, 1, Rat(1));
    base.addTerm(1, -1, Rat(-1));
    base.addTerm(-1, 1, Rat(-1));
    base.addTerm(-1, -1, Rat(1));
    Poly p = powPoly(base, exp);
    auto inserted = dPairPowerCache_.emplace(exp, std::move(p));
    return inserted.first->second;
  }

  const Poly &dQ1Power(int exp) {
    auto it = dQ1PowerCache_.find(exp);
    if (it != dQ1PowerCache_.end()) return it->second;
    Poly base;
    base.addTerm(1, 0, Rat(1));
    base.addTerm(-1, 0, Rat(-1));
    Poly p = powPoly(base, exp);
    auto inserted = dQ1PowerCache_.emplace(exp, std::move(p));
    return inserted.first->second;
  }

  const Poly &KK(int n, int beta) {
    auto key = std::make_pair(n, beta);
    auto it = kkCache_.find(key);
    if (it != kkCache_.end()) return it->second;
    Poly shifted = V(beta, n, true).shifted(0, qRShiftExp(n));
    Poly p = toQ1Q2(shifted);
    auto inserted = kkCache_.emplace(key, std::move(p));
    return inserted.first->second;
  }

  const Poly &KTilde(int beta, int r) {
    auto key = std::make_pair(beta, r);
    auto it = kTildeCache_.find(key);
    if (it != kTildeCache_.end()) return it->second;
    Poly out;
    const auto &inv = wilsonBps(beta, r);
    for (const auto &kv : inv) {
      int jl2 = kv.first.first;
      int jr2 = kv.first.second;
      Rat coeff = kv.second * Rat(parityMod2(jl2 + jr2 + beta - r) == 0 ? 1 : -1);
      Poly spin;
      for (int ml = -jl2; ml <= jl2; ml += 2) {
        for (int mr = -jr2; mr <= jr2; mr += 2) {
          spin.addTerm(ml + mr, -ml + mr, coeff);
        }
      }
      if (r > 1) {
        out.addScaled(multiplyPoly(spin, dPairPower(r - 1)), Rat(1));
      } else {
        out.addScaled(spin, Rat(1));
      }
    }
    auto inserted = kTildeCache_.emplace(key, std::move(out));
    return inserted.first->second;
  }

  const Poly &WTilde(int beta, int r, bool complete) {
    auto key = std::make_tuple(beta, r, complete);
    auto it = wTildeCache_.find(key);
    if (it != wTildeCache_.end()) return it->second;
    Poly out;
    if (r == 0) {
      if (beta == 0) out.addTerm(0, 0, Rat(1));
    } else if (complete) {
      out = WTilde(beta, r, false);
      out.addScaled(KTilde(beta, r), Rat(1));
    } else {
      for (int bp = 0; bp <= beta; ++bp) {
        for (int rp = 1; rp <= r; ++rp) {
          if (rp == r) continue;
          const Poly &left = WTilde(bp, r - rp, true);
          if (left.empty()) continue;
          const Poly &right = KTilde(beta - bp, rp);
          if (right.empty()) continue;
          Poly prod = multiplyPoly(left, right);
          out.addScaled(prod, Rat(binomInt(r - 1, rp - 1)));
        }
      }
    }
    auto inserted = wTildeCache_.emplace(key, std::move(out));
    return inserted.first->second;
  }

  const Poly &LHS(int beta, int r, bool complete) {
    auto key = std::make_tuple(beta, r, complete);
    auto it = lhsCache_.find(key);
    if (it != lhsCache_.end()) return it->second;
    Poly out;
    int lo = 0;
    int hi = 0;
    while (aTilde(lo - 1) <= beta) --lo;
    while (aTilde(hi + 1) <= beta) ++hi;
    for (int n = lo; n <= hi; ++n) {
      int a = aTilde(n);
      if (a < 0 || a > beta) continue;
      int beta0 = beta - a;
      for (int bp = 0; bp <= beta0; ++bp) {
        const Poly &wRaw = WTilde(bp, r, complete || bp != beta);
        if (wRaw.empty()) continue;
        const Poly &kk = KK(n, beta0 - bp);
        if (kk.empty()) continue;
        if (bp % 3 != 0) {
          throw std::runtime_error("fractional q1 exponent in Wilson LHS at beta " +
                                   std::to_string(beta) + ", r " + std::to_string(r));
        }
        Poly w = substituteQ2OverQ1(wRaw);
        Poly prod = multiplyPoly(w, kk);
        int q1Shift = -2 * r * n + 2 * bp * n + (2 * bp * R0_NUM) / R0_DEN;
        prod = prod.shifted(q1Shift, 0);
        out.addScaled(prod, Rat((n % 2 == 0) ? 1 : -1));
      }
    }
    auto inserted = lhsCache_.emplace(key, std::move(out));
    return inserted.first->second;
  }

  const std::map<int, Poly> &solveLambda(int r) {
    auto it = lambdaCache_.find(r);
    if (it != lambdaCache_.end()) return it->second;
    std::map<int, Poly> lambdas;
    for (int rp = r; rp >= 0; --rp) {
      Poly numerator = LHS(r - rp, r, true);
      for (const auto &kv : lambdas) {
        int lamR = kv.first;
        if (rp > lamR) continue;
        const Poly &w = WTilde(lamR - rp, lamR, true);
        if (w.empty()) continue;
        Poly prod = multiplyPoly(kv.second, w);
        numerator.addScaled(prod, Rat(-1));
      }
      Poly lambda = divideExact(numerator, WTilde(0, rp, true));
      if (!lambda.empty()) lambdas.emplace(rp, std::move(lambda));
    }
    auto inserted = lambdaCache_.emplace(r, std::move(lambdas));
    return inserted.first->second;
  }

  const Poly &lambdaRR(int r) {
    const auto &lambdas = solveLambda(r);
    auto it = lambdas.find(r);
    if (it == lambdas.end()) {
      static const Poly empty;
      return empty;
    }
    return it->second;
  }

  const Poly &LFunction(int beta, int r) {
    auto key = std::make_pair(beta, r);
    auto it = lFunctionCache_.find(key);
    if (it != lFunctionCache_.end()) return it->second;
    const auto &lambdas = solveLambda(r);
    Poly out = LHS(beta, r, false);
    for (const auto &kv : lambdas) {
      int lamR = kv.first;
      int wBeta = lamR + beta - r;
      if (wBeta < 0) continue;
      const Poly &w = WTilde(wBeta, lamR, lamR != r);
      if (w.empty()) continue;
      Poly prod = multiplyPoly(kv.second, w);
      out.addScaled(prod, Rat(-1));
    }
    auto inserted = lFunctionCache_.emplace(key, std::move(out));
    return inserted.first->second;
  }

  const Poly &hStd(int jl2, int jr2, int s2, int r) {
    auto key = std::make_tuple(jl2, jr2, s2, r);
    auto it = hCache_.find(key);
    if (it != hCache_.end()) return it->second;
    Poly out;
    int rt2 = r - 1;
    const Poly &lambda = lambdaRR(r);
    for (int m0 = -rt2; m0 <= rt2; m0 += 2) {
      int choose = (rt2 - m0) / 2;
      i128 absCoeff = binomInt(rt2, choose);
      if (parityMod2(choose) == 1) absCoeff = -absCoeff;
      Rat coeff(absCoeff);
      for (int m1 = -jl2; m1 <= jl2; m1 += 2) {
        for (int m2 = -jr2; m2 <= jr2; m2 += 2) {
          int q2Exp = m0 + m1 + m2;
          if (!lambda.empty()) {
            out.addScaled(lambda.shifted(-m1 + m2, q2Exp), coeff);
          }
          out.addTerm(s2 - m0 - 2 * m1, q2Exp, -coeff);
        }
      }
    }
    auto inserted = hCache_.emplace(key, std::move(out));
    return inserted.first->second;
  }

  static bool inverseMaxBasisH(int n1, int n2, int type, int s2, int rt2,
                               int l1max, int l2max, const Rat &cmax,
                               int &jl2, int &jr2, Rat &coef) {
    if (type == 0) {
      int a = s2 - rt2 - n1;
      int b = 2 * n2 - s2 - rt2 + n1;
      if (a % 2 != 0 || b % 2 != 0) return false;
      jl2 = a / 2;
      jr2 = b / 2;
      coef = Rat(-1);
      return true;
    }
    int a = n2 - n1 - l2max + l1max - rt2;
    int b = n2 + n1 - l2max - l1max - rt2;
    if (a % 2 != 0 || b % 2 != 0) return false;
    jl2 = a / 2;
    jr2 = b / 2;
    if (l1max == 0 && l2max == 0 && s2 == 0 && rt2 == 0 && jl2 == 0 && jr2 == 0) {
      coef = cmax + Rat(-1);
    } else {
      coef = cmax;
    }
    return true;
  }

	  static bool inverse0(int u, int v, int s2, int &jl2, int &jr2, Rat &coef) {
    if (s2 > 0) return false;
    if ((u + v + s2 + 1) % 2 != 0) return false;
    if (s2 == -1 && u == -1 && v == 1) return false;
    if (v >= u - s2 + 1 && v <= 3 * u - s2 + 1) {
      int a = u + v - s2 + 1;
      int b = 3 * u - v - s2 + 1;
      if (a % 2 != 0 || b % 2 != 0) return false;
      jl2 = a / 2;
      jr2 = b / 2;
      coef = Rat(-1);
      return true;
    }
    if (u >= -1 && v >= 1 && v <= u - s2 + 1) {
      jl2 = v - 1;
      jr2 = u + 1;
      coef = Rat(-1);
      return true;
    }
    return false;
  }

  static bool inverse1(int u, int v, int s2, int &jr42, Rat &coef) {
    if (s2 <= -1 && u == s2 && v == -2 * s2 + 1) {
      jr42 = 1;
      coef = Rat(1);
      return true;
    }
    if (s2 == 0 && u == 0 && (v + 1) % 2 == 0 && Rat(v + 1, 4).n >= Rat(1).n) {
      jr42 = (v + 1) / 2;
      coef = Rat(2);
      return true;
    }
    if (s2 < 0 && u == 0 && (v + s2 + 1) % 2 == 0 && Rat(v + s2 + 1, 4).n >= Rat(1).n) {
      jr42 = (v + s2 + 1) / 2;
      coef = Rat(1);
      return true;
    }
    return false;
  }

  static void shiftInv(Inv &inv, int jl2, int jr2, const Rat &delta) {
    if (jl2 < 0 || jr2 < 0 || delta.isZero()) return;
    Spin key{jl2, jr2};
    auto it = inv.find(key);
    if (it == inv.end()) {
      inv.emplace(key, delta);
    } else {
      Rat v = it->second + delta;
      if (v.isZero()) {
        inv.erase(it);
      } else {
        it->second = v;
      }
    }
  }

	  void solveBPS(int beta) {
    if (beta <= 0) {
      solved_[beta] = true;
      return;
    }
    // P1xP1: invariants may exist for any beta >= 1
    Poly ii = I(beta).reflected();
    int s2 = -(2 * beta * R0_NUM) / R0_DEN;
    Inv inv;
    int guard = 0;
    int guardMax = 100 + 3 * beta * beta * beta * beta;
    Term first{};

    {
      ScopedProfileTimer decomposeTimer(&gProf.decomposeSeconds);
      while (ii.maxTerm(first) && guard++ <= guardMax) {
        if (gProfileDetail) ++gProf.decomposeSteps;
      if (first.l < 0) throw std::runtime_error("bad maximum basis at beta " + std::to_string(beta));
      int u = first.r;
      int v = first.l + 1;
      int jl2 = 0, jr2 = 0;
      Rat coef;
      if (inverse0(u, v, s2, jl2, jr2, coef)) {
        Rat ratio = first.c / coef;
        const Poly &basis = fTilde(jl2, jr2, s2);
        ii.addScaled(basis, -ratio);
        shiftInv(inv, jl2, jr2, ratio);
      } else {
        int jr42 = 0;
        if (!inverse1(u, v, s2, jr42, coef)) {
          throw std::runtime_error("cannot invert repeated maximum basis at beta " + std::to_string(beta));
        }
        Rat ratio = first.c / coef;
        const Poly &basis = ffTilde(jr42, s2);
        ii.addScaled(basis, -ratio);
        shiftInv(inv, jr42 - s2 - 1, jr42, ratio);
        shiftInv(inv, jr42 - s2, jr42 - 1, -ratio);
        int oMax = jr42 / 3 + 1;
        for (int o = 1; o <= oMax; ++o) {
          shiftInv(inv, jr42 - s2 - 3 * o, jr42 - 3 * o + 3, -ratio);
          shiftInv(inv, jr42 - s2 - 3 * o + 3, jr42 - 3 * o, ratio);
          shiftInv(inv, jr42 - s2 - 3 * o - 1, jr42 - 3 * o + 2, ratio);
          shiftInv(inv, jr42 - s2 - 3 * o + 2, jr42 - 3 * o - 1, -ratio);
        }
      }
      }
    }
    if (!ii.empty()) throw std::runtime_error("decomposition did not terminate at beta " + std::to_string(beta));
	    bps_[beta] = inv;
	    solved_[beta] = true;
	  }

  void solveWilson(int beta, int r) {
    Poly lll = divideExact(LFunction(beta, r), dQ1Power(r - 1));
    Inv inv;
    if (lll.empty()) {
      wilson_[beta][r] = inv;
      wilsonSolved_[beta][r] = true;
      return;
    }
    int s2 = (2 * beta * R0_NUM) / R0_DEN;
    int rt2 = r - 1;
    const Poly &lambda = lambdaRR(r);
    int type = 0;
    int l1max = 0;
    int l2max = 0;
    Rat cmax(0);
    Term lambdaLead{};
    if (lambda.maxTerm(lambdaLead)) {
      l1max = lambdaLead.l;
      l2max = lambdaLead.r;
      cmax = lambdaLead.c;
      type = (l2max > 0 || s2 <= 0) ? 1 : 2;
    }
    if (type == 2) {
      s2 = -s2;
      lll = lll.reflected();
      if (parityMod2(r - 1) == 1) lll = lll.scaled(Rat(-1));
    }

    int guard = 0;
    int guardMax = 100 + 3 * (beta + r) * (beta + r) * (beta + r) * (beta + r);
    Term first{};
    while (lll.maxTerm(first) && guard++ <= guardMax) {
      int jl2 = 0;
      int jr2 = 0;
      Rat coef;
      if (!inverseMaxBasisH(first.l, first.r, type, s2, rt2, l1max, l2max, cmax, jl2, jr2, coef) ||
          jl2 < 0 || jr2 < 0 || coef.isZero()) {
        throw std::runtime_error("cannot invert Wilson maximum basis at beta " +
                                 std::to_string(beta) + ", r " + std::to_string(r));
      }
      Rat ratio = first.c / coef;
      const Poly &basis = hStd(jl2, jr2, s2, r);
      lll.addScaled(basis, -ratio);
      Rat signedRatio = ratio * Rat(parityMod2(jl2 + jr2 + beta - r) == 0 ? 1 : -1);
      shiftInv(inv, jl2, jr2, signedRatio);
    }
    if (!lll.empty()) {
      throw std::runtime_error("Wilson decomposition did not terminate at beta " +
                               std::to_string(beta) + ", r " + std::to_string(r));
    }
    wilson_[beta][r] = inv;
    wilsonSolved_[beta][r] = true;
  }
};

int main(int argc, char **argv) {
  int degree = 30;
  int maxR = 0;
  bool print = true;
  bool profile = false;
  bool wilson = false;
  for (int i = 1; i < argc; ++i) {
    std::string arg = argv[i];
    if (arg == "--degree" && i + 1 < argc) {
      degree = std::atoi(argv[++i]);
    } else if (arg == "--max-r" && i + 1 < argc) {
      maxR = std::atoi(argv[++i]);
      wilson = true;
    } else if (arg == "--r" && i + 1 < argc) {
      maxR = std::atoi(argv[++i]);
      wilson = true;
    } else if (arg == "--wilson") {
      wilson = true;
      if (maxR == 0) maxR = 1;
    } else if (arg == "--quiet") {
      print = false;
    } else if (arg == "--profile") {
      profile = true;
    } else if (arg == "--profile-detail") {
      profile = true;
      gProfileDetail = true;
    }
  }
  if (wilson && maxR <= 0) maxR = 1;

  auto t0 = std::chrono::steady_clock::now();
  P1P1ClosedBPS solver(degree, maxR);
  if (wilson) {
    solver.solveThrough(degree);
    if (profile) {
      double cumulative = 0.0;
      for (int r = 1; r <= maxR; ++r) {
        auto r0 = std::chrono::steady_clock::now();
        (void)solver.wilsonBps(0, r);
        for (int beta = 1; beta <= degree; ++beta) (void)solver.wilsonBps(beta, r);
        auto r1 = std::chrono::steady_clock::now();
        double seconds = std::chrono::duration<double>(r1 - r0).count();
        cumulative += seconds;
        std::cout << "p1p1_wilson_cpp_r_step=" << r
                  << " seconds=" << seconds
                  << " cumulative=" << cumulative << std::endl;
      }
    } else {
      solver.solveWilsonThrough(degree, maxR);
    }
  } else if (profile) {
    double cumulative = 0.0;
    for (int d = 1; d <= degree; ++d) {
      auto d0 = std::chrono::steady_clock::now();
      const auto &inv = solver.bps(d);
      auto d1 = std::chrono::steady_clock::now();
      double seconds = std::chrono::duration<double>(d1 - d0).count();
      cumulative += seconds;
      std::cout << "p1p1_closed_cpp_degree_step=" << d
                << " seconds=" << seconds
                << " cumulative=" << cumulative
                << " sectors=" << inv.size() << std::endl;
    }
  } else {
    solver.solveThrough(degree);
  }
  auto t1 = std::chrono::steady_clock::now();
  double seconds = std::chrono::duration<double>(t1 - t0).count();

  if (print) {
    if (wilson) {
      solver.printWilsonNonEmpty(degree, maxR);
    } else {
      solver.printNonEmpty(degree);
    }
  }
  printProfileDetail();
  if (wilson) {
    std::cout << "p1p1_wilson_cpp_degree=" << degree << " max_r=" << maxR
              << " seconds=" << seconds << "\n";
  } else {
    std::cout << "p1p1_closed_cpp_degree=" << degree << " seconds=" << seconds << "\n";
  }
  return 0;
}

BeginPackage["BlowupBPS`"];

SetBlowupModel::usage =
  "SetBlowupModel[model] installs a classical blowup-equation geometry association.";
CurrentModel::usage = "CurrentModel[] returns the installed geometry association.";
ResetInvariants::usage =
  "ResetInvariants[] clears cached closed and Wilson-loop BPS invariants.";
LocalP2Model::usage = "LocalP2Model[] returns the local P2 geometry used by the reference notebooks.";
LocalF0Model::usage =
  "LocalF0Model[] returns a placeholder for local F0; fill and validate the classical input before using.";
LocalF1Model::usage =
  "LocalF1Model[] returns a placeholder for local F1; fill and validate the classical input before using.";
BPSList::usage =
  "BPSList[beta] computes {{jL,jR},N} refined closed BPS invariants for a curve-degree vector beta.";
WilsonLoopBPSList::usage =
  "WilsonLoopBPSList[beta,r] computes {{jL,jR},N} Wilson-loop BPS invariants.";
SolveBPSDegree::usage =
  "SolveBPSDegree[d] computes closed BPS invariants for all nonnegative beta with total degree d.";
SolveWilsonLoopBox::usage =
  "SolveWilsonLoopBox[maxBeta,maxR] computes Wilson-loop data for one-parameter beta<=maxBeta and r<=maxR.";
SaveState::usage = "SaveState[file] saves the current invariant caches.";
LoadState::usage = "LoadState[file] loads invariant caches saved by SaveState.";

Begin["`Private`"];

$Model = Missing["NoModel"];
$BPSInvariants = <||>;
$WilsonLoopBPSInvariants = <||>;
$LambdaRRPrime = <||>;

memoizedSymbols = {
  betaVectors, aTilde, bTildeExp, fTildeStd, fTildeSorted,
  ffTildeStd, ffTildeSorted, KfunctionCore, VfunctionCore,
  Ifunction, KTildeFunctionCore, WTildeFunction0, WTildeFunction1,
  KKCore, LHSCore, LFunctionCore, LambdaRR, hStd
};

patternDefinitionQ[rule_] := !FreeQ[
  Unevaluated[rule[[1]]],
  Pattern | Blank | BlankSequence | BlankNullSequence | PatternTest | Condition
];

clearMemoizedSymbol[s_Symbol] := DownValues[s] = Select[DownValues[s], patternDefinitionQ];

ClearMemoized[] := Scan[clearMemoizedSymbol, memoizedSymbols];

CurrentModel[] := $Model;

requiredModelKeys = {
  "Name", "DimFlux", "DimBeta", "TripleIntersection",
  "LinearB", "LinearBPrime", "CartanMatrix", "R0Vector", "BField"
};

SetBlowupModel::missing = "Model is missing required keys: `1`.";
SetBlowupModel::dims = "Model dimensions are inconsistent.";
SetBlowupModel::unverified =
  "Model `1` is marked unverified. Supply validated classical input, or pass \"AllowUnverified\" -> True.";

Options[SetBlowupModel] = {"AllowUnverified" -> False};

SetBlowupModel[model_Association, OptionsPattern[]] := Module[
  {missing, dimFlux, dimBeta, triple, linearB, linearBPrime, cartan, r0, bfield},
  missing = Complement[requiredModelKeys, Keys[model]];
  If[missing =!= {}, Message[SetBlowupModel::missing, missing]; Return[$Failed]];

  dimFlux = model["DimFlux"];
  dimBeta = model["DimBeta"];
  triple = model["TripleIntersection"];
  linearB = model["LinearB"];
  linearBPrime = model["LinearBPrime"];
  cartan = model["CartanMatrix"];
  r0 = model["R0Vector"];
  bfield = model["BField"];

  If[
    !IntegerQ[dimFlux] || !IntegerQ[dimBeta] ||
      Dimensions[triple] =!= {dimBeta, dimBeta, dimBeta} ||
      Length[linearB] =!= dimBeta || Length[linearBPrime] =!= dimBeta ||
      Dimensions[cartan] =!= {dimBeta, dimFlux} ||
      Length[r0] =!= dimBeta || Length[bfield] =!= dimBeta,
    Message[SetBlowupModel::dims];
    Return[$Failed]
  ];

  If[TrueQ[Lookup[model, "Verified", True] === False] && !TrueQ[OptionValue["AllowUnverified"]],
    Message[SetBlowupModel::unverified, model["Name"]];
    Return[$Failed]
  ];

  $Model = model;
  ResetInvariants[];
  $Model
];

ResetInvariants[] := Module[{initWilson},
  ClearMemoized[];
  $BPSInvariants = <||>;
  initWilson = Lookup[$Model, "InitialWilsonInvariants", <||>];
  $WilsonLoopBPSInvariants = initWilson;
  $LambdaRRPrime = <||>;
  Null
];

LocalP2Model[] := <|
  "Name" -> "local P2",
  "Verified" -> True,
  "DimFlux" -> 1,
  "DimBeta" -> 1,
  "TripleIntersection" -> {{{9}}},
  "LinearB" -> {1/8},
  "LinearBPrime" -> {1/4},
  "CartanMatrix" -> {{1}},
  "R0Vector" -> {1/3},
  "BField" -> {1},
  "InitialWilsonInvariants" -> <|{{0}, {1}} -> <|{0, 0} -> 1|>|>
|>;

LocalF0Model[] := <|
  "Name" -> "local F0",
  "Verified" -> False,
  "DimFlux" -> 2,
  "DimBeta" -> 2,
  "TripleIntersection" -> ConstantArray[0, {2, 2, 2}],
  "LinearB" -> {0, 0},
  "LinearBPrime" -> {0, 0},
  "CartanMatrix" -> IdentityMatrix[2],
  "R0Vector" -> {0, 0},
  "BField" -> {0, 0},
  "InitialWilsonInvariants" -> <||>,
  "Status" -> "placeholder: replace by validated local F0 blowup data"
|>;

LocalF1Model[] := <|
  "Name" -> "local F1",
  "Verified" -> False,
  "DimFlux" -> 2,
  "DimBeta" -> 2,
  "TripleIntersection" -> ConstantArray[0, {2, 2, 2}],
  "LinearB" -> {0, 0},
  "LinearBPrime" -> {0, 0},
  "CartanMatrix" -> IdentityMatrix[2],
  "R0Vector" -> {0, 0},
  "BField" -> {0, 0},
  "InitialWilsonInvariants" -> <||>,
  "Status" -> "placeholder: replace by validated local F1 blowup data"
|>;

dimBeta[] := $Model["DimBeta"];
dimFlux[] := $Model["DimFlux"];
zeroBeta[] := ConstantArray[0, dimBeta[]];
zeroFlux[] := ConstantArray[0, dimFlux[]];
qVars[] := Array[Global`Q, dimBeta[]];

rVector[n_List] := $Model["CartanMatrix"].n + $Model["R0Vector"]/2;

allNoMoreThan[aa_List, bb_List] := And @@ Thread[aa <= bb];

weakCompositions[n_Integer?NonNegative, k_Integer?Positive] := Module[{cuts},
  If[k == 1, Return[{{n}}]];
  cuts = Subsets[Range[n + k - 1], {k - 1}];
  ({First[#] - 1} ~Join~ Differences[#] - 1 ~Join~ {n + k - 1 - Last[#]} &) /@ cuts
];

betaVectors[deg_Integer?NonNegative] :=
  betaVectors[deg] = weakCompositions[deg, dimBeta[]];

aTilde[n_List] := aTilde[n] = Module[
  {dim = dimBeta[], triple = $Model["TripleIntersection"], r = rVector[n]},
  Table[
    Sum[1/2 triple[[i, j, k]] r[[j]] r[[k]], {j, dim}, {k, dim}] +
      $Model["LinearB"][[i]] - $Model["LinearBPrime"][[i]],
    {i, dim}
  ]
];

bTildeExp[n_List] := bTildeExp[n] = Module[
  {dim = dimBeta[], triple = $Model["TripleIntersection"], r = rVector[n]},
  Sum[1/6 triple[[i, j, k]] r[[i]] r[[j]] r[[k]], {i, dim}, {j, dim}, {k, dim}] -
    Sum[r[[i]] ($Model["LinearB"][[i]] + $Model["LinearBPrime"][[i]]), {i, dim}]
];

chi[j_, q_] := Sum[q^(2 m), {m, -j, j}];

chiTilde[v_, q_] := Which[
  v == 0, 0,
  v > 0, chi[(v - 1)/2, q],
  v < 0, -chi[(-v - 1)/2, q]
];

tildedSum[expr_, {l_, m_, n_}] := Which[
  n > m, Sum[expr, {l, m, n - 1}],
  n == m, 0,
  n < m, -Sum[expr, {l, n, m - 1}]
];

fTildeStd[jL_, jR_, s_] := fTildeStd[jL, jR, s] = Expand[
  Sum[
    tildedSum[chiTilde[2 m1 + 2 m2 - u, ql] qr^u,
      {u, 2 m2, s - m1 + m2 + 1/2}],
    {m1, -jL, jL}, {m2, -jR, jR}
  ]
];

fTilde[jL_, jR_, s_, qL_, qR_] := fTildeStd[jL, jR, s] /. {ql -> qL, qr -> qR};

maxBasis[jL_, jR_, s_] := Which[
  jL == 0 && ((jR == 0 && (s == 1/2 || s == -1/2)) || (jR == 1/2 && s == 0)),
    0,
  s + jL - jR + 1/2 > 0 && (jL > 0 || s > jR + 1/2),
    {{s + jL + jR - 1/2, 3 jL - jR + s - 1/2}, -1},
  jL == 0 && jR > 0 && s == jR + 1/2,
    {{2 jR - 1, 1}, -1},
  jL == 0 && jR >= 1 && s == jR - 1/2,
    {{2 jR - 3, 1}, -1},
  s + jL - jR + 1/2 < 0 || (s + jL - jR + 1/2 == 0 && jL > 0),
    {{2 jR - 1, 2 jL + 1}, -1},
  True, 0
];

minBasis[jL_, jR_, s_] := Module[{bval},
  bval = maxBasis[jL, jR, -s];
  If[bval === 0, 0, {{-bval[[1, 1]], bval[[1, 2]]}, bval[[2]]}]
];

inverseMaxBasis0[u_, v_, s_] := Which[
  s > 0 || !IntegerQ[(u + v + 2 s + 1)/2] ||
    (s == -1/2 && u == -1 && v == 1),
    0,
  v >= u - 2 s + 1 && v <= 3 u - 2 s + 1,
    {{(u + v - 2 s + 1)/4, (3 u - v - 2 s + 1)/4}, -1},
  u >= -1 && v >= 1 && v <= u - 2 s + 1,
    {{(v - 1)/2, (u + 1)/2}, -1},
  True, 0
];

inverseMaxBasis1[u_, v_, s_] := Which[
  s <= -1/2 && u == 2 s && v == -4 s + 1, {{1/2}, 1},
  s == 0 && u == 0 && IntegerQ[(v + 1)/2] && (v + 1)/4 >= 1,
    {{(v + 1)/4}, 2},
  s < 0 && u == 0 && IntegerQ[(v + 2 s + 1)/2] && (v + 2 s + 1)/4 >= 1,
    {{(v + 2 s + 1)/4}, 1},
  True, 0
];

indicator[p_] := If[p, 1, 0];

modPick[c_, p0_, p1_, p2_] := Which[
  Mod[c, 3] == 0, p0,
  Mod[c, 3] == 1, p1,
  Mod[c, 3] == 2, p2
];

ffTildeStd[jR4_, s_] := ffTildeStd[jR4, s] = Expand[
  Sum[chiTilde[-4 s + 4 jR4 - 1 + u, ql] qr^(2 s + u), {u, -2 jR4 + 1, 0}] -
  chiTilde[-4 s + 2 jR4 - 2, ql] qr^(2 s - 2 jR4 + 1) +
  Sum[
    -Sum[chiTilde[-4 s + 4 jR4 - 6 o + 5 + u, ql] qr^(2 s + u),
      {u, -2 jR4 + 3 o - 1, -2}] -
    Sum[chiTilde[-4 s + 4 jR4 - 6 o + 1 + u, ql] qr^(2 s + u),
      {u, -2 jR4 + 3 o, -1}] -
    indicator[2 jR4 - 3 o - 2 s >= 2] *
      Sum[chiTilde[-4 s + 4 jR4 - 6 o - 3 + u, ql] qr^(2 s + u),
        {u, -2 jR4 + 3 o + 1, 0}] -
    Sum[chiTilde[-2 s + 4 jR4 - 6 o + 1 + u, ql] qr^u,
      {u, -2 jR4 + 3 o, 0}] -
    Sum[chiTilde[-2 s + 4 jR4 - 6 o - 1 + u, ql] qr^u,
      {u, -2 jR4 + 3 o, -1}] -
    Sum[chiTilde[-2 s + 4 jR4 - 6 o + 3 + u, ql] qr^u,
      {u, -2 jR4 + 3 o, 0}],
    {o, 1, (2/3) jR4}
  ] +
  indicator[jR4 >= 1] chiTilde[-2 s + 4 jR4 - 1, ql] +
  Sum[
    chiTilde[-2 s + 4 jR4 + 2 k - 5 - u, ql] qr^u,
    {k, -2 jR4 + 4, 0}, {u, -1 + k, modPick[-k, 0, 0, -1]}
  ] +
  indicator[jR4 >= 1] chiTilde[-2 s + 2 jR4 - 2, ql] qr^(-2 jR4 + 1) +
  indicator[jR4 >= 3/2] (
    chiTilde[-2 s + 2 jR4 - 1, ql] qr^(-2 jR4 + 2) +
    chiTilde[-2 s + 2 jR4 - 2, ql] qr^(-2 jR4 + 3)
  ) +
  Sum[
    tildedSum[chiTilde[-2 s + 4 jR4 + 2 k - 3 - u, ql] qr^u,
      {u, 2 s + 1 + k, 2 s + modPick[-k, -1, 0, 1]}],
    {k, -2 jR4 + 3, 0}
  ] +
  indicator[jR4 >= 1] *
    tildedSum[chiTilde[-2 s + 1 - u, ql] qr^u,
      {u, 2 s - 2 jR4 + 3, -2 jR4 + 1}] +
  indicator[jR4 >= 3/2] *
    modPick[
      2 jR4 - 3 Floor[(2/3) jR4],
      -tildedSum[chiTilde[-2 s + 1 - u, ql] qr^u, {u, 2 s, 0}] -
        indicator[s <= -1] *
          tildedSum[chiTilde[-2 s - 1 - u, ql] qr^u, {u, 2 s + 1, 0}],
      chiTilde[-2 s, ql]/qr -
        tildedSum[chiTilde[-2 s + 1 - u, ql] qr^u, {u, 2 s + 1, -1}],
      0
    ] +
  Sum[
    chiTilde[-2 s + 2 k - 1 - u, ql] qr^u,
    {o, 1, (2/3) jR4}, {k, 0, Min[2 jR4 - 3 o, 1]},
    {u, -2 jR4 + 3 o, Min[2 jR4 - 3 o - 3, -2 jR4 + 3 o + 2 k + 1]}
  ]
];

ffTilde[jR4_, s_, qL_, qR_] := ffTildeStd[jR4, s] /. {ql -> qL, qr -> qR};

SetAttributes[shiftInvariant, HoldFirst];
shiftInvariant[invariants_, jL_, jR_, shift_] := If[jL >= 0 && jR >= 0,
  Module[{key = {jL, jR}, value},
    value = Lookup[invariants, Key[key], 0] + shift;
    If[value == 0,
      KeyDropFrom[invariants, Key[key]],
      AssociateTo[invariants, key -> value]
    ]
  ]
];

KfunctionCore[k_Integer?Positive, beta_List, nList_List] :=
  KfunctionCore[k, beta, nList] = Expand[
    Sum[
      (-1)^(2 Total[bps[[1]]] (1 + k) + k) bps[[2]]
        (fTildeStd[bps[[1, 1]], bps[[1, 2]], beta.rVector[nList]]/k) /.
        {ql -> ql^k, qr -> qr^k},
      {bps, BPSList[beta]}
    ]
  ];

Kfunction[k_, beta_, nList_, qL_, qR_] := KfunctionCore[k, beta, nList] /. {ql -> qL, qr -> qR};

CoefficientLaurent[expr_, vars_List, exps_List] := Module[{subbed, coeff, dim = Length[vars]},
  subbed = Expand[expr Product[vars[[i]]^(1 - exps[[i]]), {i, dim}]];
  coeff = Coefficient[subbed, Times @@ vars];
  Simplify[coeff]
];

VfunctionCore[beta_List, nList_List, complete_] :=
  VfunctionCore[beta, nList, complete] = Which[
    beta == zeroBeta[], 1,
    TrueQ[complete], VfunctionCore[beta, nList, False] + KfunctionCore[1, beta, nList],
    True,
      Module[{l = 0, k, sum = 0, betaPP, betaP, betaPIterators, dim = dimBeta[]},
        Do[If[beta[[i]] >= 1, l = i; Break[]], {i, dim}];
        If[l == 0, Message[VfunctionCore::badbeta, beta]; Return[0]];
        betaP = Table[betaPP[i], {i, dim}];
        betaPIterators = Table[{betaPP[i], 0, beta[[i]]}, {i, dim}];
        Do[
          If[betaP[[l]] < 1 || betaP == beta, Continue[]];
          For[k = 1, allNoMoreThan[k betaP, beta], k++,
            sum += k (betaP[[l]]/beta[[l]]) *
              VfunctionCore[beta - k betaP, nList, True] *
              KfunctionCore[k, betaP, nList]
          ],
          Evaluate[Sequence @@ betaPIterators]
        ];
        Expand[sum]
      ]
  ];

Vfunction[beta_, nList_, complete_, qL_, qR_] :=
  VfunctionCore[beta, nList, complete] /. {ql -> qL, qr -> qR};

solveFluxRules[bound_] := Module[{vars = Array[nv, dimFlux[]]},
  Solve[Total[aTilde[vars]] <= bound, vars, Integers]
];

Ifunction[beta_List, qL_, qR_] := Ifunction[beta, qL, qR] = Module[
  {sum = 0, nLists, nnn, vars = Array[nv, dimFlux[]], beta0},
  nLists = Solve[Total[aTilde[vars]] <= Total[beta], vars, Integers];
  Do[
    nnn = vars /. nList;
    beta0 = beta - aTilde[nnn];
    If[!allNoMoreThan[zeroBeta[], beta0], Continue[]];
    sum += (-1)^Total[nnn] *
      qR^(2 (bTildeExp[nnn] - bTildeExp[zeroFlux[]])) *
      Vfunction[beta0, nnn, aTilde[nnn] != zeroBeta[], qL, qR],
    {nList, nLists}
  ];
  Expand[sum]
];

LaurentPolynomialQ[expr_, vars_List] := Module[{tgt, num, den},
  tgt = Together[expr];
  num = Numerator[tgt];
  den = Denominator[tgt];
  PolynomialQ[num, vars] && PolynomialQ[den, vars] &&
    Length[MonomialList[den, vars]] == 1
];

polyToSorted[expr_, qL_, qR_] := Module[{minL, minR, shifted, rules, list},
  If[expr === 0, Return[{}]];
  minL = Exponent[expr, qL, Min];
  minR = Exponent[expr, qR, Min];
  shifted = Expand[expr/(qL^minL qR^minR)];
  rules = CoefficientRules[shifted, {qR, qL}];
  list = ({#[[1, 2]] + minL, #[[1, 1]] + minR, #[[2]]} &) /@ rules;
  SortBy[list, {-#[[2]], -#[[1]]} &]
];

sortedGetMaxBasis[sorted_List] := Module[{nL, nR, coeff},
  If[Length[sorted] == 0, Return[{{}, 0}]];
  {nL, nR, coeff} = First[sorted];
  If[nL < 0, Return[0]];
  {{nR, nL + 1}, coeff}
];

fTildeSorted[jL_, jR_, s_] :=
  fTildeSorted[jL, jR, s] = polyToSorted[fTilde[jL, jR, s, qL, qR], qL, qR];

ffTildeSorted[jR4_, s_] :=
  ffTildeSorted[jR4, s] = polyToSorted[ffTilde[jR4, s, qL, qR], qL, qR];

sortedSubtractScaled[aList_List, bList_List, c_] := Module[{assoc = <||>, add, out},
  add[{nL_, nR_, coeff_}] := Module[{key = {nL, nR}},
    If[coeff =!= 0,
      AssociateTo[assoc, key -> Lookup[assoc, Key[key], 0] + coeff]
    ]
  ];
  Scan[add, aList];
  Scan[add[{#[[1]], #[[2]], -c #[[3]]}] &, bList];
  out = KeyValueMap[{#[[1]], #[[2]], #2} &, Select[assoc, # =!= 0 &]];
  SortBy[out, {-#[[2]], -#[[1]]} &]
];

BPSList::nomodel = "Install a model first with SetBlowupModel[...].";
BPSList[beta_List] := Module[{},
  If[!AssociationQ[$Model], Message[BPSList::nomodel]; Return[$Failed]];
  If[Length[beta] =!= dimBeta[] || Total[beta] <= 0 || !allNoMoreThan[zeroBeta[], beta],
    Return[{}]
  ];
  If[KeyExistsQ[$BPSInvariants, beta],
    KeyValueMap[List, Lookup[$BPSInvariants, Key[beta]]],
    SolveBPS[beta];
    KeyValueMap[List, Lookup[$BPSInvariants, Key[beta]]]
  ]
];

SolveBPS::badmax = "Cannot invert highest basis for beta `1`, term `2`.";
SolveBPS[beta_List] := Module[
  {invariants = <||>, II, sorted, s, maxbasis, umax, vmax, coefmax,
    inverse, jL, jR, coef, jR4, o, oMax, number = 0,
    numberMax, fSorted, ratio},
  If[beta.rVector[zeroFlux[]] <= 0,
    II = Ifunction[beta, qL, qR];
    s = beta.rVector[zeroFlux[]],
    II = Ifunction[beta, qL, qR] /. {qL -> qL^-1, qR -> qR^-1};
    s = -beta.rVector[zeroFlux[]]
  ];
  sorted = polyToSorted[II, qL, qR];
  numberMax = 100 + 3 Total[beta]^4;
  While[Length[sorted] > 0 && number <= numberMax,
    number++;
    maxbasis = sortedGetMaxBasis[sorted];
    If[maxbasis === 0, Message[SolveBPS::badmax, beta, First[sorted]]; Abort[]];
    {{umax, vmax}, coefmax} = maxbasis;
    inverse = inverseMaxBasis0[umax, vmax, s];
    If[inverse === 0,
      inverse = inverseMaxBasis1[umax, vmax, s];
      If[inverse === 0, Message[SolveBPS::badmax, beta, {umax, vmax, s}]; Abort[]];
      {{jR4}, coef} = inverse;
      If[coef == 0, Break[]];
      ratio = coefmax/coef;
      fSorted = ffTildeSorted[jR4, s];
      sorted = sortedSubtractScaled[sorted, fSorted, ratio];
      shiftInvariant[invariants, jR4 - s - 1/2, jR4, ratio];
      shiftInvariant[invariants, jR4 - s, jR4 - 1/2, -ratio];
      oMax = Floor[(2 jR4)/3] + 1;
      Do[
        shiftInvariant[invariants, jR4 - s - (3/2) o, jR4 - (3/2) o + 3/2, -ratio];
        shiftInvariant[invariants, jR4 - s - (3/2) o + 3/2, jR4 - (3/2) o, ratio];
        shiftInvariant[invariants, jR4 - s - (3/2) o - 1/2, jR4 - (3/2) o + 1, ratio];
        shiftInvariant[invariants, jR4 - s - (3/2) o + 1, jR4 - (3/2) o - 1/2, -ratio],
        {o, 1, oMax}
      ],
      {{jL, jR}, coef} = inverse;
      ratio = coefmax/coef;
      fSorted = fTildeSorted[jL, jR, s];
      sorted = sortedSubtractScaled[sorted, fSorted, ratio];
      shiftInvariant[invariants, jL, jR, ratio]
    ];
  ];
  AssociateTo[$BPSInvariants, beta -> KeySort[invariants]];
];

SolveBPSDegree[deg_Integer?NonNegative] := Association[
  Table[beta -> BPSList[beta], {beta, betaVectors[deg]}]
];

WilsonLoopBPSList[beta_List, r_List] := Module[{},
  If[!AssociationQ[$Model], Message[BPSList::nomodel]; Return[$Failed]];
  If[Length[beta] =!= dimBeta[] || Length[r] =!= dimBeta[] ||
      !allNoMoreThan[zeroBeta[], beta] || !allNoMoreThan[zeroBeta[], r] || Total[r] <= 0,
    Return[{}]
  ];
  Which[
    KeyExistsQ[$WilsonLoopBPSInvariants, {beta, r}],
      KeyValueMap[List, Lookup[$WilsonLoopBPSInvariants, Key[{beta, r}]]],
    allNoMoreThan[beta, r],
      {},
    True,
      SolveWilsonLoop[beta, r];
      KeyValueMap[List, Lookup[$WilsonLoopBPSInvariants, Key[{beta, r}]]]
  ]
];

KTildeFunctionCore[beta_List, r_List] :=
  KTildeFunctionCore[beta, r] = Expand[
    Sum[
      (-1)^(2 Total[bps[[1]]] + (beta - r).$Model["BField"]) *
        bps[[2]] * chi[bps[[1, 1]], q1/q2] * chi[bps[[1, 2]], q1 q2] *
        ((q1 - q1^-1) (q2 - q2^-1))^(Total[r] - 1),
      {bps, WilsonLoopBPSList[beta, r]}
    ]
  ];

KTildeFunction[beta_, r_, qq1_, qq2_] := KTildeFunctionCore[beta, r] /. {q1 -> qq1, q2 -> qq2};

WTildeFunction0[r_List] := WTildeFunction0[r] = Module[
  {initial, expCoeff, qCoeff, rP, rPP, rPIterators, vars = qVars[], dim = dimBeta[]},
  rP = Table[rPP[i], {i, dim}];
  rPIterators = Table[{rPP[i], 0, r[[i]]}, {i, dim}];
  initial = Sum[
    If[rP != zeroBeta[], Frr[rP] Product[(vars[[i]] h)^rP[[i]]/rP[[i]]!, {i, dim}], 0],
    Evaluate[Sequence @@ rPIterators]
  ];
  expCoeff = SeriesCoefficient[Exp[initial], {h, 0, Total[r]}];
  qCoeff = CoefficientLaurent[expCoeff, vars, r];
  Expand[qCoeff Product[r[[i]]!, {i, dim}]]
];

WTildeFunction1[beta_List, r_List, complete_] :=
  WTildeFunction1[beta, r, complete] = Which[
    r == zeroBeta[], If[beta == zeroBeta[], 1, 0],
    TrueQ[complete], WTildeFunction1[beta, r, False] + KTildeFunctionCore[beta, r],
    True,
      Module[{l = 0, betaPP, betaP, betaPIterators, rP, rPP, rPIterators, dim = dimBeta[]},
        Do[If[r[[i]] >= 1, l = i; Break[]], {i, dim}];
        If[l == 0, Message[WTildeFunction1::badr, r]; Return[0]];
        betaP = Table[betaPP[i], {i, dim}];
        betaPIterators = Table[{betaPP[i], 0, beta[[i]]}, {i, dim}];
        rP = Table[rPP[i], {i, dim}];
        rPIterators = Table[{rPP[i], If[i == l, 1, 0], r[[i]]}, {i, dim}];
        Expand[
          Sum[
            If[rP == r, 0,
              Product[Binomial[r[[i]] - If[i == l, 1, 0], rP[[i]] - If[i == l, 1, 0]], {i, dim}] *
                WTildeFunction1[betaP, r - rP, True] *
                KTildeFunctionCore[beta - betaP, rP]
            ],
            Evaluate[Sequence @@ rPIterators],
            Evaluate[Sequence @@ betaPIterators]
          ]
        ]
      ]
  ];

WTildeFunction2[beta_, r_, complete_, qq1_, qq2_] :=
  WTildeFunction1[beta, r, complete] /. {q1 -> qq1, q2 -> qq2};

KKCore[nnn_List, beta_List] := KKCore[nnn, beta] = Module[
  {ff = 0, k, expCoeff, qCoeff, betaP, betaPP, betaPIterators,
    vars = qVars[], dim = dimBeta[]},
  betaP = Table[betaPP[i], {i, dim}];
  betaPIterators = Table[{betaPP[i], 0, beta[[i]]}, {i, dim}];
  Do[
    If[betaP != zeroBeta[],
      For[k = 1, allNoMoreThan[k betaP, beta], k++,
        ff += KfunctionCore[k, betaP, nnn] Product[(vars[[i]] h)^(k betaP[[i]]), {i, dim}]
      ]
    ],
    Evaluate[Sequence @@ betaPIterators]
  ];
  expCoeff = SeriesCoefficient[Exp[ff], {h, 0, Total[beta]}];
  qCoeff = CoefficientLaurent[expCoeff, vars, beta];
  Expand[qCoeff qr^(2 (bTildeExp[nnn] - bTildeExp[zeroFlux[]])) /. {ql -> q1/q2, qr -> q1 q2}]
];

KK[nnn_, beta_, qq1_, qq2_] := KKCore[nnn, beta] /. {q1 -> qq1, q2 -> qq2};

LHSCore[beta_List, r_List, complete_] := LHSCore[beta, r, complete] = Module[
  {sum = 0, nLists, nnn, beta0, factor, betaP, betaPP, betaPIterators,
    wtilde, kk, vars = Array[nv, dimFlux[]], dim = dimBeta[]},
  betaP = Table[betaPP[i], {i, dim}];
  nLists = Solve[Total[aTilde[vars]] <= Total[beta], vars, Integers];
  Do[
    nnn = vars /. nList;
    beta0 = beta - aTilde[nnn];
    If[!allNoMoreThan[zeroBeta[], beta0], Continue[]];
    betaPIterators = Table[{betaPP[i], 0, beta0[[i]]}, {i, dim}];
    factor = (-1)^Total[nnn]/q1^(2 r.(rVector[nnn] - rVector[zeroFlux[]]));
    Do[
      wtilde = WTildeFunction1[betaP, r, complete || betaP != beta] /. {q2 -> q2/q1};
      kk = KKCore[nnn, beta0 - betaP];
      sum += factor q1^(2 betaP.rVector[nnn]) wtilde kk,
      Evaluate[Sequence @@ betaPIterators]
    ],
    {nList, nLists}
  ];
  Expand[sum]
];

LHS[beta_, r_, complete_, qq1_, qq2_] := LHSCore[beta, r, complete] /. {q1 -> qq1, q2 -> qq2};

SolveLambda::notpoly = "Lambda[`1`,`2`] = `3` is not a Laurent polynomial.";
SolveLambda[r_List] := Module[{lambdas = <||>, rPPs, temp, numerator, denominator},
  rPPs = SortBy[Tuples[Table[Range[0, r[[i]]], {i, dimBeta[]}]], -Total[#] &];
  Do[
    denominator = WTildeFunction1[zeroBeta[], rPP, True];
    numerator = LHSCore[r - rPP, r, True] -
      Sum[
        If[allNoMoreThan[rPP, lam[[1]]],
          lam[[2]] WTildeFunction1[lam[[1]] - rPP, lam[[1]], True],
          0
        ],
        {lam, KeyValueMap[List, lambdas]}
      ];
    temp = Expand[Cancel[numerator/denominator]];
    If[!LaurentPolynomialQ[temp, {q1, q2}],
      Message[SolveLambda::notpoly, r, rPP, temp];
      Abort[]
    ];
    If[temp =!= 0, AssociateTo[lambdas, rPP -> temp]],
    {rPP, rPPs}
  ];
  AssociateTo[$LambdaRRPrime, r -> KeySort[lambdas]];
];

LFunctionCore[beta_List, r_List] := LFunctionCore[beta, r] = Module[{lhs, rhs},
  If[!KeyExistsQ[$LambdaRRPrime, r], SolveLambda[r]];
  lhs = LHSCore[beta, r, False];
  rhs = Sum[
    lam[[2]] WTildeFunction1[lam[[1]] + beta - r, lam[[1]], lam[[1]] != r],
    {lam, KeyValueMap[List, Lookup[$LambdaRRPrime, Key[r]]]}
  ];
  lhs - rhs
];

LambdaRR[r_List] := LambdaRR[r] = Module[{},
  If[!KeyExistsQ[$LambdaRRPrime, r], SolveLambda[r]];
  Lookup[Lookup[$LambdaRRPrime, Key[r]], Key[r], 0]
];

rrt[r_List] := (Total[r] - 1)/2;

hStd[jL_, jR_, s_, r_List] := hStd[jL, jR, s, r] = Module[{rt = rrt[r]},
  Sum[
    (-1)^(rt - m0) Binomial[2 rt, rt - m0] q2^(2 (m0 + m1 + m2)) *
      (LambdaRR[r] q1^(2 (-m1 + m2)) - q1^(2 (s - m0 - 2 m1))),
    {m0, -rt, rt}, {m1, -jL, jL}, {m2, -jR, jR}
  ]
];

h[jL_, jR_, s_, r_, qq1_, qq2_] := hStd[jL, jR, s, r] /. {q1 -> qq1, q2 -> qq2};

inverseMaxBasisOfh[n1_, n2_, type_, paras_List] := Module[
  {jL, jR, s, rt, l1max, l2max, cmax},
  {s, rt, l1max, l2max, cmax} = paras;
  If[type == 0, Return[{(s - rt - n1/2)/2, n2/2 - (s + rt - n1/2)/2, -1}]];
  jL = ((n2 - n1 - l2max + l1max)/2 - rt)/2;
  jR = ((n2 + n1 - l2max - l1max)/2 - rt)/2;
  If[{l1max, l2max, s, rt, jL, jR} == {0, 0, 0, 0, 0, 0},
    {0, 0, cmax - 1},
    {jL, jR, cmax}
  ]
];

SolveWilsonLoop::badmax = "Cannot invert Wilson-loop highest basis for beta `1`, r `2`, term `3`.";
SolveWilsonLoop[beta_List, r_List] := Module[
  {invariants = <||>, lll, sorted, s, rt, lambdaSorted, l1max, l2max, cmax,
    useMaxBasisOrNot, n1max, n2max, coefmax, jL, jR, coef,
    number = 0, numberMax, hSorted, ratio},
  lambdaSorted = polyToSorted[LambdaRR[r], q1, q2];
  lll = Expand[Cancel[LFunctionCore[beta, r]/(q1 - q1^-1)^(Total[r] - 1)]];
  If[!LaurentPolynomialQ[lll, {q1, q2}],
    Message[SolveWilsonLoop::badmax, beta, r, lll];
    Abort[]
  ];
  s = beta.rVector[zeroFlux[]];
  rt = rrt[r];
  useMaxBasisOrNot = If[Length[lambdaSorted] == 0,
    {l1max, l2max, cmax} = {0, 0, 0}; 0,
    {l1max, l2max, cmax} = First[lambdaSorted];
    If[l2max > 0 || s <= 0, 1, 2]
  ];
  If[useMaxBasisOrNot == 2,
    s = -s;
    lll = (-1)^(Total[r] - 1) lll /. {q1 -> q1^-1, q2 -> q2^-1}
  ];
  sorted = polyToSorted[lll, q1, q2];
  numberMax = 100 + 3 (Total[beta] + Total[r])^4;
  While[Length[sorted] > 0 && number <= numberMax,
    number++;
    {n1max, n2max, coefmax} = First[sorted];
    {jL, jR, coef} = inverseMaxBasisOfh[n1max, n2max, useMaxBasisOrNot,
      {s, rt, l1max, l2max, cmax}];
    If[jL < 0 || jR < 0 || coef == 0,
      Message[SolveWilsonLoop::badmax, beta, r, {n1max, n2max, coefmax}];
      Abort[]
    ];
    ratio = coefmax/coef;
    hSorted = polyToSorted[hStd[jL, jR, s, r], q1, q2];
    sorted = sortedSubtractScaled[sorted, hSorted, ratio];
    AssociateTo[
      invariants,
      {jL, jR} -> ratio * (-1)^(2 jL + 2 jR + (beta - r).$Model["BField"])
    ];
  ];
  AssociateTo[$WilsonLoopBPSInvariants, {beta, r} -> KeySort[invariants]];
];

SolveWilsonLoopBox[maxBeta_Integer?NonNegative, maxR_Integer?Positive] /; dimBeta[] == 1 :=
  Association[
    Flatten[
      Table[{{b}, {rr}} -> WilsonLoopBPSList[{b}, {rr}], {rr, 1, maxR}, {b, 0, maxBeta}],
      1
    ]
  ];

SaveState[file_String] := Save[file, {$BPSInvariants, $WilsonLoopBPSInvariants, $LambdaRRPrime}];
LoadState[file_String] := Get[file];

End[];

EndPackage[];

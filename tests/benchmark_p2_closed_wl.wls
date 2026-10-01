Get[FileNameJoin[{DirectoryName[$InputFileName], "..", "src", "BlowupBPS.wl"}]];

BlowupBPS`SetBlowupModel[BlowupBPS`LocalP2Model[]];

total = 0.;
Do[
  timing = AbsoluteTiming[BlowupBPS`BPSList[{d}]][[1]];
  total += timing;
  Print[
    "p2_closed_wolfram_degree=", d,
    " seconds=", timing,
    " cumulative=", total
  ],
  {d, 1, 30}
];

Print["p2_closed_wolfram_degree=30_total seconds=", total];

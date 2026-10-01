Get[FileNameJoin[{DirectoryName[$InputFileName], "..", "src", "BlowupBPS.wl"}]];

BlowupBPS`SetBlowupModel[BlowupBPS`LocalP2Model[]];

checks = {
  "BPS[{1}]" -> (BlowupBPS`BPSList[{1}] === {}),
  "BPS[{2}]" -> (BlowupBPS`BPSList[{2}] === {}),
  "BPS[{3}]" -> (BlowupBPS`BPSList[{3}] === {{{0, 1}, 1}}),
  "BPS[{6}]" -> (BlowupBPS`BPSList[{6}] === {{{0, 5/2}, 1}}),
  "BPS[{9}]" -> (BlowupBPS`BPSList[{9}] === {{{0, 3}, 1}, {{1/2, 9/2}, 1}}),
  "W[{0},{1}]" -> (BlowupBPS`WilsonLoopBPSList[{0}, {1}] === {{{0, 0}, 1}}),
  "W[{3},{1}]" -> (BlowupBPS`WilsonLoopBPSList[{3}, {1}] === {{{0, 1/2}, 1}}),
  "W[{6},{1}]" -> (BlowupBPS`WilsonLoopBPSList[{6}, {1}] === {{{0, 2}, 1}})
};

failed = Select[checks, Last[#] =!= True &];

If[failed === {},
  Print["local P2 smoke tests passed"],
  Print["local P2 smoke tests failed: ", failed];
  Exit[1]
];

Get[FileNameJoin[{DirectoryName[$InputFileName], "..", "src", "BlowupBPS.wl"}]];

BlowupBPS`SetBlowupModel[BlowupBPS`LocalP2Model[]];

Print["Closed refined BPS invariants"];
Print["BPS[{3}]  = ", BlowupBPS`BPSList[{3}]];
Print["BPS[{6}]  = ", BlowupBPS`BPSList[{6}]];
Print["BPS[{9}]  = ", BlowupBPS`BPSList[{9}]];

Print["Wilson-loop refined BPS invariants"];
Print["W[{0},{1}] = ", BlowupBPS`WilsonLoopBPSList[{0}, {1}]];
Print["W[{3},{1}] = ", BlowupBPS`WilsonLoopBPSList[{3}, {1}]];
Print["W[{6},{1}] = ", BlowupBPS`WilsonLoopBPSList[{6}, {1}]];
